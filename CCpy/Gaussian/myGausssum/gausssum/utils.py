#
# GaussSum (http://gausssum.sf.net)
# Copyright (C) 2006-2013 Noel O'Boyle <baoilleach@gmail.com>
#
# This program is free software; you can redistribute and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

#from tkinter import *
import sys
#sys.path.append('/usr/local/lib/python3.4')
#import ConfigParser     # For writing the settings to an .ini file

from CCpy.Gaussian.myGausssum.gausssum.cclib.parser.utils import PeriodicTable

import numpy

import os

import math


# This class allows Find.py, etc. to write directly to the 'console screen'
# in GaussSum.py.
# It also allows Find.py, etc. to write to stdout when testing.

class Redirect:
    def __init__(self,write_function,root,other):
        self.outputfn=write_function
        self.args=other
        self.root=root
    def write(self,text):
        if self.args=="None":
            self.outputfn(text)
        else:
            self.outputfn(self.args[0],text)
            self.root.update() # so it shows it immediately
            self.args[1].see(END) # scroll if necessary to see last text inserted
    def flush(self):
        pass

def readinconfigfile(inifile):
    # Taken from the Python Cookbook (O'Reilly):
    # reads in configuration information from a
    # Windows-style .ini file, and returns a
    # dictionary of settings.
    #
    # Uses the Python module configparser for the complicated stuff.
    cp = configparser.ConfigParser(interpolation=None)
    cp.read(inifile)
    config={}
    for sec in cp.sections():
        name = sec.lower()
        for opt in cp.options(sec):
            config[name+"."+opt.lower()] = cp.get(sec,opt).strip()

    return config

def writeoutconfigfile(config,inifile):
    # The companion to readinconfigile
    # Written by me!
    cp = configparser.ConfigParser(interpolation=None)

    for key,value in config.items():
        sec=key.split('.')[0]
        opt=key.split('.')[1]
        if not cp.has_section(sec):
            cp.add_section(sec)
        cp.set(sec,opt,value)

    out=open(inifile,"w")
    cp.write(out)
    out.close()

class NMRstandards(object):
    def __init__(self,location):
        """
        Read in the nmr standards file (if it exists)
        Returns a list of dictionaries:
            nmrdata=[dict_1,dict_2,dict_3...]
            dict_n=['theory':'B3LYP/6-31G(d)','name':'TMS','C':12.23,'H':123]
        """
        self.location = location
        self.nmrdata = []

        # Check for nmr standards file in location described in settings
        try:
            readin = open(self.location,"r")
        except IOError:
            # File does not exist - i.e. no settings yet
            pass
        else:
            for line in readin:
                if not line.lstrip().startswith("#"): # Ignore comments
                    temp=line.split("\t")
                    adict={}
                    adict['theory']=temp[0]
                    adict['name']=temp[1]
                    for i in range(2,len(temp),2): # Read in the calculated shifts
                        adict[temp[i]]=float(temp[i+1])
                    self.nmrdata.append(adict)
            readin.close()
    def __getitem__(self,index):
        """Shortcut to access the data directly."""
        return self.nmrdata[index]
    def save(self):
        """Write the NMR standards to disk."""
        outputfile = open(self.location,"w")
        lines = []
        for adict in self.nmrdata:
            output = [adict['theory'],adict['name']]
            for k,v in adict.items():
                if k not in ["theory","name"]:
                    output.append(k)
                    output.append(str(v))
            lines.append("\t".join(output))
        outputfile.write("\n".join(lines))
        outputfile.close()

class ErrorCatcher:
    def __init__(self):
        self.log=""
        self.longlog=""
    def write(self,text):
        self.log=self.log+text
    def clear(self):
        self.longlog=self.longlog+self.log
        self.log=""

def percent(number):
    return str(int(round(number*100))) # round leaves .0 at the end of a number

def levelname(i,HOMO):
    if i<HOMO:
        level='H-'+str(HOMO-i)
    elif i>HOMO+1:
        level='L+'+str(i-HOMO-1)
    elif i==HOMO+1:
        level="LUMO"
    else:
        level="HOMO"
    return level

def lorentzian(x,peak,height,width):
    """The lorentzian curve.

    f(x) = a/(1+a)

    where a is FWHM**2/4
    """
    a = width**2./4.
    return float(height)*a/( (peak-x)**2 + a )

class Spectrum(object):
    """Convolutes and stores spectrum data.

    Usage:
     Spectrum(start,end,numpts,peaks,width,formula)

    where
     peaks is [(pos,height),...]
     formula is a function such as gaussianpeak or delta
    

    >>> t = Spectrum(0,50,11,[[(10,1),(30,0.9),(35,1)]],5,delta)
    >>> t.spectrum
    array([[ 0.        ],
           [ 1.        ],
           [ 1.        ],
           [ 1.        ],
           [ 0.        ],
           [ 0.89999998],
           [ 1.89999998],
           [ 1.89999998],
           [ 1.        ],
           [ 0.        ],
           [ 0.        ]],'d')
    """
    def __init__(self,start,end,numpts,peaks,width,formula):
        self.start = start
        self.end = end
        self.numpts = numpts
        self.peaks = peaks
        self.width = width
        self.formula = formula

        # len(peaks) is the number of spectra in this object
        self.spectrum = numpy.zeros( (numpts,len(peaks)),"d")
        self.xvalues = numpy.arange(numpts)*float(end-start)/(numpts-1) + start
        for i in range(numpts):
            x = self.xvalues[i]
            for spectrumno in range(len(peaks)):
                for (pos,height) in peaks[spectrumno]:
                    self.spectrum[i,spectrumno] = self.spectrum[i,spectrumno] + formula(x,pos,height,width)

class GaussianSpectrum(object):
    """An optimised version of Spectrum for convoluting gaussian curves.

    Usage:
     GaussianSpectrum(start,end,numpts,peaks,width)
    where
     peaks -- ( [List of peaks],[ [list of heights],[list of heights],..] )
    """
    def __init__(self,start,end,numpts,peaks,width):
        self.start = start
        self.end = end
        self.numpts = numpts
        self.peaks = peaks[0]
        self.heights = peaks[1]
        self.width = width

        # make heights a local variable as it's accessed in the inner loop
        heights = self.heights

        # len(heights) is the number of spectra in this object
        data = []
        self.xvalues = numpy.arange(self.numpts)*float(self.end-self.start)/(self.numpts-1) + self.start
        A = -2.7726/self.width**2
        for x in self.xvalues:
            tot = [0]*len(self.heights) # The total for each spectrum for this x value

            for peakno in range(len(self.peaks)): # For each peak
                pos = self.peaks[peakno]
                exponent = math.exp(A*(pos-x)**2)
                for spectrumno in range(len(heights)):
                    tot[spectrumno] += heights[spectrumno][peakno]*exponent

            data.append(tot)

        self.spectrum = numpy.swapaxes(numpy.array(data),0,1)

class Groups(object):
    """Stores the groups to be used for a DOS or COOP calculation

    Essentially, the groups are a partitioning of the orbitals among
    atoms or a group of atoms.

    Usage:
     Groups(filename, atomnos, aonames, atombasis)
    where
     atomnos is a list of the atomic numbers of the atoms
     aonames is a list of the 'names' of each atomic orbital
     atombasis is a list for each atom of the indices of the basis fns on that atom

    Attributes:
     groups -- a dictionary of the partition of the orbitals among groups

    """
    def __init__(self, filename, atomnos, aonames, atombasis):
        self.atomnos = atomnos
        self.aonames = aonames
        self.atombasis = atombasis

        self._makeatomnames()
        if not self.aonames:
            self._makeaonames()
        self._readfile(filename)
        self._setup()

    def _readfile(self,filename):
        """Read group info from a file.

        File format:
         First line must be one of "orbitals","atoms","allorbitals","allatoms".
         The remainder of the file is ignored in the case of allatoms/allorbitals.
         Otherwise a series of groups must be described with a name by itself on one line
         and a list of group members on the following line.
         As an example:
             atoms
             Phenyl ring
             1,4-6,8
             The rest
             2,3,7
        """
        inputfile = open(filename,"r")
        grouptype = next(inputfile).strip()
        while not grouptype: # Ignore blank lines
            grouptype = next(inputfile).strip()
        groups = {}
        for line in inputfile:
            if not line.strip(): continue # Ignore blank lines
            groupname = line.strip()
            atoms = []
            line = next(inputfile)
            parts = line.split(",")
            for x in parts:
                temp = x.split("-")
                if len(temp)==1:
                    atoms.append(int(temp[0]))
                else:
                    atoms.extend(range(int(temp[0]),int(temp[1])+1))
            groups[groupname] = atoms
        inputfile.close()

        self.filegroups = groups
        self.grouptype = grouptype

    def _makeatomnames(self):
        """Create unique names for the atoms"""
        pt = PeriodicTable()
        d = {}
        names = []
        for atomno in self.atomnos:
            if atomno in d:
                d[atomno] += 1
            else:
                d[atomno] = 1
            names.append(pt.element[atomno] + str(d[atomno]))
        self.atomnames = names

    def _makeaonames(self):
        """Create unique names for the atomic basis if needed"""
        d = {}
        for atomname, ab in zip(self.atomnames, self.atombasis):
            names.extend([atomname + "_" + x for x in range(len(ab))])
        self.aonames = names

    def __str__(self):
        """Return a string representation of this object."""
        ans = []
        for k,v in self.groups.items():
            ans.append("%s: %s" % (k,",".join(map(str,v))))
        return "\n".join(ans)

    def _setup(self):
        """Create the groups attribute."""
        type = self.grouptype.lower()
        if not self.filegroups:
            if type=="allorbitals":
                self.groups = {}
                for i in range(len(self.aonames)):
                    orbital = self.aonames[i]
                    self.groups[orbital] = [i]
            elif type=="allatoms":
                self.groups = dict(zip(self.atomnames, self.atombasis))
            else:
                raise TypeError("You need to specify groups")
        else:
            if type=="orbitals":
                self.groups = {}
                for k,v in self.filegroups.items():
                    # Convert the indices to start from 0
                    self.groups[k] = [x-1 for x in v]
            elif type=="atoms":
                self.groups = {}
                for k,v in self.filegroups.items():
                    self.groups[k] = []
                    for x in v:
                        # Convert the indices to start from 0
                        self.groups[k].extend(self.atombasis[x - 1])
            else:
                raise TypeError("%s is not a valid type of group" % type)


    def verify(self,purpose):
        """Verify that the groups attribute is consistent.

        purpose is one of "DOS" or "COOP"
        """
        status = ""

        # Create one big list of all of the atom orbitals in the groups
        all = []
        for x in self.groups.values():
            all += x
        all.sort()

        if purpose=="DOS": # Each atom orb must appear exactly once
            ok = (all==list(range(len(self.aonames))))
            if not ok:
                status = "Problem with Groups.txt!\n(1)Every atom/orbital must " \
                          "be listed as a member of some group\n(2)No "   \
                          "atom/orbital can be listed twice"
        elif purpose=="COOP":
            # No atom can appear twice (and cannot have any crazy atoms either)
            badatom = [x for x in all if x not in range(len(self.aonames))]
            if badatom:
                status = "Atomic orb number out of range in Groups.txt\n"
            else:
                for i in range(len(all)-1):
                    if all[i]==all[i+1]:
                        status += "Atomic orb %d has been included twice\n" % all[i]

        return status

if __name__=="__main__":
    import doctest
    doctest.testmod(verbose=True)
