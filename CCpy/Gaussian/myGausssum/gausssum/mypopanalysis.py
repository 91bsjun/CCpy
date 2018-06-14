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

import string
import os
import sys
import numpy
#import numpy.oldnumeric as Numeric
from math import exp, log
from .plot import DisplayPlot
from .mympl3 import MPLPlot
from .folder import folder
from CCpy.Gaussian.myGausssum.gausssum.utils import levelname
from CCpy.Gaussian.myGausssum.gausssum.utils import GaussianSpectrum
from CCpy.Gaussian.myGausssum.gausssum.utils import Groups

def Popanalysis(logfile,logfilename,start,end,FWHM):

    def DOSconvolute(orb_MPA,evalue):
        """Convolute the DOS spectrum"""

        heights =[x for x in numpy.swapaxes(orb_MPA,0,1)]

        spectrum = GaussianSpectrum(start, end, 1000,
                                    (evalue, heights),
                                    FWHM)
        return spectrum

    def tidy(num): # Changes +-0.155648 into +-0.16
        if num>=0:
            rounded=int(num*100+.5)/100.
            return str(rounded)[:4]
        else:
            rounded=int(num*100-.5)/100.
            return str(rounded)[:5]

    def originoutput(MPA):
        """Write a file suitable for reading with Origin."""
        outputfile=open(os.path.join(gaussdir,"origin_orbs.txt"),"w")
        for i in range(len(evalue[0])):
            if numgroups>0 and pop:
                total=0
                for j in range(numgroups):
                    outputfile.write(str(total)+"\t"+str(evalue[0][i])+"\t")
                    total += MPA[0][i,j]
                outputfile.write("\n")
                total=0
                for j in range(numgroups):
                    total += MPA[0][i,j]
                    outputfile.write(str(total)+"\t"+str(evalue[0][i])+"\t")
                outputfile.write("\n")
            else:
                outputfile.write("0\t"+str(evalue[0][i])+"\n")
                outputfile.write("1\t"+str(evalue[0][i])+"\n")
        if unres:
            for i in range(len(evalue[1])):
                if numgroups>0 and pop:
                    total=0
                    for j in range(numgroups):
                        outputfile.write(str(total)+"\t"+str(evalue[1][i])+"\t")
                        total += MPA[1][i,j]
                    outputfile.write("\n")
                    total=0
                    for j in range(numgroups):
                        total += MPA[1][i,j]
                        outputfile.write(str(total)+"\t"+str(evalue[1][i])+"\t")
                    outputfile.write("\n")
                else:
                    outputfile.write("0\t"+str(evalue[1][i])+"\n")
                    outputfile.write("1\t"+str(evalue[1][i])+"\n")

        outputfile.close()

    def densityofstates():
        """Do the density of status calculation."""
        groupnames = list(groups.groups.keys()) if groups else []

        if groups and pop:
            contrib = [x * numpy.dot(x,overlap) for x in MOCoeff]
            if not unres:
                MPA = [numpy.zeros( (logfile.nmo,numgroups), "d")]
            else:
                MPA = [numpy.zeros( (logfile.nmo,numgroups), "d")
                       for x in range(2)]
            for i,groupname in enumerate(groupnames):
                for basisfn in groups.groups[groupname]:
                    MPA[0][:,i] += contrib[0][:,basisfn]
                    if unres:
                        MPA[1][:,i] += contrib[1][:,basisfn]

        else: # Set MPA to be all ones
            MPA = [numpy.ones( (logfile.nmo,1), "d")]
            if unres:
                MPA = [numpy.ones( (logfile.nmo,1), "d")
                       for x in range(2)]

        # Write DOS and PDOS data to orbital_data.txt                

        #print("Writing orbital data to orbital_data.txt\n")
        outputfile=open(os.path.join("./GaussSum_tmp/orbital_data.txt"),"w")

        outputfile.write("NBasis:\t"+str(NBsUse)+"\n")
        outputfile.write("HOMO:\t%s" % "\t".join([str(x+1) for x in HOMO]))

        if not (groups and pop):
            # No point outputting group info since we don't have the %contribs
            outputfile.write("\nGroups:\t0\n")
        else:
            outputfile.write("\nGroups:\t"+str(len(groups.groups))+"\n")
            line = []
            for groupname in groupnames:
                v = groups.groups[groupname]
                line.append("%s\t%s" % (groupname," ".join(map(str,v))))
            outputfile.write("\n".join(line) + "\n")

        if unres:
            outputfile.write("\nAlpha MO\t\teV\tSymmetry")
        else:
            outputfile.write("\nMO\t\teV\tSymmetry")

        if groups and pop:
            t = "\t".join(groupnames)
            outputfile.write("\t" + t+"\tAccurate values (for the Electronic Transitions module)")
        if unres:
            if groups and pop:
                outputfile.write("\t"*(len(groups.groups)-1))
            outputfile.write("\tBeta MO\t\teV\tSymmetry")
            if groups and pop:
                outputfile.write(t+"\tAccurate values (for UVVis.py)")
        outputfile.write("\n")

        for i in range(max([len(x) for x in evalue])-1,-1,-1): # Print them out backwards
            line=str(i+1)+"\t"+levelname(i, HOMO[0])+"\t"+str(round(evalue[0][i],2))+"\t"+symmetry[0][i]
            if groups and pop:
                for j in range(len(groups.groups)):
                    line=line+"\t"+str(int(MPA[0][i,j]*100+.5))
                for j in range(len(groups.groups)):
                    line=line+"\t"+str(MPA[0][i,j])
            if unres and i<len(evalue[1]):
                line=line+"\t"+str(i+1)+"\t"+levelname(i, HOMO[1])+"\t"+str(round(evalue[1][i],2))+"\t"+symmetry[1][i]
                if groups and pop:
                    for j in range(len(groups.groups)):
                        line=line+"\t"+str(int(MPA[1][i,j]*100+.5))
                    for j in range(len(groups.groups)):
                        line=line+"\t"+str(MPA[1][i,j])
            outputfile.write(line+"\n")


        # Print out a file suitable for drawing with origin

        

        # Convolute the DOS spectrum

        #print("\nConvoluting the DOS spectrum\n")

        if not unres:
            spectrum = DOSconvolute(MPA[0], evalue[0])
        else:
            spectrum = DOSconvolute(MPA[0], evalue[0])
            spectrum_beta = DOSconvolute(MPA[1], evalue[1])

        # Write the convoluted DOS spectrum to disk
        #print("Writing DOS spectrum to DOS_spectrum.txt\n")
        outputfile=open(os.path.join(gaussdir,"DOS_spectrum.txt"),"w")

        firstline="DOS Spectrum"
        grouptext = ""
        if groups:
            grouptext = "\t"*(len(groups.groups)) # For "Total"
        if unres:
            firstline += "\tAlpha\t%sBeta\t%sAlpha MO eigenvalues\t" \
                         "Beta MO eigenvalues\n" % (grouptext,grouptext)
        else:
            firstline += "\t%s\tMO eigenvalues\n" % grouptext
        outputfile.write(firstline)

        line="Energy (eV)"
        if groups and pop:
            for x in groupnames:
                line=line+"\t"+x
            line+="\tTotal"
            if unres:
                for x in groupnames:
                    line=line+"\t"+x
                line+="\tTotal"
        outputfile.write(line+"\n")

        width=end-start
        for x in range(max([1000] + [len(y) for y in evalue])):
            line=""
            if x<1000: # Print the spectrum
                realx=width*x/1000+start # Print the DOS spectrum from 'start' to 'end'
                line=line+str(realx)
                if groups and pop:
                    for i in range(len(groups.groups)):
                        if spectrum.spectrum[i,x]<1e-10:
                            spectrum.spectrum[i,x]=0
                        line=line+"\t"+str(spectrum.spectrum[i,x])

                total = sum(spectrum.spectrum[:,x])
                if total < 1e-10:
                    total = 0.
                line = line + "\t" + str(total) # Print the total
                if unres:
                    if groups and  pop:
                        for i in range(len(groups.groups)):
                            if spectrum_beta.spectrum[i,x]<1e-10:
                                spectrum_beta.spectrum[i,x]=0
                            line=line+"\t"+str(spectrum_beta.spectrum[i,x])
                    total = sum(spectrum_beta.spectrum[:,x])
                    if total < 1e-10:
                        total = 0.
                    line = line + "\t" + str(total) # Print the total                    

            if x<max([len(y) for y in evalue]): # Print the energy levels
                if line=="": # if the DOS spectrum is finished
                    line=line+'\t'*(numgroups+2) # make the first two columns blank
                line=line+"\t"+str(evalue[0][x])
                if unres:
                    line = line + "\t"
                    if x<len(evalue[1]):
                        line = line + str(evalue[1][x])
                line=line+"\t-1"

            line=line+"\n"
            outputfile.write(line)
        outputfile.close()

        #if root:

        # Plot the DOS spectrum as follows
        #
        # For res:
        #  one plot with stacked PDOS plus evalues at bottom
        # For unres:
        #  one plot with stacked sum_of_PDOS's plus both evalues at bottom
        
        g = MPLPlot()

        if groups and pop:
            #print("Plotting the stacked PDOS\n")
            if not unres:
                total = numpy.zeros(len(spectrum.xvalues),'d')
                for i in range(len(groups.groups)):
                    total += spectrum.spectrum[i,:]
                    g.data(zip(spectrum.xvalues,total),
                           title=groupnames[i])
            else:
                total = numpy.zeros(len(spectrum.xvalues),'d')
                for i in range(len(groups.groups)):
                    total += spectrum.spectrum[i,:] + spectrum_beta.spectrum[i,:]
                    g.data(zip(spectrum.xvalues,total),
                           title=groupnames[i])
        else:
            #print("Plotting the total DOS\n")
            if not unres:
                g.data(zip(spectrum.xvalues, spectrum.spectrum[0,:]),
                       title="DOS spectrum")
            else:
                g.data(zip(spectrum.xvalues, spectrum.spectrum[0,:]),
                       title='Alpha DOS spectrum')
                g.data(zip(spectrum_beta.xvalues, spectrum_beta.spectrum[0,:]),
                       title='Beta DOS spectrum')
                g.data(zip(spectrum.xvalues,
                          (spectrum.spectrum[0,:]+spectrum_beta.spectrum[0,:])*0.5),
                       title='Total DOS spectrum (scaled by 0.5)')

        # Plot the evalues
        g.data([ (x,-1) for x in evalue[0][:HOMO[0]+1] ], vlines=True,
               title='%sOccupied orbitals' % ["","Alpha "][unres])
        g.data([ (x,-1) for x in evalue[0][HOMO[0]+1:] ], vlines=True,
               title='%sVirtual orbitals' % ["","Alpha "][unres])

        if unres: # For the extra beta impulses
            g.data([ (x,-1) for x in evalue[1][:HOMO[0]+1] ], vlines=True,
                   title='Beta Occupied Orbitals')
            g.data([ (x,-1) for x in evalue[1][HOMO[0]+1:] ], vlines=True,
                   title='Beta Virtual Orbitals')

        g.setlabels("Energy (eV)", "")
        g.subplot.set_ylim(bottom=-1)
        g.subplot.set_xlim(left=start, right=end)
        g.subplot.legend(loc="upper right", prop={'size':8})
        mpl = g.makechart()
        return mpl
        

        #return # End of densityofstates()

############### START OF MAIN ##################
    #print("Starting to analyse the molecular orbitals\n")
    groupatoms=[]; groupname=[]; atomorb=[]

    # Create the output directory if necessary
    logdir=os.path.dirname(logfilename)
    logname=os.path.basename(logfilename)
    gaussdir=os.path.join("./GaussSum_tmp")
    #gaussdir=folder(screen,logfilename)

    # Read in the groups (if they exist!)
    filename = os.path.join(gaussdir,"Groups.txt")
    groups = False
    numgroups = 0
    if not os.path.isfile(filename):
        pass
        #print("Groups.txt not found\n")
    elif not (hasattr(logfile, "atombasis") and hasattr(logfile, "atomnos")):
        pass
        #print("Groups.txt found but not used as logfile does not have"
                     #" atombasis or atomnos\n") # not necessary depending on the groups
    else:
        #print("Reading Groups.txt\n")
        if not hasattr(logfile, "aonames"):
            groups = Groups(filename, logfile.atomnos, None, logfile.atombasis)
        else:
            groups = Groups(filename, logfile.atomnos, logfile.aonames, logfile.atombasis)
        numgroups = len(groups.groups)
        #print("There are %d groups\n" % numgroups)

    NAtoms = logfile.natom
    NBasis = logfile.nbasis
    NBsUse = logfile.nmo

    # Verify that groups.txt is okay
    if groups:
        status = groups.verify("DOS")
        if status:
            print(status)
            return 1

    #print("The number of atoms is "+str(NAtoms)+"\n")
    #print("NBasis is "+str(NBasis)+"\n")
    #print("NBsUse is "+str(NBsUse)+"\n")

    pop=False

    if hasattr(logfile, "aooverlaps") and hasattr(logfile, "mocoeffs"):

        #print("Found an overlap matrix and MO coefficients\n")

        pop=True # Assuming you never get an overlap matrix without the MOCoeffs
        MOCoeff = logfile.mocoeffs
        overlap = logfile.aooverlaps
        atomorb = logfile.aonames

        if len(MOCoeff)==2:
            pass
            #print("This is an unrestricted calculation - found the Beta MO coefficents\n")

    unres=False
    try:
        HOMO = logfile.homos
    except:
        return "Orbital Information Doesn't Exist."
    if len(HOMO)==2:
        #print("This is an unrestricted calculation\n")
        unres=True

    evalue = logfile.moenergies
    if hasattr(logfile,"mosyms"):
        symmetry = logfile.mosyms
    else:
        symmetry = [["?" for x in evalue[0]]]
        if unres:
            symmetry.append(["?" for x in evalue[1]])

    #print("Number of evalues found: %s\n" %
                   #" ".join([str(len(x)) for x in evalue]))

    #print("Number of orbital symmetries found: %s\n" %
                   #" ".join([str(len(x)) for x in symmetry]))

    mpldobject = densityofstates()
    return mpldobject
    #print("Finished\n")
