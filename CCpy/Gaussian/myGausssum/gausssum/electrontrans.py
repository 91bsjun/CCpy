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
# General Public License for more details

import os
import sys
import math
import stat
import numpy
import glob

from gausssum.utils import GaussianSpectrum, levelname, percent
from gausssum.cclib.parser.utils import convertor

# from Tkinter import *
from .mpl import MPLPlot
from .plot import DisplayPlot
from .folder import folder

def createEDDM(screen, logfile, contrib, gaussdir, unres):

    def cubman(cmds):
        if cmds[0] == "sc":
            output.write("echo Scaling %s by %s\n" % (cmds[1], cmds[-1]))
        elif cmds[0] == "a":
            output.write("echo Adding to %s\n" % cmds[3])
        elif cmds[0] == "sq":
            output.write("echo Squaring %s\n" % cmds[1])
        elif cmds[0] == "su":
            output.write("echo Subtracting %s from %s\n" % (cmds[1], cmds[3]))
        lines = []
        lines.append("echo %s > tmp.txt" % cmds[0])
        for cmd in cmds[1:]:
            lines.append("echo %s >> tmp.txt" % cmd)
        cmdname = syscmd[CAT]
        lines.append("%s tmp.txt | %s%scubman > tmp2.txt\n" % (cmdname, envvar('G03DIR'), os.sep))
        output.write("\n".join(lines))

    def envvar(x):
        if windows:
            return "%" + x + "%"
        else:
            return "${" + x + "}"

    def createcubes(mo):
        cube = "mo%s.cub" % getmo(mo)
        report = "echo Creating cubefile %s" % cube
        orb = mo[0] + 1
        if unres:
            calctype = ['alpha', 'beta'][mo[1]]
            orb += len(logfile.moenergies[0]) * mo[1]
        cmd = "%s%scubegen 0 MO=%d %s %s -2 h\n" % (
                    envvar('G03DIR'), os.sep, orb, fchkpoint, cube)
        if windows:
            line = "if not exist %s %s & %s\n" % (cube, report, cmd)
        else:
            line = 'if [ ! -e "%s" ]\nthen\n  %s\n  %s\nfi\n' % (cube, report, cmd)
        output.write(line)

    def getmo(mo):
        suffix = ""
        if unres:
            suffix = ["A", "B"][mo[1]]
        return "%d%s" % (mo[0] + 1, suffix)
        
    def createsquares(mo):
        cube = "mo%s.cub" % getmo(mo)
        sq = "sq%s.cub" % getmo(mo)
        if windows:
            output.write("if not exist %s (\n" % sq)
        else:
            output.write("if [ ! -e %s ]\nthen\n" % sq)
        cubman(["sq", cube, "y", sq, "y"])
        if windows:
            output.write(")\n")
        else:
            output.write("fi\n")

    def formchk(filename):
        filename = filename.split(fakeossep)[-1]
        fchkpoint = filename.replace('.chk', '.fck')
        report = "echo Formatting %s to %s" % (filename, fchkpoint)
        cmd = "%s%sformchk %s %s > tmp2.txt\n" % (envvar('G03DIR'), os.sep, filename,
                                      fchkpoint)
        if windows:
            line = "if not exist %s %s & %s\n" % (fchkpoint, report, cmd)
        else:
            line = 'if [ ! -e "%s" ]\nthen\n  %s\n  %s\nfi\n' % (fchkpoint, report, cmd)
        return fchkpoint, line

    windows = sys.platform == "win32"
##    windows = False
    fakeossep = os.sep
##    fakeossep = "\\"
##    os.sep = "/"

    COPY, DELETE, CAT, MOVE = range(4)
    syscmd = ['cp', 'rm', 'cat', 'mv']
    if windows:
         syscmd = ['copy', 'del', 'type', 'move']

    fchkpoint = glob.glob(os.path.join(gaussdir, "*.fck")) + glob.glob(os.path.join(gaussdir, "*.fchk"))
    line = ""
    if len(fchkpoint) == 0:
        chkpoint = glob.glob(os.path.join(gaussdir, "*.chk"))
        fchkpoint, line = formchk(chkpoint[0])
    else:
        fchkpoint = fchkpoint[0].split(fakeossep)[-1]
        # Error!

    screen.write("Creating the EDDM script file for %s\n" % fchkpoint)

    if windows:
        output = open(os.path.join(gaussdir, "eddm.bat"), "w")
        output.write("@echo off\n")
    else:
        finalsection = []
        output = open(os.path.join(gaussdir, "eddm.sh"), "w")
        os.chmod(os.path.join(gaussdir, "eddm.sh"), stat.S_IRWXU) # Make executable
    if line:
        output.write(line)
    output.write("echo === Using formatted checkpoint %s ===\n" % fchkpoint)
    for i in range(len(logfile.etenergies)): # For each transition
        if windows:
            output.write("""if "%%1" == "%d" goto :tran%d\n""" % (i+1, i+1))
        else:
            finalsection.append("""if [ "$1" == "%d" ]\nthen\n  tran%d\nfi\n""" % (i+1, i+1))
    line = "echo You need to specify a transition in the range 1 to %d\n" % len(logfile.etenergies)
    if windows:
        output.write("%s\ngoto :end\n" % line)
    else:
        finalsection.append(line)

    for i in range(len(logfile.etenergies)): # For each transition
        if windows:
            output.write(":tran%d\n" % (i+1))
        else:
            output.write("function tran%d\n{\n" % (i+1))

        totalcon = sum(contrib[i])
        scales = [x / float(totalcon) for x in contrib[i]]
        minimum_scale = 0 # Don't include contributions that don't contribute!

        # Create any necessary cube and squared files
        alreadydone = set()
        for sec, scale in zip(logfile.etsecs[i], scales):
            if scale > minimum_scale:
                for M in range(2):
                    if sec[M] not in alreadydone:
                        createcubes(sec[M])
                        createsquares(sec[M])
                        alreadydone.add(sec[M])

        N = 0 # Find the first transition with non-zero scale
        while N < len(scales) and scales[N] <= minimum_scale:
            N += 1
        if N < len(scales):
            scale = scales[N]
            cubman(["sc","sq%s.cub" % getmo(logfile.etsecs[i][N][0]),"y","before.cub","y",str(scale)])
            cubman(["sc","sq%s.cub" % getmo(logfile.etsecs[i][N][1]),"y","after.cub","y",str(scale)])
            created_tmp = False
            for j in range(N + 1, len(logfile.etsecs[i])):
                scale = scales[j]
                if scale > minimum_scale:
                    created_tmp = True
                    cubman(["sc","sq%s.cub" % getmo(logfile.etsecs[i][j][0]),"y","tmp.cub","y",str(scale)])
                    cubman(["a","tmp.cub","y","before.cub","y","tmp2.cub","y"])
                    output.write("%s tmp2.cub before.cub\n" % syscmd[MOVE])
                    cubman(["sc","sq%s.cub" % getmo(logfile.etsecs[i][j][1]),"y","tmp.cub","y",str(scale)])
                    cubman(["a","tmp.cub","y","after.cub","y","tmp2.cub","y"])
                    output.write("%s tmp2.cub after.cub\n" % syscmd[MOVE])
            if created_tmp:
                output.write("%s tmp.cub\n" % syscmd[DELETE])
            cubman(["su","before.cub","y","after.cub","y",
                    "trans%d.cub" % (i+1,),"y"])
        else:
            output.write("echo No significant contributions found")
        if windows:
            output.write("goto :end\n")
        else:
            output.write("}\n")

    if windows:
        output.write(":end\n")
    else:
        output.write("\n".join(finalsection))
    output.close()

def readorbital_data(inputfile):
    # Reads in all data from orbital_data.txt

    line=inputfile.readline(); NBasis=int(line.split()[1])
    line=inputfile.readline().split(); HOMO=[int(line[1])-1]
    unres=False
    if len(line)==3:
        unres=True # This is an unrestricted calculation
        HOMO.append(int(line[2])-1)
    line=inputfile.readline(); NGroups=int(line.split()[1])
    groupname=[]; groupatoms=[]
    for i in range(NGroups): # Read in group info
        line=inputfile.readline()
        temp=line.split('\t')
        groupname.append(temp[0])
        groupatoms.append(map(int,temp[1].split()))

    line=inputfile.readline()
    headers = inputfile.readline()

    # Contribs for orbital#1 will be in contrib[0][0-->NGroups]
    if not unres:
        contrib = numpy.zeros((1,NBasis,NGroups), "d")
    else:
        contrib = numpy.zeros((2,NBasis,NGroups), "d")

    evalues=[]
    for i in range(NBasis-1,-1,-1):
        line=inputfile.readline().split()
        evalue = [float(line[2])]
        more = line[4+NGroups:4+NGroups*2] # Strip off the crud at the start
        contrib[0,i,:] = [float(x) for x in more]

        if unres:
            evalue.append(float(line[6+NGroups*2]))
            more = line[8+NGroups*3:8+NGroups*4]
            contrib[1,i,:] = [float(x) for x in more]
        evalues.append(evalue)

    evalue.reverse() # Because you're reading them in backwards

    return HOMO,NBasis,NGroups,groupname,groupatoms,contrib,evalues


def ET(root,screen,logfile,logfilename,
       start,end,numpts,FWHM,UVplot,
       EDDM):

    screen.write("Starting to analyse the electronic transitions\n")

    # Create output directory if necessary
    gaussdir=folder(screen,logfilename)
    unres = len(logfile.homos) > 1

    CIS = logfile.etsecs

    if UVplot==True:
#######################################################
#                 UV-Visible section                  #
#######################################################

    # Read in orbital_data.txt if it exists(which contains info on the contribs)
        NGroups=0
        orbdata = False
        try:
            inputfile=open(os.path.join(gaussdir,"orbital_data.txt"),"r")
        except IOError:
            screen.write("orbital_data.txt not found\n")
        else:
            thisHOMO,NBasis,NGroups,groupname,groupatoms,contrib,evalue=readorbital_data(inputfile)
            inputfile.close()

            orbdata=True
            screen.write("Using orbital_data.txt\n")
            if thisHOMO != list(logfile.homos):
                screen.write("Disagreement on HOMO...orbital_data.txt says "+str(thisHOMO)+"\n"+logfile.filename+" says "+str(logfile.homos)+"\n")
                return False
            screenCD=[]
            for i in range(len(logfile.etenergies)): # For each transition
                screenCD.append("")
                # charge density [before, after] for each of the groups for each of the transitions

        majorCIS, minorCIS = [[] for x in logfile.etenergies], [[] for x in logfile.etenergies]
        allpercent = [[] for x in logfile.etenergies]
        for i in range(len(logfile.etenergies)): # For each transition
            if orbdata:
                CD=[ [0, 0] for x in range(NGroups)]
            totcontribs = 0
            for j in range(len(CIS[i])): # For each contribution
                thisCIS = CIS[i][j]
                mycontrib = thisCIS[2] ** 2
                totcontribs += mycontrib # tot up the contribs
                percontrib = int(mycontrib * 100 + .5)
                if orbdata:
                    for k in range(NGroups):
                        CD[k][0] += mycontrib * contrib[thisCIS[0][1], thisCIS[0][0], k] # Add to 'before' charge density
                        CD[k][1] += mycontrib * contrib[thisCIS[1][1], thisCIS[1][0], k] # Add to 'after' charge density
                alphabeta = ["", ""]
                if unres:
                    alphabeta = [["(A)", "(B)"][thisCIS[0][1]], ["(A)", "(B)"][thisCIS[1][1]]]
                CIStext = "%s%s->%s%s (%d%%)" % (levelname(thisCIS[0][0], logfile.homos[thisCIS[0][1]]), alphabeta[0],
                                                   levelname(thisCIS[1][0], logfile.homos[thisCIS[1][1]]), alphabeta[1],
                                                   percontrib)
                if percontrib >= 10: # Major contributions (>=10%)
                    majorCIS[i].append(CIStext)
                elif percontrib >= 2: #Minor contributions (>=2%)
                    minorCIS[i].append(CIStext)
                allpercent[i].append(percontrib)

            if orbdata:
                for j in range(len(groupname)): # The charge densities are scaled so that they add to one
                    CD[j][0]=CD[j][0]/totcontribs
                    CD[j][1]=CD[j][1]/totcontribs
                    screenCD[i]=screenCD[i]+percent(CD[j][0])+"-->"+percent(CD[j][1])+" ("+percent(round(CD[j][1],2)-round(CD[j][0],2))+")\t"

        if EDDM:
            createEDDM(screen, logfile, allpercent, gaussdir, unres)

    # Write UVData.txt containing info on the transitions

        fileoutput = ""
        for i in range(len(logfile.etenergies)): # For each transition
            temp = [i+1,logfile.etenergies[i],
                    convertor(logfile.etenergies[i],"cm-1","nm"),
                    logfile.etoscs[i],logfile.etsyms[i],
                    ", ".join(majorCIS[i]),", ".join(minorCIS[i])]
            if orbdata:
                temp.append(screenCD[i])
            fileoutput += "\t".join(map(str,temp)) + "\n"

        screen.write("Writing the transition info to UVData.txt\n")
        outputfile=open(os.path.join(gaussdir,"UVData.txt"),"w")
        outputfile.write("HOMO is "+str(logfile.homos[0]+1))
        if unres:
            outputfile.write("\t%d" % (logfile.homos[1] + 1,))
        outputfile.write("\nNo.\tEnergy (cm-1)\tWavelength (nm)\tOsc. Strength\tSymmetry\tMajor contribs\tMinor contribs")
        for i in range(NGroups):
            outputfile.write("\t"+groupname[i])
        outputfile.write("\n"+fileoutput)
        outputfile.close()

        endwaveno = convertor(start,"nm","cm-1")
        startwaveno = convertor(end,"nm","cm-1")
        t = GaussianSpectrum(startwaveno,endwaveno,numpts,
                             ( logfile.etenergies,[[x*2.174e8/FWHM for x in logfile.etoscs]] ),
                             FWHM)
        screen.write("Writing the spectrum to UVSpectrum.txt\n")
        outputfile=open(os.path.join(gaussdir,"UVSpectrum.txt"),"w")
        outputfile.write("Energy (cm-1)\tWavelength (nm)\tAbs\t<--UV Spectrum\tUV-Vis transitions-->\tEnergy (cm-1)\tWavelength (nm)\tOsc. strength\n")

        width=endwaveno-startwaveno
        for x in range(numpts):
            realx=width*x/numpts+startwaveno
            outputfile.write(str(realx)+"\t"+str(convertor(realx,"cm-1","nm"))+"\t"+str(t.spectrum[0,x]))
            if x<len(logfile.etenergies): # Write the oscillator strengths out also
                outputfile.write("\t\t\t"+str(logfile.etenergies[x])+"\t"+str(convertor(logfile.etenergies[x],"cm-1","nm"))+"\t"+str(logfile.etoscs[x]))
            outputfile.write("\n")
        outputfile.close()

        if root:
            if max(t.spectrum[0,:])<1E-8: # Don't draw it if the spectrum is flat
                screen.write("There are no peaks in this wavelength range!\n")
            else:
                g = MPLPlot()
                xvalues_nm = [convertor(x,"cm-1","nm") for x in t.xvalues]
                g.setlabels("Wavelength (nm)", r"$\epsilon$")
                g.data(zip(xvalues_nm,t.spectrum[0,:]))
                energies_nm = [convertor(x,"cm-1","nm") for x in logfile.etenergies]
                oscdata = [(x, y) for x, y in zip(energies_nm, logfile.etoscs) if start < x < end]
                g.data(oscdata, vlines=True, y2axis="Oscillator strength")

                g.subplot.set_xlim(left=start, right=end)
                g.secondaxis.set_ylim(bottom=0)
                DisplayPlot(root, g, "UV-Vis Spectrum")

    else:
#######################################################
# Circular dichroism section from here to end of main #
#######################################################

        if not hasattr(logfile,"etrotats"):
            screen.write("No rotatory lengths were found\n")
            return False
        elif len(logfile.etrotats)!=len(logfile.etenergies): # Catch violation of an assertion
            screen.write("The number of R values doesn't agree with the number of electronic transitions!\n")
            return False

        # Equation 8 in Stephens, Harada, Chirality, 2010, 22, 229.
        # This uses Delta, the half width at 1/e height.
        # We use Sigma, the full width at 1/e height.
        #   Delta = Sigma / 2

        endwaveno = convertor(start,"nm","cm-1")
        startwaveno = convertor(end,"nm","cm-1")
        sigma = convertor(FWHM, "eV", "cm-1")
        Delta = sigma / 2.

        prefactor = 1.0 / (2.296e-39 * math.sqrt(math.pi) * Delta)
        peakmax = []
        for i in range(len(logfile.etrotats)):
            peakmax.append(prefactor * logfile.etrotats[i] *
                           logfile.etenergies[i] * 1e-40)
        # FWHM is sqrt(ln2) times sigma
        real_FWHM = math.sqrt(math.log(2)) * sigma
        t = GaussianSpectrum(startwaveno,endwaveno,numpts,
                             ( logfile.etenergies,[peakmax] ),
                             real_FWHM)

        # Write CDSpectrum.txt containing info on the CD spectrum
        screen.write("Writing the spectrum to CDSpectrum.txt\n")
        outputfile=open(os.path.join(gaussdir,"CDSpectrum.txt"),"w")
        outputfile.write("Energy (cm-1)\tWidth (nm)\tAbs\t<--CD Spectrum\tStates-->\tWavelength (nm)\tEnergy (cm-1)\tR(length)\n")

        width = endwaveno - startwaveno
        for x in range(numpts):
            # Need to use float, otherwise it's an integer
            # realx = float(width)*x/numpts+start
            realx = width * x / numpts + startwaveno
            outputfile.write("%f\t%f\t%f" % (realx, 1.0e7/realx, t.spectrum[0,x]))
            if x < len(logfile.etenergies): # Write the R values out also
                outputfile.write( "\t\t\t%f\t%f\t%f" % (convertor(logfile.etenergies[x], "cm-1", "nm"),
                                                        logfile.etenergies[x],logfile.etrotats[x]) )
            outputfile.write("\n")
        outputfile.close()

        if root:
            # Plot the UV Spectrum using Gnuplot

            screen.write("Plotting using Gnuplot\n")

            if max(t.spectrum[0,:])<1E-8: # Gnuplot won't draw it if the spectrum is flat
                screen.write("There are no peaks in this wavelength range!\n")
            else:
                g = MPLPlot()
                g.setlabels("Wavelength (nm)", r"$\epsilon$")

                xvalues_nm = [convertor(x,"cm-1","nm") for x in t.xvalues]
                g.data(zip(xvalues_nm,t.spectrum[0,:]))
                energies_nm = [convertor(x,"cm-1","nm") for x in logfile.etenergies]
                oscdata = [(x, y) for x, y in zip(energies_nm, logfile.etrotats) if start < x < end]
                g.data(oscdata, vlines=True, y2axis="R (length) / $10^{-40}$")

                g.subplot.set_xlim(left=start, right=end)
                g.secondaxis.set_ylim(bottom=0)
                DisplayPlot(root, g, "Circular dichroism spectrum")

    screen.write("Finished")

#######################################################
#                      End of main                    #
#######################################################    
