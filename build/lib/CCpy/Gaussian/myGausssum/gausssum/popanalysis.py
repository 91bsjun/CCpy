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
from .mpl import MPLPlot
from .folder import folder
from gausssum.utils import levelname
from gausssum.utils import GaussianSpectrum
from gausssum.utils import Groups

def Popanalysis(root,screen,logfile,logfilename,start,end,COOP,FWHM,makeorigin):

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

        screen.write("Writing orbital data to orbital_data.txt\n")
        outputfile=open(os.path.join(gaussdir,"orbital_data.txt"),"w")

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

        if makeorigin==True:
            originoutput(MPA)

        # Convolute the DOS spectrum

        screen.write("\nConvoluting the DOS spectrum\n")

        if not unres:
            spectrum = DOSconvolute(MPA[0], evalue[0])
        else:
            spectrum = DOSconvolute(MPA[0], evalue[0])
            spectrum_beta = DOSconvolute(MPA[1], evalue[1])

        # Write the convoluted DOS spectrum to disk
        screen.write("Writing DOS spectrum to DOS_spectrum.txt\n")
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

        if root:

            # Plot the DOS spectrum as follows
            #
            # For res:
            #  one plot with stacked PDOS plus evalues at bottom
            # For unres:
            #  one plot with stacked sum_of_PDOS's plus both evalues at bottom

            g = MPLPlot()

            if groups and pop:
                screen.write("Plotting the stacked PDOS\n")
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
                screen.write("Plotting the total DOS\n")
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

            if not unres:
                DisplayPlot(root, g, "DOS spectrum")
            else:
                DisplayPlot(root, g, "DOS spectrum (sum of alpha plus beta electrons)")

        return # End of densityofstates()

    def crystalorbital():
        """Do the COOP."""

        groupoverlap = [numpy.zeros( (len(evalue[0]),numgroups, numgroups), "d")]
        if unres:
            groupoverlap.append(numpy.zeros( (len(evalue[1]),numgroups, numgroups), "d"))
        groupnames = list(groups.groups.keys())

        for j in range(len(HOMO)):
            for k in range(len(evalue[j])):
                kcoop = numpy.outer(MOCoeff[j][k,:], MOCoeff[j][k,:]) * overlap

                for groupa in range(numgroups-1):
                    ao_a = groups.groups[groupnames[groupa]]
                    for groupb in range(1, numgroups):
                        ao_b = groups.groups[groupnames[groupb]]
                        lap = sum(Numeric.take(Numeric.take(kcoop,ao_a),
                                               ao_b,axis=1).ravel())

                        groupoverlap[j][k,groupa,groupb] = lap
                        groupoverlap[j][k,groupb,groupa] = lap


        # Create COOP_data.txt
        output=open(os.path.join(gaussdir,"COOP_data.txt"),"w")
        listofoverlaps = []
        for fragx in range(numgroups-1):
            for fragy in range(fragx+1,numgroups):
                listofoverlaps.append(groupnames[fragx]+" and "+groupnames[fragy])
        numoverlaps = len(listofoverlaps)
        listofoverlaps = "\t".join(listofoverlaps)
        if not unres:
            output.write("MO\t\tevalue\t%s\n" % listofoverlaps)
        else:
            output.write("Alpha MO\t\tevalue\t%sBeta MO\t\tevalue\t%s\n" %
                         (listofoverlaps, listofoverlaps))

        for k in range(max([len(x) for x in evalue])-1, -1, -1):
            for j in range(len(HOMO)):
                if j==1 and k>=len(evalue[1]):
                    continue
                output.write(str(k+1)+"\t"+levelname(k, HOMO[j])+"\t"+str(evalue[j][k]))
                for fragx in range(len(groups.groups)-1):
                    for fragy in range(fragx+1,len(groups.groups)):
                        output.write("\t"+str(groupoverlap[j][k,fragx,fragy]))
            output.write("\n")
        output.close()

        # Convolute the COOP spectrum
        spectrum = []
        for j in range(len(HOMO)):
            overlaps = []
            for fragx in range(numgroups - 1):
                for fragy in range(fragx+1, numgroups):
                    overlaps.append(groupoverlap[j][:,fragx,fragy])
            spectrum.append(GaussianSpectrum(start, end, 1000,
                                        (evalue[j], overlaps), FWHM))

        # Create COOP_spectrum.txt
        output = open(os.path.join(gaussdir,"COOP_spectrum.txt"),"w")
        output.write("\t")
        if unres:
            output.write("Alpha%sBeta%s" % ("\t"*numoverlaps,"\t"*numoverlaps))
        output.write("Total\n")
        output.write("eV\t%s" % listofoverlaps)
        if unres:
            output.write("\t%s\t%s" % (listofoverlaps, listofoverlaps))
        output.write("\n")

        for i in range(len(spectrum[0].xvalues)):
            output.write("%f" % spectrum[0].xvalues[i])
            for k in range(len(spectrum)):
                for j in range(len(spectrum[k].spectrum[:,0])):
                    output.write("\t%f" % spectrum[k].spectrum[j,i])
            if unres:
                output.write("\t%f" % sum([spectrum[x].spectrum[j,i] for x in [0,1]]))
            output.write("\n")
        output.close()

        g = MPLPlot()

        i = 0
        for fragx in range(numgroups-1):
            for fragy in range(fragx+1, numgroups):
                for j in range(len(spectrum)):
                    if not unres:
                        g.data( zip(spectrum[j].xvalues,spectrum[j].spectrum[i,:]),
                                title='overlap of %s with %s' % (groupnames[fragx], groupnames[fragy]))
                    else:
                        g.data( zip(spectrum[j].xvalues,spectrum[j].spectrum[i,:]),
                                title='%s overlap of %s with %s' % (['Alpha','Beta'][j], groupnames[fragx], groupnames[fragy]))
##                if unres: # Plot the total or not?
##                    g.data( zip(spectrum[j].xvalues,spectrum[0].spectrum[i,:] + spectrum[1].spectrum[i,:]),
##                            title='Total overlap of %s with %s' % ( groupnames[fragx], groupnames[fragy]))
                i += 1

        g.setlabels("Energy (eV)", "")
        g.subplot.set_xlim(left=start, right=end)
        g.subplot.legend(loc="upper right", prop={'size':8})
        DisplayPlot(root,g,"COOP spectrum")

        return # End of crystalorbital()


############### START OF MAIN ##################

    screen.write("Starting to analyse the molecular orbitals\n")
    groupatoms=[]; groupname=[]; atomorb=[]

    # Create the output directory if necessary
    gaussdir=folder(screen,logfilename)

    # Read in the groups (if they exist!)
    filename = os.path.join(gaussdir,"Groups.txt")
    groups = False
    numgroups = 0
    if not os.path.isfile(filename):
        screen.write("Groups.txt not found\n")
    elif not (hasattr(logfile, "atombasis") and hasattr(logfile, "atomnos")):
        screen.write("Groups.txt found but not used as logfile does not have"
                     " atombasis or atomnos\n") # not necessary depending on the groups
    else:
        screen.write("Reading Groups.txt\n")
        if not hasattr(logfile, "aonames"):
            groups = Groups(filename, logfile.atomnos, None, logfile.atombasis)
        else:
            groups = Groups(filename, logfile.atomnos, logfile.aonames, logfile.atombasis)
        numgroups = len(groups.groups)
        screen.write("There are %d groups\n" % numgroups)

    NAtoms = logfile.natom
    NBasis = logfile.nbasis
    NBsUse = logfile.nmo

    # Verify that groups.txt is okay
    if groups:
        if COOP==False:
            status = groups.verify("DOS")
        else:
            status = groups.verify("COOP")
        if status:
            screen.write(status)
            return 1

    screen.write("The number of atoms is "+str(NAtoms)+"\n")
    screen.write("NBasis is "+str(NBasis)+"\n")
    screen.write("NBsUse is "+str(NBsUse)+"\n")

    pop=False

    if hasattr(logfile, "aooverlaps") and hasattr(logfile, "mocoeffs"):

        screen.write("Found an overlap matrix and MO coefficients\n")

        pop=True # Assuming you never get an overlap matrix without the MOCoeffs
        MOCoeff = logfile.mocoeffs
        overlap = logfile.aooverlaps
        atomorb = logfile.aonames

        if len(MOCoeff)==2:
            screen.write("This is an unrestricted calculation - found the Beta MO coefficents\n")

    unres=False
    HOMO = logfile.homos
    if len(HOMO)==2:
        screen.write("This is an unrestricted calculation\n")
        unres=True

    evalue = logfile.moenergies
    if hasattr(logfile,"mosyms"):
        symmetry = logfile.mosyms
    else:
        symmetry = [["?" for x in evalue[0]]]
        if unres:
            symmetry.append(["?" for x in evalue[1]])

    screen.write("Number of evalues found: %s\n" %
                   " ".join([str(len(x)) for x in evalue]))

    screen.write("Number of orbital symmetries found: %s\n" %
                   " ".join([str(len(x)) for x in symmetry]))

    if COOP==True:
        if not (groups and pop):
            screen.write("To calculate the COOP spectrum, you need Groups.txt and a log file containing a full population analysis\n")
            return 1
        crystalorbital()

    else: # Density of States
        densityofstates()

    screen.write("Finished\n")
