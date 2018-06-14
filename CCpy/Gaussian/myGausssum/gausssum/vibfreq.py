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

import os
import math
import gausssum.utils

from .plot import DisplayPlot
from .mpl import MPLPlot
from .folder import folder

def activity_to_intensity(activity, frequency, excitation, temperature):
    """Convert Raman acitivity to Raman intensity according to
    Krishnakumar et al, J. Mol. Struct., 2004, 702, 9."""

    excitecm = 1 / (1e-7 * excitation)
    f = 1e-13
    above = f * (excitecm - frequency)**4 * activity
    exponential = -6.626068e-34 * 299792458 * frequency / (1.3806503e-23 * temperature)
    below = frequency * (1 - math.exp(exponential))
    return above / below

def get_scaling_factors(filename, scale):
    """Read in scaling factors from an existing output file.

    Note: Scale is prepopulated with the general scaling factor
    """
    inputfile = open(filename, "r")

    line = inputfile.readline()
    line = inputfile.readline()
    i = 0
    line = inputfile.readline().split('\t')
    while len(line) > 6: # Read in the individual scaling factors
        sf = line[-2]
        if sf != '':
            scale[i] = float(sf)
        i += 1
        line = inputfile.readline().split('\t')
    inputfile.close()
    return scale

def Vibfreq(root,screen,logfile,logfilename,start,end,numpts,FWHM,typeofscale,scalefactor,excitation,temperature):

    def dofreqs(name,act):

        screen.write("\n************* Doing the "+name+" *****************\n")
        filename=name+"Spectrum.txt"

        freq = logfile.vibfreqs.copy() # Copy so that it won't be changed by the routine
        if hasattr(logfile, "vibsyms"):
            vibsyms = logfile.vibsyms
        else:
            vibsyms = ['?'] * len(freq)

        # Handle the scaling of the frequencies
        scale = [scalefactor] * len(freq)
        if typeofscale == "Gen":
            screen.write("Going to scale with a scaling factor of "+str(scalefactor)+"\n")
            general = True
        else:
            screen.write("Going to use individual scaling factors\n")
            screen.write("Looking for scaling factors in "+filename+"...")
            inputfilename = os.path.join(gaussdir,filename)
            if not os.path.isfile(inputfilename):
                screen.write("not found\nGoing to use general scale factor of "+str(scalefactor)+" instead\n")
                general = True
            else:
                screen.write("found\n")
                general = False
                get_scaling_factors(inputfilename, scale) # Update scaling factors in 'scale' from file

        for i in range(len(freq)): # Scale the freqs
            freq[i] = freq[i]*scale[i]

        # Convolute the spectrum
        spectrum = gausssum.utils.Spectrum(start,end,numpts,
                                           [list(zip(freq,act))],
                                           FWHM,gausssum.utils.lorentzian)
        if name == "Raman":
            intensity = [activity_to_intensity(activity, frequency, excitation, temperature)
                         for activity, frequency in zip(act, freq)]
            spectrum_intensity = gausssum.utils.Spectrum(start,end,numpts,
                                           [list(zip(freq, intensity))],
                                           FWHM,gausssum.utils.lorentzian)

        outputfile = open(os.path.join(gaussdir,filename),"w")
        screen.write("Writing scaled spectrum to "+filename+"\n")
        outputfile.write("Spectrum\t%s\t\tNormal Modes\n" % ["", "\t"][name=="Raman"])
        outputfile.write("Freq (cm-1)\t%s act\t%s\tMode\tLabel\tFreq (cm-1)\t%s act\t" % (name, ["", "Intensity\t"][name=="Raman"],name))
        outputfile.write("%sScaling factors\tUnscaled freq\n" % ["", "Intensity\t"][name=="Raman"])
        width = end-start
        for x in range(0,numpts):
            if spectrum.spectrum[x,0]<1e-20:
                spectrum.spectrum[x,0] = 0.
            realx = width*(x+1)/numpts+start
            outputfile.write(str(realx)+"\t"+str(spectrum.spectrum[x,0]))
            if name == "Raman":
                outputfile.write("\t%f" % spectrum_intensity.spectrum[x,0])
            if x<len(freq): # Write the activities (assumes more pts to plot than freqs - fix this)
                outputfile.write("\t\t"+str(x+1)+"\t"+vibsyms[x]+"\t"+str(freq[x])+"\t"+str(act[x]))
                if name == "Raman":
                    outputfile.write("\t%f" % intensity[x])
                outputfile.write("\t"+str(scale[x])+"\t"+str(logfile.vibfreqs[x]))
            outputfile.write("\n")
        outputfile.close()

        if root:

            if general==True:
                title = "%s spectrum scaled by %f" % (name, scalefactor)
            else:
                title = "%s spectrum scaled by individual scaling factors" % name

            if name == "IR":
                g = MPLPlot()
                g.setlabels("Frequency (cm$^{-1}$)", "IR activity")
                g.subplot.invert_xaxis()
                g.subplot.invert_yaxis()

                g.data(zip(spectrum.xvalues,spectrum.spectrum[:,0]), title=title, lines=True)
                g.subplot.legend(loc="lower right", prop={'size':8})
                DisplayPlot(root, g, "%s Spectrum" % name)

            if name == "Raman":
                for type, spec in [("activity", spectrum),
                                   ("intensity", spectrum_intensity)]:
                    g = MPLPlot()
                    g.setlabels("Frequency (cm$^{-1}$)", "Raman %s" % type)
                    g.data(zip(spec.xvalues,spec.spectrum[:,0]), lines=True, title=title)
                    g.subplot.legend(loc="upper right", prop={'size':8})
                    DisplayPlot(root, g, "%s Spectrum" % name)


    ############## START OF MAIN FUNCTION #############

    screen.write("Starting to analyse the vibrational frequencies\n")

    # Create a new output folder if necessary (returns the location of it in any case)
    gaussdir=folder(screen,logfilename)

    if hasattr(logfile,"vibirs"):
        dofreqs("IR",logfile.vibirs)

    if hasattr(logfile,"vibramans"):
        dofreqs("Raman",logfile.vibramans)

    screen.write("Finished\n")

    return True

    ############## END OF MAIN FUNCTION #############
