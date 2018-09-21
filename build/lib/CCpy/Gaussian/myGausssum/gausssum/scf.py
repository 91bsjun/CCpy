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
import sys
import math

from .plot import DisplayPlot
from .mpl import MPLPlot

def SCF(root,screen,logfile,numpoints):

    screen.write("Starting to analyse the progress of the SCF\n")

    scfvalues = logfile.scfvalues[-1] # The most recent in the logfile
    scftargets = logfile.scftargets[-1] # Ditto

    deviation = []
    for i in range(len(scfvalues)): # Which SCF cycle
        dev = 0
        for j in range(len(scftargets)): # Which target
            if abs(scfvalues[i][j]) > scftargets[j]:
                dev += math.log(abs(scfvalues[i][j]) / scftargets[j])
        deviation.append(dev)

    if len(deviation)>=numpoints+2: # If there are two points to plot

        h = MPLPlot()
        h.setlabels("SCF convergence step", "Deviation from targets")
        data = list(zip(range(len(deviation)-numpoints),deviation[numpoints:]))
        h.data(data)
        h.data(data, lines=False)
        h.subplot.set_ylim(bottom=0)

        DisplayPlot(root,h,"Plot of SCF deviation vs Iteration")
    else:
        screen.write("I need at least two points to plot\n")

    screen.write("Finished\n")
