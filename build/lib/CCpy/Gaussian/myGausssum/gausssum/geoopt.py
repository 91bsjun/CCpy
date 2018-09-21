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

# from Tkinter import *
from .plot import DisplayPlot
from .mpl import MPLPlot

def GeoOpt(root, screen, logfile, numpts):

    screen.write("Starting GeoOpt.py\n")

    deviation = []
    for i in range(len(logfile.geovalues)):
        dev = 0
        for j in range(len(logfile.geotargets)):
            if abs(logfile.geovalues[i][j]) > logfile.geotargets[j]:
                dev += math.log(abs(logfile.geovalues[i][j]) / logfile.geotargets[j])
        deviation.append(dev)

    if len(logfile.scfenergies) >= numpts+2: # If there are two points to plot

        g = MPLPlot()
        g.setlabels("Optimisation Step", "Energy")
        data = list(zip(range(len(logfile.scfenergies)-numpts), logfile.scfenergies[numpts:]))
        g.data(data)
        g.data(data, lines=False)

        DisplayPlot(root, g, "Geometry optimisation")

        if len(deviation) >= numpts+2:

            h = MPLPlot()
            h.setlabels("Optimisation Step", "Deviation from targets")
            data = list(zip(range(len(deviation)-numpts), deviation[numpts:]))
            h.data(data)
            h.data(data, lines=False)
            h.subplot.set_ylim(bottom=0)

            DisplayPlot(root, h, "Deviation from targets")

    else:
        screen.write("I need at least two points to plot\n")

    screen.write("Finishing GeoOpt.py\n")
