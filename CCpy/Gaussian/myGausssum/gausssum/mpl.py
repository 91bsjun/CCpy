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

from matplotlib.figure import Figure
from matplotlib import rcParams
from matplotlib import pyplot
rcParams.update({'figure.autolayout': True})

# Uncomment the following for XKCD-style plots
# pyplot.xkcd()

colors = "bgrcmyk"

class MPLPlot(object):
    def __init__(self):
        self.figure = Figure(figsize=(6,5), dpi=100)
        self.subplot = self.figure.add_subplot(111)
        self.cindex =0

    def data(self, mtuples, lines=True, title=None, vlines=False, y2axis=""):
        color = colors[self.cindex]
        if y2axis:
            new_axis = self.subplot.twinx()
            new_axis.set_ylabel(y2axis)
            self.secondaxis = new_axis
            axis = new_axis
        else:
            axis = self.subplot

        if not mtuples: return
        xvals, yvals = zip(*mtuples)
        if not lines:
            axis.plot(xvals, yvals, "x", color=color, label=title)
        else:
            if not vlines:
                axis.plot(xvals, yvals, color=color, label=title)
            else:
                for i, (xval, yval) in enumerate(zip(xvals, yvals)):
                    axis.vlines(xval, yval, 0, color=color, label=title if i==0 else "")
        self.cindex += 1
        if self.cindex == len(colors):
            self.cindex = 0

    def setlabels(self, xaxis, yaxis):
        self.subplot.set_xlabel(xaxis)
        self.subplot.set_ylabel(yaxis)
