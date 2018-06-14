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
import matplotlib as mpl
#mpl.use('Agg')
from matplotlib.figure import Figure
from matplotlib import rcParams
from matplotlib import pyplot
rcParams.update({'figure.autolayout': True})
import sys
#sys.path.append("/usr/local/lib/python3.4/site-packages/")
#import mpld3
#from mpld3 import fig_to_html, plugins

# Uncomment the following for XKCD-style plots
# pyplot.xkcd()

colors = "bgrcmyk"

class MPLPlot(object):
    def __init__(self):
        self.figure = pyplot.figure(figsize=(5,6), dpi=100)
        self.subplot = self.figure.add_subplot(211)
        self.subplot2 = self.figure.add_subplot(212)
        self.cindex =0

    def data(self, mtuples, lines=True, title=None, vlines=False, y2axis=""):
        color = colors[self.cindex]
        if y2axis:
            new_axis = self.subplot2
            new_axis.set_ylabel(y2axis)
            self.secondaxis = new_axis
            axis = new_axis
        else:
            axis = self.subplot

        if not mtuples: return
        xvals, yvals = zip(*mtuples)
        
        #self.ax = axis.vlines(xvals, yvals, 0, color="g")
        
        if not lines:
            axis.plot(xvals, yvals, "x", color=color, label=title)
        else:
            if not vlines:
                axis.plot(xvals, yvals, color=color, label=title)
            else:
                self.ax = axis.vlines(xvals, yvals, 0, color="g", linewidth="1")
                
        self.cindex += 1
        if self.cindex == len(colors):
            self.cindex = 0
        
    def setlabels(self, xaxis, yaxis):
        self.subplot.set_xlabel(xaxis)
        self.subplot2.set_xlabel(xaxis)
        self.subplot.set_ylabel(yaxis)

    def makechart(self):
        fig = self.figure
        #plugins.connect(fig, plugins.MousePosition(fontsize=14))
        
        openfile = open("./GaussSum_tmp/UVData.txt")
        line = openfile.readlines()
        tmp=[]
        for i in line:
            spl = i.split()
            tmp.append(spl)
        contri=[]
        for i in range(len(tmp)):
            k=i+2
            if k==(len(tmp)):
                break;
            else:        
                contri.append(tmp[k][5:])

        #plugins.connect(fig, plugins.MyTooltip(self.ax, contri))
        #fig.savefig('/var/www/FlaskApp/FlaskApp/static/GaussUploads/etgraph.png')
        #mpl=mpld3.fig_to_html(self.figure)
        return "ok"
