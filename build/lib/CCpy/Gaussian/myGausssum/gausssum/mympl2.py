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
import sys
sys.path.append('/usr/local/lib/python3.4/site-packages/')
import matplotlib as mpl
mpl.use('Agg')
from matplotlib.figure import Figure
from matplotlib import rcParams
from matplotlib import pyplot
rcParams.update({'figure.autolayout': True})


import mpld3
from mpld3 import fig_to_html, plugins
# Uncomment the following for XKCD-style plots
# pyplot.xkcd()

colors = "bgrcmyk"

class MPLPlot(object):
    def __init__(self):
        self.figure = pyplot.figure(figsize=(6,5), dpi=100)
        self.subplot = self.figure.add_subplot(111)
        self.cindex =0

    def data(self, mtuples, lines=True, title=None, vlines=False, y2axis=""):
        color = colors[self.cindex]

        axis = self.subplot

        if not mtuples: return
        xvals, yvals = zip(*mtuples)
        for i, (xval, yval) in enumerate(zip(xvals, yvals)):
            axis.vlines(xval, yval, 0, color=color, label=title if i==0 else "")
            self.ax = axis.vlines(xval, yval, 0, color=color, label=title if i==0 else "")
        self.cindex += 1
        if self.cindex == len(colors):
            self.cindex = 0

    def setlabels(self, xaxis, yaxis):
        self.subplot.set_xlabel(xaxis)
        self.subplot.set_ylabel(yaxis)
    def makechart(self):
        fig = self.figure
        #plugins.connect(fig, plugins.MousePosition(fontsize=14))
        
        openfile = open("/var/www/FlaskApp/FlaskApp/static/GaussUploads/GaussSum3.0/UVData.txt")
        line = openfile.readlines()
        tmp=[]
        for i in line:
            spl = i.split()
            tmp.append(spl)
        majorcon=[]
        for i in range(len(tmp)):
            k=i+2
            if k==(len(tmp)):
                break;
            else:        
                majorcon.append(tmp[k][5:])
        
        plugins.connect(fig, plugins.PointHTMLTooltip(self.ax, majorcon))
        fig.savefig('/var/www/FlaskApp/FlaskApp/static/GaussUploads/etgraph.png')
        mpl=mpld3.fig_to_html(self.figure)
        return mpl

