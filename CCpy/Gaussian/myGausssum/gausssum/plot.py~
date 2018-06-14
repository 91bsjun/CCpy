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

from tkinter import *

# Matplotlib imports
import matplotlib
matplotlib.use('TkAgg')
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg
from matplotlib.backend_bases import key_press_handler
from matplotlib.figure import Figure

class DisplayPlot(object):
    def __init__(self, root, g, title):

        self.popup = Toplevel(root)
        self.popup.focus_set()

        self.popup.title(title)
        self.title = title # do this better

        self.popup.resizable(False,False)
        self.frame1 = Frame(self.popup)
        self.frame1.pack()

        canvas = FigureCanvasTkAgg(g.figure, master=self.frame1)
        canvas.show()
        canvas.get_tk_widget().pack(side=TOP, fill=BOTH, expand=1)

        toolbar = NavigationToolbar2TkAgg( canvas, self.frame1 )
        toolbar.update()
        canvas._tkcanvas.pack(side=TOP, fill=BOTH, expand=1)

        frame2 = Frame(self.frame1)
        frame2.pack(side=TOP)

        def on_key_event(event):
            key_press_handler(event, canvas, toolbar)

        def close(event=None):
            self.popup.destroy()

        Button(frame2, text="Close", underline=0, command=close).pack(side=LEFT)

        canvas.mpl_connect('key_press_event', on_key_event)
        self.popup.bind("<Return>",close)
        self.popup.bind("<Escape>",close)

        self.popup.bind("<Alt-c>",close)

        self.popup.protocol("WM_DELETE_WINDOW", close)

