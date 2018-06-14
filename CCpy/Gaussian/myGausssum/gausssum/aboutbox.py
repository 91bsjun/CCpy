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

from tkinter import *   # GUI stuff
import webbrowser
import tkinter.simpledialog
import traceback
import copy             # For deepcopy...until I find a better way of doing this

from gausssum.cclib.parser import ADF, GAMESS, Gaussian
import os, sys
if hasattr(sys, "frozen"): # i.e. if using py2exe
    installlocation = os.path.dirname(sys.executable)
else:
    import gausssum
    installlocation = gausssum.__path__[0]

class AboutPopupBox(tkinter.simpledialog.Dialog):

    def __init__(self, parent, title = None): # Override (just to set the geometry!)

        Toplevel.__init__(self, parent)
        self.transient(parent)

        if title:
            self.title(title)

        self.parent = parent
        self.result = None
        body = Frame(self)
        self.initial_focus = self.body(body)
        body.pack(padx=5, pady=5)

        self.buttonbox()
        self.grab_set()

        if not self.initial_focus:
            self.initial_focus = self

        self.protocol("WM_DELETE_WINDOW", self.cancel)

        self.initial_focus.focus_set()
        self.wait_window(self)


    def body(self,master): # Override

        self.resizable(False,False)
        self.canvas2 = Canvas(master,width=340,height=260)
        self.canvas2.pack(side=TOP)

        self.photo2 = PhotoImage(file=os.path.join(installlocation,"mesh.gif"))
        self.item2 = self.canvas2.create_image(11,11,anchor=NW,image=self.photo2)

        Label(master,text="(c) 2013 Noel O'Boyle").pack(side=TOP)
        Label(master,text="http://gausssum.sf.net").pack(side=TOP)
        Label(master,text="").pack(side=TOP) # Creates a bit of spacing at the bottom
        Label(master,text="Support GaussSum by citing:").pack(side=TOP)
        Label(master,text="N.M. O'Boyle, A.L. Tenderholt and K.M. Langner.",font=("Times",10,"bold")).pack(side=TOP)
        Label(master,text="J. Comp. Chem. 2008, 29, 839-845.",font=("Times",10,"bold")).pack(side=TOP)
        #Button(master,text="GaussSum home page on SourceForge",command=self.openmybrowser).pack(side=TOP)
        Label(master,text="").pack(side=TOP) # Creates a bit of spacing at the bottom

        x = (652-354)//2 + self.parent.winfo_rootx() # Popup is 354x389
        y = (480-405)//2 + self.parent.winfo_rooty()
        self.geometry("354x405+"+str(x)+"+"+str(y)) # Place it in the centre of the root window

    def openmybrowser(self): # New
        webbrowser.open("http://gausssum.sf.net")

    def buttonbox(self): # Override
        box = Frame(self)
        w = Button(box, text="OK", width=10, command=self.ok, default=ACTIVE)
        w.pack(side=LEFT, padx=5, pady=5)
        self.bind("<Return>", self.ok)
        box.pack()
