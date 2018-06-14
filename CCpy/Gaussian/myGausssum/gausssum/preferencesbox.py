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
import tkinter.messagebox     # For the About Dialog
import tkinter.simpledialog

import os

class PreferencesPopupBox(simpledialog.Dialog):

    def __init__(self, parent, settings, title = None): # Override (just to set the geometry!)

        Toplevel.__init__(self, parent)
        self.transient(parent)
        self.settings=settings                    # Note that changes to self.settings will affect the global self.settings thru 'settings'
        self.oldsettings = settings.copy()          # Remember the current settings
        if title:
            self.title(title)

        self.parent = parent
        self.result = None
        body = Frame(self)
        self.initial_focus = self.body(body)
        body.pack(padx=5, pady=5)

        self.buttonbox()

        if not self.initial_focus:
            self.initial_focus = self

        self.protocol("WM_DELETE_WINDOW", self.cancel)

        self.initial_focus.focus_set()
        self.wait_window(self)


    def body(self,master): # Override
        # The content of the settings dialog box

        # Creation of the main sections
        self.resizable(False,False)
        self.frame1=Frame(master,relief=SUNKEN,borderwidth=2)
        self.frame1.pack()

        Label(self.frame1,text="Search file").pack()
        self.frame3=Frame(self.frame1)
        self.frame3.pack()
        Label(self.frame1,text="").pack()

        Label(self.frame1,text="Frequencies").pack()
        self.frame4=Frame(self.frame1)
        self.frame4.pack()
        Label(self.frame1,text="").pack()

        Label(self.frame1,text="Orbitals").pack()
        self.frame5=Frame(self.frame1)
        self.frame5.pack()
        Label(self.frame1,text="").pack()

        Label(self.frame1,text="Electronic transitions").pack()
        self.frame6=Frame(self.frame1)
        self.frame6.pack()
        Label(self.frame1,text="").pack()

        # The Find.py section
        Label(self.frame3,text="Search for:").grid(row=0,column=0)
        self.find=[None]*4
        self.find[0]=Entry(self.frame3,width=15)
        self.find[1]=Entry(self.frame3,width=15)
        self.find[2]=Entry(self.frame3,width=15)
        self.find[3]=Entry(self.frame3,width=15)
        self.find[0].grid(row=0,column=1)
        self.find[1].grid(row=1,column=1)
        self.find[2].grid(row=0,column=2)
        self.find[3].grid(row=1,column=2)
        for i in range(4):
            self.find[i].delete(0,END)
            self.find[i].insert(0,self.settings[ 'find.text%d'%(i+1) ])

        # The IR_Raman.py section
        self.frame4a = Frame(self.frame4)
        self.frame4a.pack()
        self.irraman=[None]*6
        Label(self.frame4a,text="Start:").grid(row=0,column=0)
        self.irraman[0]=Entry(self.frame4a,width=5)
        self.irraman[0].grid(row=0,column=1)
        Label(self.frame4a,text="End:").grid(row=0,column=2)
        self.irraman[1]=Entry(self.frame4a,width=5)
        self.irraman[1].grid(row=0,column=3)
        Label(self.frame4a,text="Num pts:").grid(row=0,column=4)
        self.irraman[2]=Entry(self.frame4a,width=5)
        self.irraman[2].grid(row=0,column=5)
        Label(self.frame4a,text="FWHM:").grid(row=0,column=6)
        self.irraman[3]=Entry(self.frame4a,width=5)
        self.irraman[3].grid(row=0,column=7)

        self.frame4b = Frame(self.frame4)
        self.frame4b.pack()

        Label(self.frame4b,text="Exc. wavelength:").grid(row=0,column=0)
        self.irraman[4]=Entry(self.frame4b,width=5)
        self.irraman[4].grid(row=0,column=1)
        Label(self.frame4b,text="Temp:").grid(row=0,column=2)
        self.irraman[5]=Entry(self.frame4b,width=6)
        self.irraman[5].grid(row=0,column=3)
        a=['start','end','numpoints','fwhm','excitation','temperature']
        for i in range(6):
            self.irraman[i].delete(0,END)
            self.irraman[i].insert(0,self.settings[ 'ir_raman.%s'%a[i] ])


        # The MO.py section
        self.mo=[None]*3
        Label(self.frame5,text="Start:").grid(row=0,column=0)
        self.mo[0]=Entry(self.frame5,width=5)
        self.mo[0].grid(row=0,column=1)
        Label(self.frame5,text="End:").grid(row=0,column=2)
        self.mo[1]=Entry(self.frame5,width=5)
        self.mo[1].grid(row=0,column=3)
        Label(self.frame5,text="FWHM").grid(row=0,column=4)
        self.mo[2]=Entry(self.frame5,width=5)
        self.mo[2].grid(row=0,column=5)
        a=['start','end','fwhm']
        for i in range(3):
            self.mo[i].delete(0,END)
            self.mo[i].insert(0,self.settings['mo.%s'%(a[i])])


        # UVVis.py section
        self.uvvis=[None]*5
        Label(self.frame6,text="Start:").grid(row=0,column=0)
        self.uvvis[0]=Entry(self.frame6,width=5)
        self.uvvis[0].grid(row=0,column=1)
        Label(self.frame6,text="End:").grid(row=0,column=2)
        self.uvvis[1]=Entry(self.frame6,width=5)
        self.uvvis[1].grid(row=0,column=3)
        Label(self.frame6,text="Num pts:").grid(row=0,column=4)
        self.uvvis[2]=Entry(self.frame6,width=5)
        self.uvvis[2].grid(row=0,column=5)
        Label(self.frame6,text="FWHM:").grid(row=0,column=6)
        self.uvvis[3]=Entry(self.frame6,width=5)
        self.uvvis[3].grid(row=0,column=7)
        Label(self.frame6,text="sigma:").grid(row=0,column=8)
        self.uvvis[4]=Entry(self.frame6,width=5)
        self.uvvis[4].grid(row=0,column=9)

        a=['start','end','numpoints','fwhm','sigma']
        for i in range(5):
            self.uvvis[i].delete(0,END)
            self.uvvis[i].insert(0,self.settings['uvvis.%s'%(a[i])])

        x = (652-450)//2 + self.parent.winfo_rootx()
        y = (480-410)//2 + self.parent.winfo_rooty()

        self.geometry("450x460+"+str(x)+"+"+str(y)) # Place it in the centre of the root window

    def buttonbox(self): # Override
        box = Frame(self)
        cancel=Button(box,text="Cancel",width=10,command=self.cancel,default=ACTIVE)
        cancel.pack(side=LEFT,padx=5,pady=5)
        self.save = Button(box, text="Save", width=10, command=self.ok, default=ACTIVE)
        self.save.pack(side=LEFT, padx=5, pady=5)
        self.bind("<Return>",self.ok)
        self.bind("<Escape>",self.cancel)
        box.pack()

    def checkcubman(self, event=None): # Checks for existence of cubman at specificed location
        if not os.path.isfile(self.cubman.get()):
            messagebox.showerror(title="No such file",
                                   message='''
There isn't any file with this name.
                                   
Make sure you include the full path, filename and extension (if any).

For example (in Windows): C:\\Program Files\\Gaussian\\cubman.exe'''
                                   )

    def checkformchk(self, event=None): # Checks for existence of formchk at specificed location
        if not os.path.isfile(self.formchk.get()):
            messagebox.showerror(title="No such file",
                                   message='''
There isn't any file with this name.
                                   
Make sure you include the full path, filename and extension (if any).

For example (in Windows): C:\\Program Files\\Gaussian\\formchk.exe'''
                                   )

    def checkcubegen(self, event=None): # Checks for existence of formchk at specificed location
        if not os.path.isfile(self.cubegen.get()):
            messagebox.showerror(title="No such file",
                                   message='''
There isn't any file with this name.
                                   
Make sure you include the full path, filename and extension (if any).

For example (in Windows): C:\\Program Files\\Gaussian\\cubegen.exe'''
                                   )


    def ok(self, event=None): # Override
        # Remembers the settings
        # If they are different from before, they will be saved by 'preferences()'

        a=['start','end','numpoints','fwhm','excitation','temperature']
        for i in range(6):
            self.settings['ir_raman.%s'%(a[i])]=self.irraman[i].get()
        a=['start','end','numpoints','fwhm']
        for i in range(4):
            self.settings['find.text%d'%(i+1)]=self.find[i].get()
            self.settings['uvvis.%s'%(a[i])]=self.uvvis[i].get()
        self.settings['uvvis.sigma']=self.uvvis[4].get()
        a=['start','end','fwhm']
        for i in range(3):
            self.settings['mo.%s'%(a[i])]=self.mo[i].get()

        if not self.validate():
            self.initial_focus.focus_set() # put focus back
            return

        self.withdraw()
        self.update_idletasks()

        self.apply()

        self.cancel()
