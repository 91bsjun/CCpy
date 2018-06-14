#
# GaussSum (http://gausssum.sf.net)
# Copyright (C) 2006-2009 Noel O'Boyle <baoilleach@gmail.com>
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
import tkinter.filedialog     # For the Open File and Save File
import webbrowser
import tkinter.simpledialog
import traceback
import copy             # For deepcopy...until I find a better way of doing this
import configparser     # For writing the settings to an .ini file
import logging
import glob

from gausssum.cclib.parser import ADF, GAMESS, Gaussian, ccopen
from gausssum.preferencesbox import PreferencesPopupBox
from gausssum.aboutbox import AboutPopupBox
from gausssum.popanalysis import Popanalysis
from gausssum.electrontrans import ET
from gausssum.geoopt import GeoOpt
from gausssum.search import Search
from gausssum.vibfreq import Vibfreq
from gausssum.scf import SCF
from gausssum.utils import *
from gausssum.folder import folder

import os, sys
if hasattr(sys, "frozen"): # i.e. if using cx_Freeze
    installlocation = os.path.dirname(sys.executable)
else:
    import gausssum
    installlocation = gausssum.__path__[0]

class App:    # This sets up the GUI  

    def __init__(self,root,argv):

        self.root = root
        self.root.resizable(False,False)
        self.addmenubar()

        self.frame1=Frame(self.root)
        self.frame1.grid()
        frame2=Frame(self.root)
        frame2.grid(row=1)

        self.scrollbar=Scrollbar(frame2)
        self.scrollbar.pack(side=RIGHT,fill=Y)
        if sys.platform == "win32":
            self.txt=Text(frame2,font=("Courier New",8),yscrollcommand=self.scrollbar.set,width=90,height=21)
        else: # Avoid font problems (? I don't remember the reason for this)
            self.txt=Text(frame2,yscrollcommand=self.scrollbar.set,width=90,height=21)
        self.txt.pack(expand=1,fill=Y)
        self.scrollbar.config(command=self.txt.yview)

        self.screen=Redirect(self.txt.insert,self.root,[END,self.txt]) # Oh yeah! Could I have done it in a more roundabout way?
        self.screen.write("Support GaussSum by citing:\nN.M. O'Boyle, A.L. Tenderholt and K.M. Langner. J. Comp. Chem. 2008, 29, 839-845.\n")

        frame3=Frame(self.frame1,relief=GROOVE,borderwidth=3)
        frame3.pack(side=LEFT,anchor=W)
        
        self.middleframe=Frame(self.frame1,width=380,height=140)
        self.middleframe.pack(side=LEFT)
        # Thanks to the students from Ulm for the next line
        # which prevents self.middleframe resizing when you choose a different option    
        self.middleframe.pack_propagate(0)
        self.frame4=Frame(self.middleframe)
        self.frame4.pack(side=LEFT)


        self.script = StringVar()

        self.b3=Radiobutton(frame3, text="Search file", variable=self.script, value="FIND",command=self.option,state=DISABLED)
        self.b3.pack(anchor=W)
        self.b0=Radiobutton(frame3, text="Monitor SCF", variable=self.script, value="SCF",command=self.option,state=DISABLED)
        self.b0.pack(anchor=W)
        self.b1=Radiobutton(frame3, text="Monitor GeoOpt", variable=self.script, value="GEOOPT",command=self.option,state=DISABLED)
        self.b1.pack(anchor=W)
        self.b2=Radiobutton(frame3, text="Frequencies", variable=self.script, value="IR_RAMAN",command=self.option,state=DISABLED)
        self.b2.pack(anchor=W)
        self.b5=Radiobutton(frame3, text="Orbitals", variable=self.script, value="MO",command=self.option,state=DISABLED)
        self.b5.pack(anchor=W)
        self.b4=Radiobutton(frame3, text="Electronic transitions", variable=self.script, value="UVVIS",command=self.option,state=DISABLED)
        self.b4.pack(anchor=W)
##        self.b6=Radiobutton(frame3, text="EDDM.py", variable=self.script, value="EDDM", command=self.option,state=DISABLED)
##        self.b6.pack(anchor=W)
##        self.b7=Radiobutton(frame3, text="NMR.py (beta)", variable=self.script, value="NMR", command=self.option,state=DISABLED)
##        self.b7.pack(anchor=W)

        self.b3.select()

        self.frame5=Frame(self.frame1)
        self.frame5.pack(side=LEFT)
        self.photo = PhotoImage(file=os.path.join(installlocation,"mesh2.gif")) # Doesn't work if don't use self.
        Button(self.frame5,image=self.photo,command=self.runscript).pack(side=LEFT)
        self.root.bind("<Return>", self.runscript)

        x = (self.root.winfo_screenwidth()-652)//2 # The window is 652x480
        y = (self.root.winfo_screenheight()-580)//2
        self.root.geometry("652x480+"+str(x)+"+"+str(y)) # Get the window dead-centre

        self.error=ErrorCatcher()

# Read in the preferences from the preferences file.
# If it doesn't exist, create one using the defaults.
        self.readprefs()

        if len(argv)>1: # What to do if a filename is passed as an argument
            if os.path.isfile(argv[1]): # Does it exist?
                self.inputfilename=os.path.basename(argv[1])
                t=os.path.dirname(argv[1])
                if t:
                    os.chdir(t)
                # Create an instance of the parser
                self.logfile = ccopen(self.inputfilename)
                self.fileopenednow()
            else:
                self.screen.write(argv[1]+" does not exist or is not a valid filename\n")
                self.inputfilename=None
        else:
            self.inputfilename=None

### Read in the nmr standards file (if it exists)
##        self.nmrstandards = NMRstandards(self.settings['nmr.data'])

    def option(self):
# What to do when they choose a script
        s=self.script.get()
        self.frame4.destroy()
        self.frame4=Frame(self.middleframe,width=380)
        self.frame4.pack(side=LEFT)

        if s=="SCF" or s=="GEOOPT":
            Label(self.frame4,text="Leave out the first n points").pack(side=LEFT)
            self.numpts=Entry(self.frame4,width=3)
            self.numpts.pack(side=LEFT)
            self.numpts.insert(0,"0")
            self.reparse = IntVar()
            ckb = Checkbutton(self.frame4, text="Reparse first?",
                              variable=self.reparse)
            ckb.pack(side=LEFT)

        elif s=="IR_RAMAN":
            frame5=Frame(self.frame4)
            frame5.pack(side=TOP)
            frame6=Frame(self.frame4)
            frame6.pack(side=TOP)
            frame7=Frame(self.frame4)
            frame7.pack(side=TOP)
            frame8=Frame(self.frame4)
            frame8.pack(side=TOP)
            frame9=Frame(self.frame4)
            frame9.pack(side=TOP)

            Label(frame5,text="Start:").pack(side=LEFT)
            self.start=Entry(frame5,width=5)
            self.start.pack(side=LEFT)
            self.start.insert(0,self.settings['ir_raman.start'])
            Label(frame5,text="End:").pack(side=LEFT)
            self.end=Entry(frame5,width=5)
            self.end.pack(side=LEFT)
            self.end.insert(0,self.settings['ir_raman.end'])
            Label(frame5,text="Num pts:").pack(side=LEFT)
            self.numpts=Entry(frame5,width=6)
            self.numpts.pack(side=LEFT)
            self.numpts.insert(0,self.settings['ir_raman.numpoints'])
            Label(frame5,text="FWHM").pack(side=LEFT)
            self.FWHM=Entry(frame5,width=3)
            self.FWHM.pack(side=LEFT)
            self.FWHM.insert(0,self.settings['ir_raman.fwhm'])

            Label(frame6,text="Scaling factors:").pack(side=LEFT)
            self.scale=StringVar()
            r=Radiobutton(frame6,text="General",variable=self.scale,value="Gen")
            r.pack(side=LEFT)
            self.scalefactor=Entry(frame6,width=5)
            self.scalefactor.insert(0,'1.00')
            self.scalefactor.pack(side=LEFT)
            r2=Radiobutton(frame6,text="Individual",variable=self.scale,value="Indiv")
            r2.pack(side=LEFT)
            r.select()

            Label(frame7, text="The following are used to calculate Raman intensities:").pack()

            Label(frame8, text="Exc. wavelength (nm)").pack(side=LEFT)
            self.excitation = Entry(frame8, width=6)
            self.excitation.pack(side=LEFT)
            self.excitation.insert(0, self.settings['ir_raman.excitation'])
            Label(frame8, text="Temp. (K)").pack(side=LEFT)
            self.temperature = Entry(frame8,width=6)
            self.temperature.pack(side=LEFT)
            self.temperature.insert(0, self.settings['ir_raman.temperature'])
            if not hasattr(self.data, "vibramans"):
                self.excitation.configure(state=DISABLED)
                self.temperature.configure(state=DISABLED)

        elif s=="FIND":
            frame6=Frame(self.frame4)
            frame6.pack(side=LEFT)
            frame7=Frame(self.frame4)
            frame7.pack(side=LEFT)
            self.searchrad=StringVar()
            for i in range(4):
                t="find.text%d" % (i+1)
                Radiobutton(frame6, text=self.settings[t], variable=self.searchrad, value=self.settings[t]).grid(sticky=W)
            r=Radiobutton(frame6, text="Custom", variable=self.searchrad, value="Custom")
            r.grid(sticky=W)
            r.select()

            self.customsearch=Entry(frame6,width=15)
            self.customsearch.grid(row=4,column=1)
            self.customsearch.insert(END,"Enter phrase here")

            self.casesensitive=IntVar()
            casetick=Checkbutton(frame6,text="Case sensitive",variable=self.casesensitive)
            casetick.grid(row=4,column=2)

        elif s=="MO":
            frame8=Frame(self.frame4)
            frame6=Frame(self.frame4)
            frame7=Frame(self.frame4)

            frame8.pack()
            frame6.pack(side=TOP)
            frame7.pack(side=TOP)

            self.MOplot=IntVar()
            self.MODOS=Radiobutton(frame8, text="DOS", variable=self.MOplot, value=False)
            self.MODOS.pack(anchor=W)
            self.MOCOOP=Radiobutton(frame8, text="COOP", variable=self.MOplot, value=True)
            self.MOCOOP.pack(anchor=W)
            self.MODOS.select()

            Label(frame6,text="Start:").pack(side=LEFT)
            self.start=Entry(frame6,width=5)
            self.start.pack(side=LEFT)
            self.start.insert(0,self.settings['mo.start'])
            Label(frame6,text="End:").pack(side=LEFT)
            self.end=Entry(frame6,width=5)
            self.end.pack(side=LEFT)
            self.end.insert(0,self.settings['mo.end'])
            Label(frame6,text="FWHM:").pack(side=LEFT)
            self.FWHM=Entry(frame6,width=5)
            self.FWHM.pack(side=LEFT)
            self.FWHM.insert(0,self.settings['mo.fwhm'])

            self.makeorigin=IntVar()
            self.makeoriginbtn=Checkbutton(frame7,text="Create originorbs.txt?",variable=self.makeorigin)
            self.makeoriginbtn.pack(anchor=W)


        elif s=="UVVIS":
            frame7=Frame(self.frame4)
            frame7.pack(side=TOP)
            frame6=Frame(self.frame4)
            frame6.pack()

            self.UVplot=IntVar()
            self.UVbox=Radiobutton(frame7, text="UV-Visible", variable=self.UVplot, command=self.UVupdate, value=True)
            self.UVbox.pack(anchor=W)
            self.CDbox=Radiobutton(frame7, text="Circular dichroism", variable=self.UVplot, command=self.UVupdate, value=False)
            self.CDbox.pack(anchor=W)
            self.UVbox.select()

            Label(frame6,text="Start:").grid(row=0,column=0)
            Label(frame6,text="nm").grid(row=1,column=1)
            self.start=Entry(frame6,width=5)
            self.start.grid(row=0,column=1)
            self.start.insert(0,self.settings['uvvis.start'])
            Label(frame6,text="End:").grid(row=0,column=2)
            Label(frame6,text="nm").grid(row=1,column=3)
            self.end=Entry(frame6,width=5)
            self.end.grid(row=0,column=3)
            self.end.insert(0,self.settings['uvvis.end'])
            Label(frame6,text="Num pts:").grid(row=0,column=4)
            self.numpts=Entry(frame6,width=5)
            self.numpts.grid(row=0,column=5)
            self.numpts.insert(0,self.settings['uvvis.numpoints'])

            self.fwhmlabel=Label(frame6,text="FWHM",width=6)
            self.fwhmlabel.grid(row=0,column=6)
            self.fwhmunits=Label(frame6,text="1/cm")
            self.fwhmunits.grid(row=1,column=7)
            self.FWHM=Entry(frame6,width=5)
            self.FWHM.grid(row=0,column=7)
            self.FWHM.insert(0,self.settings['uvvis.fwhm'])

            self.eddm = IntVar()
            self.eddmbtn = Checkbutton(frame7, text="Create EDDM script?", variable=self.eddm)
            self.eddmbtn.pack(anchor=W)
            gaussdir = folder(self.screen, self.logfile.filename, create=False)
            if (len(glob.glob(os.path.join(gaussdir, "*.fck"))) == 0 and
                len(glob.glob(os.path.join(gaussdir, "*.fchk"))) == 0 and
                len(glob.glob(os.path.join(gaussdir, "*.chk"))) == 0):
                self.eddmbtn.configure(state=DISABLED)


    def createnmrpanel(self,parent):
# Sets up the Panel for the NMR options
        frame6=Frame(parent)
        frame6.grid(row=0,column=0)
        # Set up the Radiobuttons
        self.nmrrb = StringVar() # Extract, Comment, Listbox
        self.nmrrb1=Radiobutton(frame6, text="Just extract NMR data", variable=self.nmrrb, value="Extract",command=self.nmrrbcb)
        self.nmrrb1.pack(anchor=W)
        self.nmrrb2=Radiobutton(frame6, text="Take level of theory from comment", variable=self.nmrrb, value="Comment",command=self.nmrrbcb)
        self.nmrrb2.pack(anchor=W)
        self.nmrrb3=Radiobutton(frame6, text="Take level of theory from listbox", variable=self.nmrrb, value="Listbox",command=self.nmrrbcb)
        self.nmrrb3.pack(anchor=W)
        self.nmrrb1.select()

        # Set up the Listbox
        frame7=Frame(parent)
        frame7.grid(row=1,column=0)
        nmrscrlbar=Scrollbar(frame7,orient=VERTICAL)
        self.nmrlbx1=Listbox(frame7,yscrollcommand=nmrscrlbar.set,height=5)
        nmrscrlbar.config(command=self.nmrlbx1.yview)
        nmrscrlbar.pack(side=RIGHT,fill=Y)
        self.nmrlbx1.pack(side=LEFT,fill=BOTH,expand=1)
        for x in self.nmrstandards:
            self.nmrlbx1.insert(END,x['theory'])
        self.nmrlbx1.select_set(0)
        self.nmrlbx1.configure(state=DISABLED)

        # Set up more Radiobuttons
        frame8=Frame(parent)
        frame8.grid(row=0,column=1)
        self.nmrrb2 = StringVar() # Calculate, Standard
        self.nmrrb2a=Radiobutton(frame8, text="Calculate relative ppm", variable=self.nmrrb2, value="Calculate")
        self.nmrrb2a.pack(anchor=W)
        self.nmrrb2b=Radiobutton(frame8, text="Add new standard", variable=self.nmrrb2, value="Standard")
        self.nmrrb2b.pack(anchor=W)
        self.nmrrb2a.select()
        self.nmrrb2a.configure(state=DISABLED)
        self.nmrrb2b.configure(state=DISABLED)


    def nmrrbcb(self):
# What to do when the user chooses one of the NMR radiobuttons
# (NMR RadioButton CallBack)
        value=self.nmrrb.get()
        if value=="Listbox": # Configure the listbox
            self.nmrlbx1.configure(state=NORMAL)
        else:
            self.nmrlbx1.configure(state=DISABLED)

        if value=="Extract": # Configure everything
            self.nmrrb2a.configure(state=DISABLED)
            self.nmrrb2b.configure(state=DISABLED)
        elif value=="Listbox":
            self.nmrrb2a.configure(state=NORMAL)
            self.nmrrb2b.configure(state=DISABLED)
        else:
            self.nmrrb2a.configure(state=NORMAL)
            self.nmrrb2b.configure(state=NORMAL)

    def UVupdate(self):
# What to do when the user chooses UVVis or CD for UVVis.py
        if self.UVplot.get()==False:
            # Choose CD
            self.fwhmlabel.configure(text="sigma:")
            self.fwhmunits.configure(text="eV")
            self.FWHM.delete(0,END)
            self.FWHM.insert(0,self.settings['uvvis.sigma'])
        else:
            # Choose UVVis
            self.fwhmlabel.configure(text="FWHM:")
            self.fwhmunits.configure(text="1/cm")
            self.FWHM.delete(0,END)
            self.FWHM.insert(0,self.settings['uvvis.fwhm'])


    def runscript(self,event=None):
# What to do when the user clicks on the GaussSum logo
# to run the script
        if not self.inputfilename:
            self.screen.write("You need to open a log file first\n")
            return   # Do nothing if no file opened

        s = self.script.get()
        self.txt.delete(1.0, END)

        worked=False

        try:
            if s=="SCF":
                if self.reparse.get():
                    self.data = self.logfile.parse()
                worked = SCF(self.root,self.screen,self.data,int(self.numpts.get()))
            elif s=="GEOOPT":
                if self.reparse.get():
                    self.data = self.logfile.parse()
                worked = GeoOpt(self.root,self.screen,self.data,int(self.numpts.get()))
            elif s=="IR_RAMAN":
                worked = Vibfreq(self.root,self.screen,self.data,self.logfile.filename,int(self.start.get()),int(self.end.get()),int(self.numpts.get()),float(self.FWHM.get()),self.scale.get(),float(self.scalefactor.get()),float(self.excitation.get()),float(self.temperature.get()))
            elif s=="FIND":
                if self.searchrad.get()=="Custom":
                    worked = Search(self.screen,self.logfile.filename,self.customsearch.get(),self.casesensitive.get())
                else:
                    worked = Search(self.screen,self.logfile.filename,self.searchrad.get(),1)
            elif s=="MO":
                worked = Popanalysis(self.root,self.screen,self.data,self.logfile.filename,float(self.start.get()),float(self.end.get()),self.MOplot.get(),float(self.FWHM.get()),self.makeorigin.get())
            elif s=="UVVIS":
                worked = ET(self.root,self.screen,self.data,self.logfile.filename,int(self.start.get()),int(self.end.get()),int(self.numpts.get()),float(self.FWHM.get()),self.UVplot.get(),self.eddm.get())
##            elif s=="NMR":
##                listboxvalue=self.nmrlbx1.curselection()
##                try: # Works for both list of strings and list of ints (see Tkinter documentation for details)
##                    listboxvalue=map(int,listboxvalue)
##                except ValueError: pass
##                if len(listboxvalue)>0:
##                    selected = self.nmrlbx1.get(listboxvalue[0])
##                else:
##                    selected = []
##                worked=GaussSum.NMR.NMR(self.screen,self.logfile,self.nmrrb.get(),
##                                        selected,self.nmrrb2.get(),self.nmrstandards)
        except:
            traceback.print_exc(file=self.error) # Catch errors from the python scripts (user GIGO errors, of course!)
            messagebox.showerror(title="The script is complaining...",message=self.error.log)
            self.error.clear()
        self.root.update()


    def addmenubar(self):
        """Set up the menubar."""
        menu = Menu(self.root)
        self.root.config(menu=menu)

        filemenu = Menu(menu)
        menu.add_cascade(label="File", underline=0, menu=filemenu)
        filemenu.add_command(label="Open...", underline=0, command=self.fileopen)
        filemenu.add_command(label="Settings...",underline=0, command=self.preferences)
        filemenu.add_separator()
        filemenu.add_command(label="Exit", underline=1, command=self.fileexit)

        viewmenu=Menu(menu)
        menu.add_cascade(label="View", underline=0, menu=viewmenu)
        viewmenu.add_command(label="Error messages", underline=1, command=self.showerrors)

        helpmenu = Menu(menu)
        menu.add_cascade(label="Help", underline=0, menu=helpmenu)
        helpmenu.add_command(label="Documentation",underline=0, command=self.webdocs)
        helpmenu.add_separator()
        helpmenu.add_command(label="About...", underline=0, command=self.aboutdialog)

    def aboutdialog(self,event=None): # Soaks up event if provided
        d=AboutPopupBox(self.root,title="About GaussSum")

    def showerrors(self):
        self.screen.write("Log of error messages\n%s\n%s\n" % ('*'*20,self.error.longlog) )

    def fileexit(self):
        self.root.destroy()

    def fileopen(self):
        if self.inputfilename!=None:
            mydir=os.path.dirname(self.inputfilename)
        else:
            mydir="."

        inputfilename = tkinter.filedialog.askopenfilename(
                filetypes=[
                    ("All files",".*"),
                    ("Output Files",".out"),
                    ("Log Files",".log"),
                    ("ADF output",".adfout")
                    ],
                initialdir=mydir
                )

        if inputfilename!="":
            self.inputfilename=os.path.basename(inputfilename)
            os.chdir(os.path.dirname(inputfilename))
            # Create an instance of the parser
            if hasattr(self, "logfile") and self.logfile!=None:
                self.logfile.logger.removeHandler(self.logfile.logger.handlers[0])
            self.logfile = ccopen(self.inputfilename)
            self.fileopenednow()

    def fileopenednow(self):
        """What to do once a file is opened."""

        if self.logfile==None:
            messagebox.showerror(
                title="Not a valid log file",
                message=("cclib does not recognise %s as a valid logfile.\n\n"
                        "If the file *is* valid, please send an email to "
                        "gausssum-help@lists.sourceforge.net\n describing the problem." % self.inputfilename)
                )
            for button in [self.b0, self.b1, self.b2, self.b3, self.b4, self.b5]:
                button.configure(state=DISABLED)
            return

        # Prevent the logger writing to stdout; instead, write to screen
        self.logfile.logger.removeHandler(self.logfile.logger.handlers[0])
        newhandler = logging.StreamHandler(self.screen)
        newhandler.setFormatter(logging.Formatter("[%(name)s %(levelname)s] %(message)s"))
        self.logfile.logger.addHandler(newhandler)

        try:
            self.data = self.logfile.parse()
        except:
            messagebox.showerror(
                title="Problems parsing the logfile",
                message=("cclib has problems parsing %s.\n\n"
                        "If you think it shouldn't, please send an email to "
                        "gausssum-help@lists.sourceforge.net\n describing the problem." % self.inputfilename)
                )
            for button in [self.b0, self.b1, self.b2, self.b3, self.b4, self.b5]:
                button.configure(state=DISABLED)
            return

        self.screen.write("Opened and parsed %s.\n" % self.inputfilename)
        has = lambda x: hasattr(self.data, x)
        self.b3.configure(state=NORMAL) # Search
        if has("scftargets") and has("scfvalues"):
            self.b0.configure(state=NORMAL)
        else:
            self.b0.configure(state=DISABLED)
        if has("geotargets") and has("geovalues"):
            self.b1.configure(state=NORMAL)
        else:
            self.b1.configure(state=DISABLED)
        if has("vibfreqs") and (has("vibirs") or has("vibramans")):
            self.b2.configure(state=NORMAL)
        else:
            self.b2.configure(state=DISABLED)
        if has("etenergies") and has("etoscs"):
            self.b4.configure(state=NORMAL)
        else:
            self.b4.configure(state=DISABLED)
        self.b5.configure(state=NORMAL) # Orbitals
##        self.b6.configure(state=NORMAL) # EDDM
##        self.b7.configure(state=NORMAL) # NMR
        self.b3.invoke() # Click on Find.py
        self.root.title("GaussSum - "+self.inputfilename)

    def preferences(self):
        # The Preferences Dialog Box
        oldsettings=copy.deepcopy(self.settings) # Remember the old settings
        d=PreferencesPopupBox(self.root,self.settings,title="Settings")
        if self.settings!=oldsettings: # if there are any changes
            self.saveprefs()

    def getsettingsfile(self):
        # Check for GaussSum.ini in $APPDATA/GaussSum2.2 (XP)
        #                        or $HOME/.GaussSum2.2 (Linux)
        if sys.platform == "win32":
            settingsfile = os.path.join(os.getenv("APPDATA"),"GaussSum2.2","GaussSum.ini")
        else:
            settingsfile = os.path.join(os.getenv("HOME"), ".GaussSum2.2","GaussSum.ini")
        return settingsfile

    def saveprefs(self):
        # Save the settings
        settingsfile = self.getsettingsfile()
        writeoutconfigfile(self.settings,settingsfile)

    def readprefs(self):
        # The default settings are overwritten by any existing stored
        # settings.

        self.settings = {
                           'find.text1':'SCF Done',
                           'find.text2':'k 501%k502',
                           'find.text3':'imaginary',
                           'find.text4':'Framework',
                           'ir_raman.start':'0',
                           'ir_raman.end':'4000',
                           'ir_raman.numpoints':'500',
                           'ir_raman.fwhm':'10',
                           'ir_raman.excitation':'785',
                           'ir_raman.temperature':'293.15',
                           'mo.start':'-20',
                           'mo.end':'0',
                           'mo.fwhm':'0.3',
                           'uvvis.start':'300',
                           'uvvis.end':'800',
                           'uvvis.numpoints':'500',
                           'uvvis.fwhm':'3000',
                           'uvvis.sigma':'0.2',
                           }

        settingsfile = self.getsettingsfile()
        if os.path.isfile(settingsfile): # Check for settings file
            # Found it! - so read it in.
            self.settings.update(readinconfigfile(settingsfile))

        else: # Initialise the settings file (and directory)
            settingsdir = os.path.dirname(settingsfile)
            if not os.path.isdir(settingsdir):
                os.mkdir(settingsdir)

            self.saveprefs() # Save the inital settings file

    def webdocs(self):
        if hasattr(sys, "frozen"): # Using cx_Freeze
            webbrowser.open(os.path.join(installlocation, "Docs", "index.html"))
        else:
            webbrowser.open(os.path.join(installlocation, "..", "Docs", "index.html"))


