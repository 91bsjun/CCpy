#!/usr/bin/env python
# -*- coding: cp1252 -*-
#
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

#from tkinter import *   # GUI stuff
import bsjunGausssum
a="C:\\test.com.log"
b=[]

class startGauss:
    def __init__(self,inputfile,outlist):
        self.inputfile = a
        self.outlist = b
        #app = mygui.App(inputfile,outlist)
        #app.fileopen()
        #self.outlist = app.fileopenednow()
        #print(self.outlist)
    def getoutresult(self):
        app = bsjunGausssum.App(a,b)
        app.fileopen()
        result = app.fileopenednow()
        return result
    
