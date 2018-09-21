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

from tkinter import *   # GUI stuff
import gausssum.gausssumgui

if __name__=="__main__":

    root = Tk()
    root.title("GaussSum")

    app = gausssum.gausssumgui.App(root,sys.argv)   # Set up the app...

    mainloop()  # ...and sit back

