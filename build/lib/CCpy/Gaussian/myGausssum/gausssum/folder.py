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

# Creates a new output folder if it doesn't already exist
# The folder is created in the same directory as the input file

import os

def folder(screen, logfilename, create=True):
     # Create the output directory if necessary
     
    logdir=os.path.dirname(logfilename)
    logname=os.path.basename(logfilename)
    gaussdir=os.path.join(logdir,"GaussSum3.0")
    if create:
        if not os.path.isdir(gaussdir):
            screen.write("Creating new output folder\n")
            os.mkdir(gaussdir)
        else:
            screen.write("Using old output folder\n")
    return gaussdir

  
    
