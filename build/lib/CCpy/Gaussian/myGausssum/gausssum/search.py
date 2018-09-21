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

def Search(screen, logfilename, searchstring, case_sensitive):

    screen.write("Starting to search\n")

    searchterm = searchstring.split("%")

    inputfile = open(logfilename, "r")

    if not case_sensitive:
        # change all the searchterms to lowercase
        temp=[]
        for x in searchterm:
            temp.append(x.lower())
        searchterm=temp

    screenoutput = []
    for line in inputfile:
        searchline = line
        if not case_sensitive:
            searchline = line.lower()

        for x in searchterm:
            if searchline.find(x) >= 0:
                screenoutput.append(line)
                break
    inputfile.close()
    
    screen.write("".join(screenoutput))

    screen.write("Finished searching\n")
