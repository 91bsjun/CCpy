#!/usr/local/bin/python2.7
"""
This is a package for ROE(Reorganization energy) calculation using Gaussian09
Input : a single molecular structure file (*.xyz, *.car, *.xsd)
"""

import os, sys

from CCpy.Gaussian.Gaussianio import GaussianInput as GI
from CCpy.Tools.CCpyTools import selectInputs,linux_command
from CCpy.Tools.CCpyTools import linux_command as lc

try:
    filename = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option]")
    print('''--------------------------------------
[option]
1 : Step   I. Optimize -1,0,+1 state
2 : Step  II. Calculate single point energy of each structure (-1 state of neutral, ...)
3 : Step III. Calculate Reorganization energy
[suboption]
a : no check files, calculate all inputs'''
          )
    quit()

try:
    step = sys.argv[2]
except:
    print("Assign step. (ex" + sys.argv[0].split("/")[-1] + "benzene.xyz 1")
    quit()

funtional = "B3LYP"
basis = "6-31G*"
opt = "gfinput gfprint SCF(maxcycle=512,conver=6) opt=gediis"
sp = "gfinput gfprint SCF(maxcycle=512,conver=6) sp"

# --------- Step 1. optimize -1,0,+1 state --------- #
if step == 1:
    name = filename.split(".")[0]
    try:
        os.mkdir(name)
    except:
        print(name + " directory is already exist. Remove it and try again")
        quit()

    os.chdir(name)

    # neutral
    os.mkdir("neutral")
    os.chdir("neutral")
    lc("cp ../" + filename + "./")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=0, multi=1, options=opt, options2="")
    myGI.newCalc(each_ingput)
    os.chdir("../")

    # anaion
    os.mkdir("anion")
    os.chdir("anion")
    lc("cp ../" + each_ingput + "./")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=-1, multi=3, options=opt, options2="")
    myGI.newCalc(each_ingput)
    os.chdir("../")

    # cation
    os.mkdir("cation")
    os.chdir("cation")
    lc("cp ../" + each_ingput + "./")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=-1, multi=3, options=opt, options2="")
    myGI.newCalc(each_ingput)
    os.chdir("../")

# --------- Step 2. Calculate single point state --------- #
elif step == 2:
    name = filename.split(".")[0]
    if name not in os.listdir("./"):
        print("You might have not performed 'step 1' process.")
        quit()

    os.chdir(name)

    # neutral structure
    os.chdir("neutral")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=-1, multi=3, options=sp, options2="")
    myGI.additionalCalc(name+".check", comname=name+"_neut_anion.com")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=1, multi=3, options=sp, options2="")
    myGI.additionalCalc(name + ".check", comname=name + "_neut_cation.com")
    os.chdir("../")

    # anion structure
    os.chdir("anion")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=0, multi=1, options=sp, options2="")
    myGI.additionalCalc(name + ".check", comname=name + "_anion_neut.com")
    os.chdir("../")

    # cation structure
    os.chdir("cation")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=0, multi=1, options=sp, options2="")
    myGI.additionalCalc(name + ".check", comname=name + "_cation_neut.com")
    os.chdir("../")
