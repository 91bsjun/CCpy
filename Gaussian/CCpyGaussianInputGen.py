#!/usr/local/bin/python2.7

import os, sys
from subprocess import call as shl

from CCpy.Gaussian.Gaussianio import GaussianInput as GI
from CCpy.Gaussian.Gaussianio import GaussianOutput as GO
from CCpy.Tools.CCpyStructure import NonPeriodicCoordinates as npc
from CCpy.Tools.CCpyTools import selectInputs

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [suboption]")
    print('''--------------------------------------
[1] : From structure files (*.xyz, *.car, *.xsd)
[2] : From log files (*.log)
[3] : From checkpoint files (in ./check directory) (*.chk)
[4] : Edit com files (*.com)
[suboption]
a : no check files, calculate all inputs'''
          )
    quit()


## Input options setting
nproc = 24
mem = 64
functional = "B3LYP"
basis = "6-31G"
options = "pop=full iop(3/33=1,3/36=-1) SP"
chg = 0
multi = 1
options2 = ""

ask = True
if "a" in sys.argv:
    ask = False

if sys.argv[1] == "1":
    input_marker = [".xsd", ".xyz", "car"]
    inputs = selectInputs(input_marker, "./", ask=ask)
elif sys.argv[1] == "2":
    input_marker = [".log"]
    inputs = selectInputs(input_marker, "./")
elif sys.argv[1] == "3":
    input_marker = [".chk"]
    inputs = selectInputs(input_marker, "./check")
elif sys.argv[1] == "4":
    input_marker = [".com"]
    inputs = selectInputs(input_marker, "./")


myGI = GI(nproc, mem, functional, basis, options, chg, multi, options2)
try:
    os.mkdir("check")
except:
    pass
for each_input in inputs:
    if sys.argv[1] == "1":
        myGI.newCalc(each_input)
    elif sys.argv[1] == "2":
        mygo = GO(each_input)
        n_atoms, atoms, coords = mygo.getFinalStructure()
        name = each_input.replace(".log","")
        if name+".xyz" in os.listdir("./"):
            name = name+"[2]"

        mynpc = npc("tmp")
        mynpc.name, mynpc.n_atoms, mynpc.atoms, mynpc.coords = name, n_atoms, atoms, coords
        mynpc.to_xyzFile()

        myGI.newCalc(name+".xyz")
        shl("rm -rf "+name+".xyz", shell=True)
        
    elif sys.argv[1] == "3":
        newname = each_input.replace(".chk","_a")
        myGI.additionalCalc(each_input, comname=newname)

    elif sys.argv[1] == "4":
        f = open(each_input, "r")
        lines = f.readlines()
        f. close()

        f.write(each_input, "w")
        for line in lines:
            if "%nproc=" in line:
                f.write("%nproc="+str(nproc)+"\n")
            elif "%mem=" in line:
                f.write("%mem="+str(mem)+"Gb\n")
            elif "#p" in line:
                f.write("#p "+functional+"/"+basis+" gfinput gfprint SCF(maxcycle=512,conver=6) "+options+"\n")
            else:
                f.write(line)
        f.close()
        print(each_input)
            

