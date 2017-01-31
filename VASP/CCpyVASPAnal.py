#!/usr/local/bin/python2.7

import os, sys
from CCpy.VASP.VASPio import VASPOutput
from CCpy.Tools.CCpyTools import selectVASPOutputs, linux_command

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [sub_option1] [sub_option2..]")
    print(""""--------------------------------------
[options]
0 : Clear VASP output files (except of POSCAR, POTCAR, KPOINTS, INCAR)
1 : Get final structures (from output directories)
2 : Energy & Cell volume convergence plot
    ex) CCpyVASPAnal.py 2 n : sub option n -> do not show plot
3 : Generate cif file from POSCAR or CONTCAR
    ex) CCpyVASPAnal.py 3 POSCAR"""
          )
    quit()

if sys.argv[1] == "0":
    inputfiles = ["INCAR","POSCAR","POTCAR","KPOINTS"]
    inputs = selectVASPOutputs("./")
    for each_input in inputs:
        print(each_input)
    yn = raw_input("Are you sure to remove these output files? (y/n)")
    if yn == "y" or yn == "yes":
        pass
    else:
        quit()
    for each_input in inputs:
        os.chdir(each_input)
        flies = [f for f in os.listdir("./")]
        for f in files:
            if f in inputfiles:
                pass
            else:
                linux_command("rm -rf "+f)
        os.chdir("../")

if sys.argv[1] == "1":
    inputs = selectVASPOutputs("./")
    for each_input in inputs:
        os.chdir(each_input)
        VO = VASPOutput()
        VO.getFinalStructure()
        os.chdir("../")

elif sys.argv[1] == "2":
    # -- Check show plot
    show_plot = True
    try:
        show_chk = sys.argv[2]
        if show_chk == "n":
            show_plot = False
    except:
        show_plot = True

    inputs = selectVASPOutputs("./")
    for each_input in inputs:
        os.chdir(each_input)
        print(each_input)
        VO = VASPOutput()
        VO.getConvergence(show_plot=show_plot)
        os.chdir("../")

elif sys.argv[1] == "3":
    try:
        filename = sys.argv[2]
    except:
        filename = raw_input("POSCAR? CONTCAR?")

    VO = VASPOutput()
    VO.getFinalStructure(filename=filename)
