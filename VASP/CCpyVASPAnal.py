#!/usr/bin/env python

import os, sys
from CCpy.VASP.VASPio import VASPOutput
from CCpy.Tools.CCpyTools import selectVASPInputs, selectVASPOutputs, selectInputs, linux_command

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [sub_option1] [sub_option2..]")
    print(""""--------------------------------------
[options]
d : Clear VASP output files (except of POSCAR, POTCAR, KPOINTS, INCAR)
    ex) CCpyVASPAnal.py d

0 : Check vasp job status.
    ex) CCpyVASPAnal.py 0

1 : Get final structures
    ex) CCpyVASPAnal.py 1

2 : Get final total energy list
    ex) CCpyVASPAnal.py 2 n  : sub option n -> do not show plot

3 : Energy & Cell volume convergence plot
    ex) CCpyVASPAnal.py 3 n  : sub option n -> do not show plot

4 : Generate cif file from POSCAR or CONTCAR
    ex) CCpyVASPAnal.py 4 POSCAR
"""
          )
    quit()

if sys.argv[1] == "d":
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
        files = [f for f in os.listdir("./")]
        for f in files:
            if f in inputfiles:
                pass
            else:
                linux_command("rm -rf "+f)
        os.chdir("../")

if sys.argv[1] == "0":
    dirs = selectVASPInputs("./", ask=False)
    VO = VASPOutput()
    VO.check_terminated(dirs=dirs)

elif sys.argv[1] == "1":
    inputs = selectVASPOutputs("./")
    for each_input in inputs:
        os.chdir(each_input)
        VO = VASPOutput()
        VO.getFinalStructure()
        os.chdir("../")

elif sys.argv[1] == "2":
    show_plot = True
    try:
        show_chk = sys.argv[2]
        if show_chk == "n":
            show_plot = False
    except:
        show_plot = True
    VO = VASPOutput()
    VO.get_energy_list(show_plot=show_plot)

elif sys.argv[1] == "3":
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

elif sys.argv[1] == "4":
    inputs = selectInputs(marker=["POSCAR"], directory_path="./")
    for each_input in inputs:
        VO = VASPOutput()
        VO.getFinalStructure(filename=each_input, path="./")

