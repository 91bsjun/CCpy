#!/usr/bin/env python

import os, sys
import time
import pandas as pd
from CCpy.VASP.VASPio import VASPOutput
from CCpy.Tools.CCpyTools import selectVASPInputs, selectVASPOutputs, selectInputs, linux_command

version = sys.version
if version[0] == '3':
    raw_input = input

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [sub_option1] [sub_option2..]")
    print(""""--------------------------------------
[suboptions]
-sub : deep in subdirectories

[options]
-d : Clear VASP output files (except of POSCAR, POTCAR, KPOINTS, INCAR)
    ex) CCpyVASPAnal.py d

 0 : Check vasp job status.
    ex) CCpyVASPAnal.py 0

 1 : Get final structures
    ex) CCpyVASPAnal.py 1
    ex) CCpyVASPAnal.py 1 -poscar    -> make cif files using POSCAR

 2 : Get final total energy list
    ex) CCpyVASPAnal.py 2 n  : sub option n -> do not show plot
    ex) CCpyVASPAnal.py 2 n -st  : sub option '-st' -> sort by total energy
    ex) CCpyVASPAnal.py 2 n -sa  : sub option '-st' -> sort by energy/atom

 3 : Energy & Cell volume convergence plot
    ex) CCpyVASPAnal.py 3 n  : sub option n -> do not show plot

 4 : Generate cif file from POSCAR or CONTCAR
    ex) CCpyVASPAnal.py 4

 -elastic : Analyze mechanical properties

-e : Handling errors listed '01_unconverged_jobs.csv' file 
     based on Materials Project's custodian module.

-zip : zip unnecessary files (zip CHGCAR DOSCAR PROCAR XDATCAR vasprun.xml)
    ex) CCpyVASPAnal.py -zip      -> user choose directories
    ex) CCpyVASPAnal.py -zip -sub -> user choose directories (include subdirectories)
    ex) CCpyVASPAnal.py -zip -auto        -> automatically detect converged jobs
    ex) CCpyVASPAnal.py -zip -auto -sub   ->               (include subdirectories)
    ex) CCpyVASPAnal.py -zip -bg          -> detect and zip converged jobs every 30 minutes
    ex) CCpyVASPAnal.py -zip -bg -sub     ->               (include sub directories)
    ex) CCpyVaspAnal.py -zip -m           -> remove CHG* DOSCAR* PROCAR*
   
    
"""
          )
    quit()

sub = False
additional_dir = None
if "-sub" in sys.argv:
    sub = True
for arg in sys.argv:
    if "-dir" in arg:
        additional_dir = arg.split("=")[1]

if sys.argv[1] == "-d":
    inputfiles = ["INCAR","POSCAR","POTCAR","KPOINTS"]
    inputs = selectVASPOutputs("./", additional_dir=additional_dir)
    for each_input in inputs:
        print(each_input)
    yn = raw_input("Are you sure to remove these output files? (y/n)")
    if yn == "y" or yn == "yes":
        pass
    else:
        quit()
    pwd = os.getcwd()
    for each_input in inputs:
        os.chdir(each_input)
        files = [f for f in os.listdir("./")]
        for f in files:
            if f in inputfiles:
                pass
            else:
                linux_command("rm -rf "+f)
        os.chdir(pwd)

if sys.argv[1] == "0":
    dirs = selectVASPOutputs("./", ask=False, sub=sub, additional_dir=additional_dir)
    VO = VASPOutput()
    VO.check_terminated(dirs=dirs)

elif sys.argv[1] == "1":
    inputs = selectVASPOutputs("./", sub=sub, additional_dir=additional_dir)
    pwd = os.getcwd()
    for each_input in inputs:
        #dirname = each_input.split('/')[-1]
        dirname = each_input.replace('./', '').replace('/', "_")
        #if additional_dir:
        #    dirname = "%s_%s" % (each_input.split("/")[0], each_input.split("/")[1])
        os.chdir(each_input)
        VO = VASPOutput()
        if "-poscar" in sys.argv:
            target_name = dirname + "_poscar.cif"
            VO.getFinalStructure(filename="POSCAR", target_name=target_name, path=pwd+"/")
        elif "CONTCAR" in os.listdir("./") and os.path.getsize("CONTCAR") != 0:
            target_name = dirname + "_contcar.cif"
            VO.getFinalStructure(target_name=target_name, path=pwd+"/")
        else:
            print(each_input + ": CONTCAR is empty!")
        os.chdir(pwd)

elif sys.argv[1] == "2":
    show_plot = True
    sort = False
    try:
        show_chk = sys.argv[2]
        if show_chk == "n":
            show_plot = False
    except:
        show_plot = True
    dirs = selectVASPOutputs("./", ask=False, sub=sub, additional_dir=additional_dir)
    # dirs = [d for d in dirs if "OUTCAR" in os.listdir(d)]    # duplicated work
    VO = VASPOutput()
    if "-st" in sys.argv:
        sort = "tot"
    elif "-sa" in sys.argv:
        sort = "atom"
    VO.get_energy_list(show_plot=show_plot, dirs=dirs, sort=sort)

elif sys.argv[1] == "3":
    # -- Check show plot
    show_plot = True
    try:
        show_chk = sys.argv[2]
        if show_chk == "n":
            show_plot = False
    except:
        show_plot = True

    inputs = selectVASPOutputs("./", additional_dir=additional_dir)
    for each_input in inputs:
        os.chdir(each_input)
        print(each_input)
        VO = VASPOutput()
        VO.getConvergence(show_plot=show_plot)
        os.chdir("../")

elif sys.argv[1] == "4":
    inputs = selectInputs(marker=["POSCAR", "CONTCAR"], directory_path="./")
    for each_input in inputs:
        VO = VASPOutput()
        VO.getFinalStructure(filename=each_input, path="./")

elif sys.argv[1] == "-elastic":
    inputs = selectVASPOutputs("./", sub=sub, additional_dir=additional_dir)
    VO = VASPOutput()
    VO.get_mechanical_properties(dirs=inputs)

elif sys.argv[1] == "-zip":
    """
    zipped status add
    """
    minimize = False
    if '-m' in sys.argv:
        minimize = True
    VO = VASPOutput()
    if "-bg" in sys.argv:
        print("Start loop..")
        cnt = 1
        while True:
            print("\nloop " + str(cnt))
            if sub:
                linux_command("CCpyVASPAnal.py -zip -auto -sub")
            else:
                linux_command("CCpyVASPAnal.py -zip -auto")
            print("Rest 30 minutes..")
            time.sleep(1800)
            cnt+=1
    elif "-auto" in sys.argv:
        if sub:
            print("# ----------- Parsing -------------- #")
            linux_command("CCpyVASPAnal.py 0 -sub")
        else:
            print("\n\n# ----------- Parsing -------------- #")
            linux_command("CCpyVASPAnal.py 0")
        df = pd.read_csv(".00_job_status.csv")
        df[['  Converged', '  Zipped']] = df[['  Converged', '  Zipped']].astype(str)
        df = df[(df['  Converged'] == 'True')]
        df = df[(df['  Zipped'] == 'False')]
        dirs = df['Directory'].tolist()
        if len(dirs) == 0:
            print("Cannot find unzipped VASP job.")
        else:
            print("\n\n# ----------- Zipping -------------- #")
            VO.vasp_zip(dirs, minimize=minimize)
    else:
        dirs = selectVASPOutputs("./", sub=sub)
        print("\n\n# ----------- Zipping -------------- #")
        VO.vasp_zip(dirs, minimize=minimize)

        
elif sys.argv[1] == "-e":
    """
    Handling errors based on Materials Project's custodian module.
    listed 01_unconverged_jobs.csv file
    """
    if "01_unconverged_jobs.csv" not in os.listdir("./"):
        print("\n01_unconverged_jobs.csv was not found in this directory.")
        print("Create it using: CCpyVASPAnal.py 0")
        quit()
    df = pd.read_csv("01_unconverged_jobs.csv")
    try:
        df = df.drop('Unnamed: 0', 1)
    except:
        pass
    print("\n* Handle error or unconverged job(s)")
    print("Unconverged job list in 01_unconverged_jobs.csv")

    dirs = df['Directory'].tolist()
    outputs = selectVASPOutputs("./", dir_list=dirs, additional_dir=additional_dir)

    VO = VASPOutput()
    VO.vasp_error_handle(outputs)









