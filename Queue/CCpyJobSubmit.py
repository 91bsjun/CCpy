#!/usr/bin/env python

# This script is for queue submitting.

import os, sys
from subprocess import call as shl

from CCpy.Queue.CCpyJobControl import JobSubmit as JS
from CCpy.Tools.CCpyTools import selectInputs, selectVASPInputs
from CCpy.Tools.CCpyTools import linux_command as lc
from CCpy.Tools.CCpyTools import get_ip


# -- version chk
version = sys.version
if version[0] == '3':
    raw_input = input

# -- Check node00
ip = get_ip()
if ip == "166.104.249.31":
    print("DO AT NODE00 !!")
    quit()



def gaussian(queue=None, divided=1):
    # --- COLLECT INPUT FILES
    input_marker = [".com"]
    inputs = selectInputs(input_marker, "./", ask=ask)

    # --- SUBMIT QUEUE
    for each_input in inputs:
        myJS = JS(each_input, queue, divided)
        myJS.gaussian()

def vasp(queue=None, divided=1):
    # --- Collect VASP inputs
    band = False
    if "-band" in sys.argv:
        band = True
        inputs = selectVASPInputs("./", ask=ask, band=True)
    else:
        inputs = selectVASPInputs("./", ask=ask)

    # --- SUBMIT QUEUE
    pwd = os.getcwd()
    for each_input in inputs:
        dirpath = pwd + "/" + each_input
        if band:
            dirpath += "/Band-DOS"
        myJS = JS(each_input, queue, divided)
        myJS.vasp(band=band, dirpath=dirpath)

def vasp_batch(queue=None, divided=1):
    dirs = []
    # --- Collect VASP inputs
    band = False
    if "-band" in sys.argv:
        band = True
        inputs = selectVASPInputs("./", ask=ask, band=True)
    else:
        inputs = selectVASPInputs("./", ask=ask)

    # --- SUBMIT QUEUE
    pwd = os.getcwd()
    for each_input in inputs:
        dirpath = pwd + "/" + each_input
        if band:
            dirpath += "/Band-DOS"
        dirs.append(dirpath)
    myJS = JS(each_input, queue, divided)
    myJS.vasp_batch(band=band, dirs=dirs)


def qchem(queue=None, divided=1):
    # --- Collect inputs
    input_marker = [".in"]
    inputs = selectInputs(input_marker, "./", ask=ask)

    # --- SUBMIT QUEUE
    for each_input in inputs:
        myJS = JS(each_input, queue, divided)
        myJS.qchem()

def atk(queue=None, divided=1):
    # --- COLLECT INPUT FILES
    input_marker = [".py"]
    inputs = selectInputs(input_marker, "./", ask=ask)

    # --- SUBMIT QUEUE
    for each_input in inputs:
        myJS = JS(each_input, queue, divided)
        myJS.ATK()


def lammps(queue=None, divided=1):
    # --- COLLECT INPUT FILES
    input_marker = ["in."]
    inputs = selectInputs(input_marker, "./", ask=ask)

    # --- SUBMIT QUEUE
    for each_input in inputs:
        myJS = JS(each_input, queue, divided)
        myJS.lammps()


def atat(queue=None, divided=1):
    all_inputs = [int(d) for d in os.listdir("./") if os.path.isdir(d) if
                  "str.out" in os.listdir(d) and "wait" in os.listdir(d)]
    all_inputs.sort()

    # fitsvl calc
    if "f" in sys.argv:
        inputs = []
        f = open("strname.in", "r")
        dirs = f.read()
        f.close()
        dirs = dirs.split()

        f = open("fitsvl_list", "w")
        pwd = os.getcwd()
        # strname.in dirs
        for d in dirs:
            os.chdir(d)
            sub_d = [a for a in os.listdir("./") if os.path.isdir(a) if "vol_" in a]
            # vol_0, vol_1.5 ...
            for sd in sub_d:
                os.chdir(sd)
                ssub_d = [a for a in os.listdir("./") if os.path.isdir(a) if "p+" in a]
                # p+0.2_8.7_0 ...
                for ssd in ssub_d:
                    os.chdir(ssd)
                    f.write(os.getcwd())
                    f.write("\n")
                    inputs.append(os.getcwd())
                    os.chdir("../")
                os.chdir("../")
            os.chdir("../")
        f.close()

    # maps calc
    else:
        print("0 : All files")
        for i in range(len(all_inputs)):
            print(str(i + 1) + " : " + str(all_inputs[i]) + " , running or calculated.")
        get_num = raw_input("Choose file : ")
        all_inputs = [str(d) for d in all_inputs]
        try:
            if get_num == "0":
                inputs = all_inputs
            else:
                inputs = []
                get_num = get_num.split(",")  # 1-4,6-10,11,12
                for i in get_num:
                    if "-" in i:
                        r = i.split("-")
                        for j in range(int(r[0]), int(r[1]) + 1):
                            inputs.append(all_inputs[j - 1])
                    else:
                        i = int(i)
                        inputs.append(all_inputs[i - 1])
        except:
            print("Unvalid input type.")
            print("ex : 1-3,5-10,11,12,13")
            quit()

    ### SUBMIT QUEUE #############################################
    pwd = os.getcwd()
    for each_input in inputs:
        os.chdir(pwd)
        myJS = JS(each_input, queue, divided)
        myJS.atat()

def pbs_runner(queue=None, divided=1):
    # --- COLLECT INPUT FILES
    input_marker = [".py"]
    inputs = selectInputs(input_marker, "./", ask=ask)

    # --- SUBMIT QUEUE
    for each_input in inputs:
        myJS = JS(each_input, queue, divided)
        myJS.pbs_runner()

if __name__=="__main__":
    try:
        chk = sys.argv[1]
        chk = sys.argv[2]
    except:
        print("\n--------------------------------------------------------------------------------------")
        print("     How to use : " + sys.argv[0].split("/")[-1] + " [Option 1] [Option 2] [Suboptions]")
        print('''--------------------------------------------------------------------------------------
< Option 1. > Software
    1  : Gaussian09
    2  : VASP
    3  : ATK
    4  : Q-chem
    6  : ATAT
    7  : LAMMPS
    8  : PBS job display

< Option 2. > Queue
    xeon1, xeon2, xeon3, ...

[Suboptions] (optional)
    -d=[integer]    : which divide CPU numbers and memories
                      ex) CCpyJobSubmit.py 2 xeon2 -d=2
                      --> will use half of CPU in xeon2

    -band           : when perform VASP band calculation
                      ex) CCpyJobSubmit.py 2 xeon5 -band
                      ex) CCpyJobSubmit.py 2 xeon5 -band -d=2
                      ex) CCpyJobSubmit.py 2 xeon5 -d=3 -band

    -batch          : run multiple jobs in a single queue
                      (Only VASP supported yet)
                      ex) CCpyJobSubmit.py 2 xeon5 -batch
                      ex) CCpyJobSubmit.py 2 xeon5 -batch -band

    -s              : use scratch directory when run batch jobs
                      (When you calculate quick hundreds jobs, to reduce load in master node)
                      (Only VASP supported yet)

    -a : no check files, calculate all inputs

    '''
              )
        quit()


    # --- Queue name check
    queues = ["xeon1", "xeon2", "xeon3", "xeon4", "xeon5", "I5", "aws"]
    try:
        queue = sys.argv[2]
    except:
        queue = raw_input("Queue (xeon1, xeon2, ...) : ")
    if queue not in queues:
        print("Unvalid queue name")
        quit()


    # --- Suboption parsing
    # -- Select all inputs automatically
    divided = 1
    scratch = False
    ask = True
    for s in sys.argv:
        if "-d" in s:
            divided = int(s.split("=")[1])
        if "-s" in s:
            scratch = True
        if "-a" in s:
            ask = False

    ## ------ GAUSSIAN
    if sys.argv[1] == "1":
        gaussian(queue=queue, divided=divided)

    ## ------ VASP
    elif sys.argv[1] == "2":
        if "-batch" in sys.argv:
            vasp_batch(queue=queue, divided=divided, scratch=scratch)
        else:
            vasp(queue=queue, divided=divided)

    ## ------ ATK
    elif sys.argv[1] == "3":
        atk(queue=queue, divided=divided)

    ## ------ Q-chem
    elif sys.argv[1] == "4":
        qchem(queue=queue, divided=divided)


    ## ------ ATAT
    elif sys.argv[1] == "6":
        atat(queue=queue, divided=divided)

    ## ------ ATAT
    elif sys.argv[1] == "7":
        lammps(queue=queue, divided=divided)

    ## ------ PBS JOBS DISPLAYER
    elif sys.argv[1] == "8":
        pbs_runner(queue=queue, divided=divided)
