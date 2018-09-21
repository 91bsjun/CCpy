#!/usr/bin/env python

# This script is for queue submitting.

import os, sys
from subprocess import call as shl

import pandas as pd

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



def gaussian(queue=None, n_of_cpu=None):
    # --- COLLECT INPUT FILES
    input_marker = [".com"]
    inputs = selectInputs(input_marker, "./", ask=ask)

    # --- SUBMIT QUEUE
    for each_input in inputs:
        myJS = JS(each_input, queue, n_of_cpu)
        myJS.gaussian()

def vasp(queue=None, n_of_cpu=None):
    # --- Collect VASP inputs
    band = False
    recalc = False
    phonon = False
    if "-band" in sys.argv:
        band = True
        inputs = selectVASPInputs("./", ask=ask, band=True)
    elif "-r" in sys.argv:
        recalc = True
        if "01_unconverged_jobs.csv" not in os.listdir("./"):
            print("\n01_unconverged_jobs.csv was not found in this directory.")
            print("Create it using: CCpyVASPAnal.py 0")
            quit()
        df = pd.read_csv("01_unconverged_jobs.csv")
        try:
            df = df.drop('Unnamed: 0', 1)
        except:
            pass
        print("\n* Following jobs will be submitted.")
        print(df)
        proceed = input("Continue ? (y/n) ")
        if proceed != "y":
            quit()
        inputs = df['Directory'].tolist()

    elif "-phonon" in sys.argv:
        phonon = True
        inputs = selectVASPInputs("./", ask=ask, phonon=True)
    else:
        inputs = selectVASPInputs("./", ask=ask)

    # --- SUBMIT QUEUE
    pwd = os.getcwd()
    for each_input in inputs:
        dirpath = pwd + "/" + each_input
        if band:
            dirpath += "/Band-DOS"
        elif phonon:
            dirpath += "/Phonon_opt"
        myJS = JS(each_input, queue, n_of_cpu)
        myJS.vasp(band=band, dirpath=dirpath, phonon=phonon)

def vasp_batch(queue=None, n_of_cpu=None, scratch=False):
    # --- Collect VASP inputs
    band = False
    phonon = False
    recalc = False
    if "-band" in sys.argv:
        band = True
        inputs = selectVASPInputs("./", ask=ask, band=True)
    elif "-phonon" in sys.argv:
        phonon = True
        inputs = selectVASPInputs("./", ask=ask, phonon=True)
    elif "-r" in sys.argv:
        recalc = True
        if "01_unconverged_jobs.csv" not in os.listdir("./"):
            print("\n01_unconverged_jobs.csv was not found in this directory.")
            print("Create it using: CCpyVASPAnal.py 0")
            quit()
        df = pd.read_csv("01_unconverged_jobs.csv")
        try:
            df = df.drop('Unnamed: 0', 1)
        except:
            pass
        print("\n* Following jobs will be submitted.")
        print(df)
        proceed = input("Continue ? (y/n) ")
        if proceed != "y":
            quit()
        inputs = df['Directory'].tolist()
    else:
        inputs = selectVASPInputs("./", ask=ask)

    # --- SUBMIT QUEUE
    dirs = []
    pwd = os.getcwd()

    for each_input in inputs:
        dirpath = pwd + "/" + each_input
        if band:
            dirpath += "/Band-DOS"
        elif phonon:
            dirpath += "/Phonon_opt"
        dirs.append(dirpath)
    myJS = JS(each_input, queue, n_of_cpu)
    myJS.vasp_batch(band=band, dirs=dirs, scratch=scratch)


def qchem(queue=None, n_of_cpu=None):
    # --- Collect inputs
    input_marker = [".in"]
    inputs = selectInputs(input_marker, "./", ask=ask)

    # --- SUBMIT QUEUE
    for each_input in inputs:
        myJS = JS(each_input, queue, n_of_cpu)
        myJS.qchem()

def atk(queue=None, n_of_cpu=None, atk_version="atk2017"):
    # --- COLLECT INPUT FILES
    input_marker = [".py"]
    inputs = selectInputs(input_marker, "./", ask=ask)

    # --- SUBMIT QUEUE
    for each_input in inputs:
        myJS = JS(each_input, queue, n_of_cpu)
        myJS.ATK(atk_version=atk_version)


def lammps(queue=None, n_of_cpu=None):
    # --- COLLECT INPUT FILES
    input_marker = ["in."]
    inputs = selectInputs(input_marker, "./", ask=ask)

    # --- SUBMIT QUEUE
    for each_input in inputs:
        myJS = JS(each_input, queue, n_of_cpu)
        myJS.lammps()


def atat(queue=None, n_of_cpu=None):
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
        myJS = JS(each_input, queue, n_of_cpu)
        myJS.atat()

def pbs_runner(queue=None, n_of_cpu=None):
    # --- COLLECT INPUT FILES
    input_marker = [".py"]
    inputs = selectInputs(input_marker, "./", ask=ask)

    # --- SUBMIT QUEUE
    for each_input in inputs:
        myJS = JS(each_input, queue, n_of_cpu)
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
    -n=[integer]    : the number of CPU to use
                      ex) CCpyJobSubmit.py 2 xeon2 -n=8
                      --> will use 8 CPUs in xeon2

    -r              : re-calculate unconverged VASP jobs from '01_unconverged_jobs.csv'
                      ex) CCpyJobSubmit.py 2 xeon3 -r
                      --> '01_unconverged_jobs.csv' required which is generated by 'CCpyVASPAnaly.py 0'

    -band           : when perform VASP band calculation
                      ex) CCpyJobSubmit.py 2 xeon5 -band
                      ex) CCpyJobSubmit.py 2 xeon5 -band -n=8
                      ex) CCpyJobSubmit.py 2 xeon5 -n=8 -band

    -phonon         : when perform optimize calculation for phonon (high accuracy VASP)

    -batch          : run multiple jobs in a single queue
                      (Only VASP supported yet)
                      ex) CCpyJobSubmit.py 2 xeon5 -batch
                      ex) CCpyJobSubmit.py 2 xeon5 -batch -band

    -s              : use scratch directory when run batch jobs
                      (When you calculate quick hundreds jobs, to reduce load in master node)
                      (Only VASP supported yet)

    -a : no check files, calculate all inputs
    
    
    <Options for ATK version handling>
    -atk2018        : ex) CCpyJobSubmit.py 3 xeon4 -atk2018  
      
    Since the default version is 2017, you do not need to mention it when you want to use the 2017 version.


    '''
              )
        quit()


    # --- Queue name check
    queues = ["xeon1", "xeon2", "xeon3", "xeon4", "xeon5", "xeon6", "xeon7", "I5", "epyc", "aws"]
    try:
        queue = sys.argv[2]
    except:
        queue = raw_input("Queue (xeon1, xeon2, ...) : ")
    if queue not in queues:
        print("Unvalid queue name")
        quit()


    # --- Suboption parsing
    # -- Select all inputs automatically
    scratch = False
    ask = True
    n_of_cpu=None
    atk_version='atk2017'
    for s in sys.argv:
        if "-n" in s:
            n_of_cpu = int(s.split("=")[1])
        if "-s" in s:
            scratch = True
        if "-a" in s:
            ask = False
        if '-atk2018' in s:
            atk_version='atk2018'

    ## ------ GAUSSIAN
    if sys.argv[1] == "1":
        gaussian(queue=queue, n_of_cpu=n_of_cpu)

    ## ------ VASP
    elif sys.argv[1] == "2":
        if "-batch" in sys.argv:
            vasp_batch(queue=queue, n_of_cpu=n_of_cpu, scratch=scratch)
        else:
            vasp(queue=queue, n_of_cpu=n_of_cpu)

    ## ------ ATK
    elif sys.argv[1] == "3":
        atk(queue=queue, n_of_cpu=n_of_cpu, atk_version=atk_version)

    ## ------ Q-chem
    elif sys.argv[1] == "4":
        qchem(queue=queue, n_of_cpu=n_of_cpu)


    ## ------ ATAT
    elif sys.argv[1] == "6":
        atat(queue=queue, n_of_cpu=n_of_cpu)

    ## ------ ATAT
    elif sys.argv[1] == "7":
        lammps(queue=queue, n_of_cpu=n_of_cpu)

    ## ------ PBS JOBS DISPLAYER
    elif sys.argv[1] == "8":
        pbs_runner(queue=queue, n_of_cpu=n_of_cpu)
