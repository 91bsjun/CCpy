#!/usr/bin/env python

# This script is for queue submitting.

import os, sys
from subprocess import call as shl
from collections import OrderedDict

import numpy as np
import pandas as pd
import yaml

from CCpy.Queue.CCpyJobControl import JobSubmit as JS
from CCpy.Tools.CCpyTools import selectInputs, selectVASPInputs, selectSIESTAInput
from CCpy.Tools.CCpyTools import linux_command as lc
from CCpy.Tools.CCpyTools import get_ip

# -- version chk
version = sys.version
if version[0] == '3':
    raw_input = input

# -- Check node00
#ip = get_ip()
#if ip == "166.104.249.31":
#    print("DO AT NODE00 !!")
#    quit()


class JobInitiator:
    def __init__(self, queue, node=None, n_of_cpu=None):
        self.queue = queue
        self.node = node
        self.n_of_cpu = n_of_cpu

    def gaussian(self):
        # --- COLLECT INPUT FILES
        input_marker = [".com"]
        inputs = selectInputs(input_marker, "./", ask=ask)

        # --- SUBMIT QUEUE
        for each_input in inputs:
            myJS = JS(each_input, self.queue, self.n_of_cpu, node=self.node)
            myJS.gaussian()

    def gaussian_batch(self):
        # --- COLLECT INPUT FILES
        input_marker = [".com"]
        inputs = selectInputs(input_marker, "./", ask=ask)

        myJS = JS("batch_job", self.queue, self.n_of_cpu, node=self.node)
        myJS.gaussian_batch(inputs)

    def vasp(self, sub=None, loop=None, additional_dir=None, sequence=None, refine_poscar=False):
        # --- Collect VASP inputs
        band = False
        if "-band" in sys.argv:
            band = True
            inputs = selectVASPInputs("./", ask=ask, sub=sub, additional_dir="Band-DOS")
        elif "-r" in sys.argv:
            if "01_unconverged_jobs.csv" not in os.listdir("./"):
                print("\n01_unconverged_jobs.csv was not found in this directory.")
                print("Create it using: CCpyVASPAnal.py 0")
                quit()
            df = pd.read_csv("01_unconverged_jobs.csv")
            try:
                df = df.drop('Unnamed: 0', 1)
            except:
                pass
            print("\n* Unconverged job list in 01_unconverged_jobs.csv")
            inputs = selectVASPInputs("./", dir_list=df['Directory'].tolist())
        elif additional_dir:
            inputs = selectVASPInputs("./", ask=ask, sub=sub, additional_dir=additional_dir)
        elif sequence:
            inputs = selectInputs(['.cif'], "./")
        else:
            inputs = selectVASPInputs("./", ask=ask, sub=sub)


        # --- SUBMIT QUEUE
        pwd = os.getcwd()
        for each_input in inputs:
            dirpath = pwd + "/" + each_input
            if band:
                dirpath += "/Band-DOS"
            elif additional_dir:
                dirpath += "/" + additional_dir
            myJS = JS(each_input, self.queue, self.n_of_cpu, node=self.node)
            myJS.vasp(band=band, dirpath=dirpath, loop=loop, sequence=sequence, refine_poscar=refine_poscar)

    def vasp_batch(self, scratch=False, sub=False, loop=False, additional_dir=None, series=False, sequence=False, refine_poscar=False):
        # --- Collect VASP inputs
        band = False
        if "-band" in sys.argv:
            band = True
            inputs = selectVASPInputs("./", ask=ask, sub=sub, additional_dir="Band-DOS")
        elif additional_dir:
            inputs = selectVASPInputs("./", ask=ask, sub=sub, additional_dir=additional_dir)
        elif "-r" in sys.argv:
            if "01_unconverged_jobs.csv" not in os.listdir("./"):
                print("\n01_unconverged_jobs.csv was not found in this directory.")
                print("Create it using: CCpyVASPAnal.py 0")
                quit()
            df = pd.read_csv("01_unconverged_jobs.csv")
            try:
                df = df.drop('Unnamed: 0', 1)
            except:
                pass
            print("\n* Unconverged job list in 01_unconverged_jobs.csv")
            inputs = selectVASPInputs("./", dir_list=df['Directory'].tolist())
        elif sequence:
            inputs = selectInputs(['.cif'], "./")
        else:
            inputs = selectVASPInputs("./", ask=ask, sub=sub)

        # --- SUBMIT QUEUE
        dirs = []
        pwd = os.getcwd()

        for each_input in inputs:
            dirpath = pwd + "/" + each_input
            if band:
                dirpath += "/Band-DOS"
            elif additional_dir:
                dirpath += "/" + additional_dir
            elif sequence:
                dirpath = each_input
            dirs.append(dirpath)
        myJS = JS("batch_job", self.queue, self.n_of_cpu, node=self.node)
        if not series:
            myJS.vasp_batch(dirs=dirs, scratch=scratch, loop=loop, sequence=sequence, refine_poscar=refine_poscar)
        else:
            block_number = int(input("Block number: "))
            base_name = input("Base name: ")
            block_number += 1
            sn = np.linspace(0, len(dirs), block_number, dtype="int")
            for i in range(len(sn) -1):
                series_dirs = dirs[sn[i]:sn[i+1]]
                jobname = "%s_%d_%d" % (base_name, sn[i]+1, sn[i+1])
                myJS.vasp_batch(dirs=series_dirs, scratch=scratch, loop=loop, jobname=jobname, sequence=sequence, refine_poscar=refine_poscar)

    def qchem(self):
        # --- Collect inputs
        input_marker = [".in"]
        inputs = selectInputs(input_marker, "./", ask=ask)

        # --- SUBMIT QUEUE
        for each_input in inputs:
            myJS = JS(each_input, self.queue, self.n_of_cpu, node=self.node)
            myJS.qchem()

    def atk(self, atk_version="atk2017"):
        # --- COLLECT INPUT FILES
        input_marker = [".py"]
        inputs = selectInputs(input_marker, "./", ask=ask)

        # --- SUBMIT QUEUE
        for each_input in inputs:
            myJS = JS(each_input, self.queue, self.n_of_cpu, node=self.node)
            myJS.ATK(atk_version=atk_version)

    def lammps(self):
        # --- COLLECT INPUT FILES
        input_marker = ["in."]
        inputs = selectInputs(input_marker, "./", ask=ask)

        # --- SUBMIT QUEUE
        for each_input in inputs:
            myJS = JS(each_input, self.queue, self.n_of_cpu, node=self.node)
            myJS.lammps()

    def atat(self):
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
            myJS = JS(each_input, self.queue, self.n_of_cpu, node=self.node)
            myJS.atat()

    def pbs_runner(self):
        # --- COLLECT INPUT FILES
        input_marker = [".py"]
        inputs = selectInputs(input_marker, "./", ask=ask)

        # --- SUBMIT QUEUE
        for each_input in inputs:
            myJS = JS(each_input, self.queue, self.n_of_cpu, node=self.node)
            myJS.pbs_runner()

    def AIMD_NVT_Loop(self, input_file=None, temp=None, specie="Li", screen="no_screen", max_step=250, vdw=False):
        if not input_file:
            # --- COLLECT INPUT FILES
            input_marker = [".cif", "POSCAR", "CONTCAR"]
            inputs = selectInputs(input_marker, "./", ask=ask)
            if len(inputs) != 1:
                print("Only single file available.")
                quit()
            input_file = inputs[0]

        myJS = JS(input_file, self.queue, self.n_of_cpu, node=self.node)
        myJS.AIMD_NVT_Loop(structure_filename=input_file, temp=temp, specie=specie, screen=screen, max_step=max_step, vdw=vdw)

    def AIMD_NVT_Loop_batch(self, temp=None, specie="Li", screen="no_screen", max_step=250, vdw=False):
        # --- COLLECT INPUT FILES
        input_marker = [".cif", "POSCAR", "CONTCAR"]
        inputs = selectInputs(input_marker, "./", ask=ask)

        myJS = JS(inputs, self.queue, self.n_of_cpu, node=self.node)
        myJS.AIMD_NVT_Loop_batch(structure_files = inputs, temp=temp, specie=specie, screen=screen, max_step=max_step, vdw=vdw)

    def casm_run(self):
        # --- SUBMIT QUEUE
        myJS = JS(None, self.queue, self.n_of_cpu, node=self.node)
        myJS.casm_run()

    def siesta(self, sub=False):
        # --- Collect inputs
        inputs = selectSIESTAInput("./", ask=ask, sub=sub)

        # --- SUBMIT QUEUE
        for each_input in inputs:
            myJS = JS(each_input, self.queue, self.n_of_cpu, node=self.node)
            myJS.siesta()

    def siesta_AIMD_NVT_Loop(self, temp=None, specie="Li"):
        # --- COLLECT INPUT FILES
        input_marker = [".cif", "POSCAR", "CONTCAR"]
        inputs = selectInputs(input_marker, "./", ask=ask)
        if len(inputs) != 1:
            print("Only single file available.")
            quit()

        myJS = JS(inputs[0], self.queue, self.n_of_cpu, node=self.node)
        myJS.siesta_AIMD_NVT_Loop(structure_filename=inputs[0], temp=temp, specie=specie)

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


if __name__ == "__main__":
    # -- initiate config file
    JS(None, None, None, init_only=True)
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
    8  : SIESTA
    9  : NVT MD Loop
    10 : CASM VASP job run
    11 : PBS job display
    12 : SIESTA NVT MD Loop

< Option 2. > Queue
    xeon1, xeon2, xeon3, ...

[Suboptions] (optional)
    -n=[integer]    : the number of CPU to use
                      ex) CCpyJobSubmit.py 2 xeon2 -n=8
                      --> will use 8 CPUs in xeon2

    -node=[nodename]: assign specific node to submit
                      ex) CCpyJobSubmit.py 2 xeon2 -node=node03

    <Support for VASP only>
    -sub            : find vasp jobs under sub directories (only support for vasp)
                      ex) CCpyJobSubmit.py 2 xeon4 -sub
                      ex) CCpyJobSubmit.py 2 xeon5 -batch -sub

    -series         : series batch jobs

    -r              : re-calculate unconverged VASP jobs from '01_unconverged_jobs.csv'
                      ex) CCpyJobSubmit.py 2 xeon3 -r
                      --> '01_unconverged_jobs.csv' required which is generated by 'CCpyVASPAnaly.py 0'

    -band           : when perform VASP band calculation
                      ex) CCpyJobSubmit.py 2 xeon5 -band
                      ex) CCpyJobSubmit.py 2 xeon5 -band -n=8
                      ex) CCpyJobSubmit.py 2 xeon5 -n=8 -band

    -batch          : run multiple jobs in a single queue (only support for vasp)
                      ex) CCpyJobSubmit.py 2 xeon5 -batch
                      ex) CCpyJobSubmit.py 2 xeon5 -batch -band

    -scratch        : use scratch directory when run batch jobs
                      (When you calculate quick hundreds jobs, to reduce load in master node)
                      (Only VASP supported yet)
                      ex) CCpyJobSubmit.py 2 xeon5 -batch -scratch
                      ex) CCpyJobSubmit.py 2 xeon5 -batch -sub -scratch

    -a              : no check files, calculate all inputs

    -T              : Assign temperature when NVT MD simulation in VASP
                      ex) CCpyJobSubmit.py 9 xeon6 -n=24 -T=1000
    
    -specie=        : Assign diffusion element when NVT MD simulation in VASP (optional, default: Li)
                      ex) CCpyJobSubmit.py 9 xeon6 -n=24 -T=1000 -specie=Na

    -loop           : run VASP jobs until converged. (error will be handled using custodian library in pymatgen)
                      ex) CCpyJobSubmit.py 2 xeon5 -loop
                      *** very careful when use this option ***
                      Not recommend under less understanding of VASP jobs

    -dir=[NAME]     : Find vasp jobs under */[NAME]
                      ./graphene/DOS
                      ex) CCpyJobSubmit.py 2 xeon2 -dir=DOS

    -sequence=[FILENAME] : sequence calculation based on presets and dirname in [FILENAME]
    -refine_poscar : option can be used with this option.
    [Sequence file example] 
    default  ./
    static   ./static
    band     ./Band-DOS



    '''
              )
        home = os.getenv("HOME")
        print(bcolors.OKGREEN + "    *** Queue config file: %s/.CCpy/queue_config.yaml ***" % home + bcolors.ENDC)
        print("""    - User can modify software version (ex. vasp_beef, atk2019...)
    - This file is created when CCpyJobSubmit.py is executed without option.
    - Therefore, if you want to regenerate as the default option or 
      if an error occurs due to this file, remove the file and execute the A command.
""")

        quit()

    try:
        queue = sys.argv[2]
    except:
        print('queue name is not assigned.')
        quit()

    # --- Suboption parsing
    # -- Select all inputs automatically
    scratch = False
    ask = True
    n_of_cpu = None
    atk_version = 'atk2017'
    temp = None
    sub = False
    loop = False
    node = None
    specie = "Li"               # AIMD option
    screen = "no_screen"        # AIMD option
    max_step = 100              # AIMD option
    additional_dir = None       # additional calc for VASP
    series = False              # series batch job submit
    vdw = False                 # optB88 at AIMD LVT Loop
    input_file = None           # input single file
    sequence = None
    refine_poscar = False
    for s in sys.argv:
        if "-n=" in s:
            n_of_cpu = int(s.split("=")[1])
        if "-scratch" in s:
            scratch = True
        if "-a" in s:
            ask = False
        if '-atk2018' in s:
            atk_version = 'atk2018'
        if '-atk2019' in s:
            atk_version = 'atk2019'
        if '-atk2019.12' in s:
            atk_version = 'atk2019.12'
        if '-T=' in s:
            temp = int(s.split("=")[1])
        if '-sub' in s:
            sub = True
        if '-loop' in s:
            loop = True
        if '-node=' in s:
            node = s.split("=")[1]
        if '-specie=' in s:
            specie = s.split("=")[1]
        if '-screen' in s:
            screen = 'screen'
        if '-dir=' in s:
            additional_dir = s.split("=")[1]
        if '-max_step=' in s:
            max_step = s.split("=")[1]
        if '-series' in s:
            series = True
        if '-vdw' in s:
            vdw = s.split("=")[1]
        if '-input=' in s:
            input_file = s.split("=")[1]
        if '-sequence=' in s:
            sequence = s.split("=")[1]
        if '-refine_poscar' in s:
            refine_poscar = True

    job_init = JobInitiator(queue=queue, node=node, n_of_cpu=n_of_cpu)

    ## ------ GAUSSIAN
    if sys.argv[1] == "1":
        if "-batch" in sys.argv:
            job_init.gaussian_batch()
        else:
            job_init.gaussian()

    ## ------ VASP
    elif sys.argv[1] == "2":
        if "-batch" in sys.argv:
            job_init.vasp_batch(scratch=scratch, sub=sub, loop=loop, additional_dir=additional_dir, series=series, sequence=sequence, refine_poscar=refine_poscar)
        else:
            job_init.vasp(sub=sub, loop=loop, additional_dir=additional_dir, sequence=sequence, refine_poscar=refine_poscar)

    ## ------ ATK
    elif sys.argv[1] == "3":
        job_init.atk(atk_version=atk_version)

    ## ------ Q-chem
    elif sys.argv[1] == "4":
        job_init.qchem()

    ## ------ ATAT
    elif sys.argv[1] == "6":
        job_init.atat()

    ## ------ LAMMPS
    elif sys.argv[1] == "7":
        job_init.lammps()

    ## ------ SIESTA
    elif sys.argv[1] == "8":
        job_init.siesta(sub=sub)

    ## ------ VASP NVT LOOP
    elif sys.argv[1] == "9":
        if not temp:
            print("Temperature must be assigned. (ex: -T=1000)")
            quit()
        if "-batch" in sys.argv:
            job_init.AIMD_NVT_Loop_batch(temp=temp, specie=specie, screen=screen, max_step=max_step, vdw=vdw)
        elif input_file:
            job_init.AIMD_NVT_Loop(input_file=input_file, temp=temp, specie=specie, screen=screen, max_step=max_step, vdw=vdw)
        else:
            job_init.AIMD_NVT_Loop(temp=temp, specie=specie, screen=screen, max_step=max_step, vdw=vdw)

    ## ------ VASP NVT LOOP
    elif sys.argv[1] == "10":
        job_init.casm_run()

    ## ------ PBS JOBS DISPLAYER
    elif sys.argv[1] == "11":
        job_init.pbs_runner()

        ## ------ VASP NVT LOOP
    elif sys.argv[1] == "12":
        if not temp:
            print("Temperature must be assigned. (ex: -T=1000)")
            quit()
        job_init.siesta_AIMD_NVT_Loop(temp=temp, specie=specie)
