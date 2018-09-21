def NVTLoopQueScriptString():
    string = """"#!/bin/env python
import os, sys
import re
import time
import pickle
from pymatgen.io.vasp.sets import MITMDSet
from pymatgen.core.structure import IStructure
from pymatgen.analysis.diffusion_analyzer import DiffusionAnalyzer

# ---------- CONFIGURATIONS ------------ #
NCORE = 4
nsw = 25000
user_incar = {"MDALGO": 2, "NCORE": NCORE, "ENCUT": 280}
structure_filename = sys.argv[1]
temp = int(sys.argv[2])
vasp = "vasp"
# -------------------------------------- #

#INCAR
#defaults = {'TEBEG': start_temp, 'TEEND': end_temp, 'NSW': nsteps,
#            'EDIFF_PER_ATOM': 0.000001, 'LSCALU': False,
#            'LCHARG': False,
#            'LPLANE': False, 'LWAVE': True, 'ISMEAR': 0,
#            'NELMIN': 4, 'LREAL': True, 'BMIX': 1,
#            'MAXMIX': 20, 'NELM': 500, 'NSIM': 4, 'ISYM': 0,
#            'ISIF': 0, 'IBRION': 0, 'NBLOCK': 1, 'KBLOCK': 100,
#            'SMASS': 0, 'POTIM': time_step, 'PREC': 'Low',
#            'ISPIN': 2 if spin_polarized else 1,
#            "LDAU": False}



def mkdir(dirname):
    if dirname not in os.listdir("./"):
        os.mkdir(dirname)


def prev_check():
    # -- Checking previous AIMD runs step

    # -- collect run** directories
    runs = [d for d in os.listdir("./") if "run" in d and d.replace("run", "").isdigit()]
    runs.sort()

    if len(runs) == 0:
        run_step = 0
    else:
        tail_oszicar = os.popen("tail %s/OSZICAR | grep \"T=\"" % runs[-1]).readlines()
        # -- when empty OSZICAR
        if len(tail_oszicar) == 0:
            run_step = len(runs) - 1

        else:
            oszicar = tail_oszicar[-1]
            crt_step = int(oszicar.split()[0])

            # -- when previous run was completed
            if crt_step == nsw:
                run_step = len(runs)
            # -- when previous run was stopped
            else:
                run_step = len(runs) - 1

    return run_step


def running(temp, pre, crt):
    # -- Run AIMD at (run00, run01, ...)
    # -- First step is heat-up.

    pre_dir = ("run%2d" % pre).replace(" ", "0")
    crt_dir = ("run%2d" % crt).replace(" ", "0")
    # -- initiating
    if crt == 0:
        structure = IStructure.from_file("../" + structure_filename)
        heating_nsw = 5000
        inputset = MITMDSet(structure, float(temp), float(temp), heating_nsw, user_incar_settings=user_incar)
        inputset.write_input(crt_dir)
    # -- run
    else:
        structure = IStructure.from_file("%s/CONTCAR" % pre_dir)
        inputset = MITMDSet(structure, float(temp), float(temp), nsw, user_incar_settings=user_incar)
        inputset.write_input(crt_dir)
    os.chdir(crt_dir)
    os.system("mpirun -np $NSLOTS %s < /dev/null > vasp.out" % vasp)
    os.system("touch vasp.done")
    time.sleep(10)
    os.chdir("../")


def write_data(crt):
    # -- Save diffusivity and conductivity of current step
    # -- Skip initial heating process and stablizing process : set as start_num
    start_num = 1
    if crt >= start_num:
        vaspruns = []
        for i in range(start_num, crt + 1):
            dirname = ("run%2d" % i).replace(" ", "0")
            vasprun = dirname + "/vasprun.xml"
            vaspruns.append(vasprun)

        # -- collect all smoothing modes of analyzer
        analyzers = {}
        for mode in [False, 'constant', 'max']:
            analyzers[mode] = DiffusionAnalyzer.from_files(vaspruns, specie="Li", smoothed=mode, min_obs=60)

        # -- save DiffusionAnalzyer as pickle to plot msd quickly
        with open(("analyzer%2d.pkl" % crt).replace(" ", "0"), 'wb') as save_data:
            pickle.dump(analyzers, save_data)

        # -- write data
        f = open("data_%sK.csv" % temp, "a")
        timestep = (crt - start_num + 1) * nsw * 2 / 1000
        step_info = "%d,%d," % (crt, timestep)
        f.write(step_info)
        for mode in [False, 'constant', 'max']:
            sd = analyzers[mode].get_summary_dict()
            diffusivity = sd['D']
            conductivity = sd['S']
            f = open("data_%sK.csv" % temp, "a")
            line = "%.10f,%.4f," % (diffusivity, conductivity)
            f.write(line)
        f.write("\n")
        f.close()


if __name__ == "__main__":
    working_dir = "%dK" % temp
    mkdir(working_dir)
    os.chdir(working_dir)

    crt_step = prev_check()
    if crt_step == 0:
        # -- write header of data file
        if "data_%sK.csv" % temp in os.listdir("./"):
            os.rename("data_%sK.csv" % temp, "data_%sK.csv~" % temp)
        f = open("data_%sK.csv" % temp, "w")
        f.write(
            "run step,timestep,diffusivity(F),conductivity(F),diffusivity(c),conductivity(c),diffusivity(m),conductivity(m)\n")
        f.close()
    while "loop.done" not in os.listdir("./"):
        pre_step = crt_step - 1
        running(temp, pre_step, crt_step)
        write_data(crt_step)
        crt_step += 1


"""
    return string