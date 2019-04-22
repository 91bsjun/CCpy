def NVTLoopQueScriptString():
    string = """
import os, sys
import re
import time
import pickle
import numpy as np
import pandas as pd
from pymatgen.io.vasp import Vasprun
from pymatgen.io.vasp.sets import MITMDSet
from pymatgen.core.structure import IStructure
from pymatgen.analysis.diffusion_analyzer import DiffusionAnalyzer

# ---------- CONFIGURATIONS ------------ #
vasp = "vasp"
NCORE = 4
#user_incar = {"NCORE": NCORE, "ENCUT": 400, "LREAL": "Auto", "PREC": "Normal", "ALGO": "Fast", "EDIFF": 1E-05, "ICHARG": 0, "IALGO": 48}
user_incar = {"NCORE": NCORE, "PREC": "Normal", "ALGO": "Fast", "ICHARG": 0}

structure_filename = sys.argv[1]
temp = int(sys.argv[2])
specie = sys.argv[3]

heating_nsw = 2000
nsw = 1000
min_step = 50
min_RSD = 0.25
min_ASD = 7
# -------------------------------------- #

# INCAR
# defaults = {'TEBEG': start_temp, 'TEEND': end_temp, 'NSW': nsteps,
#             'EDIFF_PER_ATOM': 0.000001, 'LSCALU': False,
#             'LCHARG': False,
#             'LPLANE': False, 'LWAVE': True, 'ISMEAR': 0,
#             'NELMIN': 4, 'LREAL': True, 'BMIX': 1,
#             'MAXMIX': 20, 'NELM': 500, 'NSIM': 4, 'ISYM': 0,
#             'ISIF': 0, 'IBRION': 0, 'NBLOCK': 1, 'KBLOCK': 100,
#             'SMASS': 0, 'POTIM': time_step, 'PREC': 'Low',
#             'ISPIN': 2 if spin_polarized else 1,
#             "LDAU": False}


def mkdir(dirname):
    if dirname not in os.listdir("./"):
        os.mkdir(dirname)


def write_log(msg):
    f = open("log", "a")
    f.write(msg + "\\n")
    f.close()


def terminated_check(nsw):
    tail_oszicar = os.popen("tail OSZICAR | grep T=").readlines()
    # -- when empty OSZICAR
    if len(tail_oszicar) == 0:
        return False
    else:
        oszicar = tail_oszicar[-1]
        crt_step = int(oszicar.split()[0])
        # -- when previous run was completed
        if crt_step == nsw:
            return True
        # -- when previous run was stopped
        else:
            return False


def prev_check():
    # -- Checking previous AIMD runs step

    # -- collect run** directories
    runs = [d for d in os.listdir("./") if "run" in d and d.replace("run", "").isdigit()]
    runs.sort()

    if len(runs) == 0:
        run_step = 0
    else:
        tail_oszicar = os.popen("tail %s/OSZICAR | grep T=" % runs[-1]).readlines()
        # -- when empty OSZICAR
        if len(tail_oszicar) == 0:
            run_step = len(runs) - 1

        else:
            if len(runs) == 1:
                crt_nsw = heating_nsw
            else:
                crt_nsw = nsw
            oszicar = tail_oszicar[-1]
            crt_step = int(oszicar.split()[0])

            # -- when previous run was completed
            if crt_step == crt_nsw:
                run_step = len(runs)
            # -- when previous run was stopped
            else:
                run_step = len(runs) - 1

    return run_step


def running(temp, pre, crt):
    # -- Run AIMD at (run00, run01, ...)
    # -- First step is heat-up.
    total_try = 1
    #pre_dir = ("run%2d" % pre).replace(" ", "0")
    #crt_dir = ("run%2d" % crt).replace(" ", "0")
    pre_dir = ("run%03d" % pre)
    crt_dir = ("run%03d" % crt)
    # -- initiating
    if crt == 0:
        structure = IStructure.from_file("../" + structure_filename)
        crt_nsw = heating_nsw
        user_incar["SMASS"] = -1
        inputset = MITMDSet(structure, 100.0, float(temp), heating_nsw, user_incar_settings=user_incar)
        inputset.write_input(crt_dir)
    # -- run
    else:
        run = Vasprun("%s/vasprun.xml.gz" % pre_dir, parse_dos=False, parse_eigen=False)
        structure = run.final_structure
        crt_nsw = nsw
        #structure = IStructure.from_file("%s/CONTCAR" % pre_dir)
        user_incar["SMASS"] = 0
        inputset = MITMDSet(structure, float(temp), float(temp), nsw, user_incar_settings=user_incar)
        inputset.write_input(crt_dir)
        os.system("cp %s/WAVECAR %s" % (pre_dir, crt_dir))
    os.chdir(crt_dir)    
    os.system("mpirun -np $NSLOTS %s < /dev/null > vasp.out" % vasp)
    time.sleep(5)
    os.system("gzip OUTCAR vasprun.xml")
    os.system("rm -rf DOSCAR XDATCAR")
    write_log("try: %d" % total_try)
    properly_terminated = terminated_check(crt_nsw)
    while not properly_terminated:
        total_try += 1
        os.system("mpirun -np $NSLOTS %s < /dev/null > vasp.out" % vasp)
        time.sleep(5)
        os.system("gzip OUTCAR vasprun.xml")
        os.system("rm -rf DOSCAR XDATCAR")
        write_log("try: %d" % total_try)
        properly_terminated = terminated_check(crt_nsw)        
    os.system("touch vasp.done")    
    os.chdir("../")
    os.system("rm -rf %s/WAVECAR" % pre_dir)   # remove WAVECAR in previous dir to reduce storage


def write_data(crt):
    # -- Save diffusivity and conductivity of current step
    # -- Skip initial heating process and stablizing process : set as start_num
    start_num = 1
    if crt >= start_num:
        vaspruns = []
        for i in range(start_num, crt + 1):
            dirname = "run%03d" % i
            vasprun = dirname + "/vasprun.xml.gz"
            vaspruns.append(vasprun)

        # -- collect all smoothing modes of analyzer
        analyzers = {}
        for mode in [False, 'constant', 'max']:
            try:
                analyzers[mode] = DiffusionAnalyzer.from_files(vaspruns, specie=specie, smoothed=mode, min_obs=60)
            except:
                analyzers[mode] = None

        # -- save DiffusionAnalzyer as pickle to plot msd quickly
        #if crt % 10 == 0:
        #    with open("analyzer%03d.pkl" % crt, 'wb') as save_data:
        #        pickle.dump(analyzers, save_data)

        # -- write data
        f = open("data_%sK.csv" % temp, "a")
        timestep = (crt - start_num + 1) * nsw * 2 / 1000
        step_info = "%d,%d," % (crt, timestep)
        f.write(step_info)
        for mode in [False, 'constant', 'max']:
            if analyzers[mode] == None:
                diffusivity, conductivity = 0.0, 0.0
            else:
                sd = analyzers[mode].get_summary_dict()
                diffusivity = sd['D']
                conductivity = sd['S']
            f = open("data_%sK.csv" % temp, "a")
            line = "%.10f,%.4f," % (diffusivity, conductivity)
            f.write(line)
        f.write("\\n")
        f.close()


def write_diffusivity_data(crt, specie, specie_distance, temp):
    start_num = 1
    chg_data = {"Li": "+", "Na": "+", "K": "+", "Cu": "+"}
    if crt >= start_num:
        os.system("analyze_aimd.py diffusivity %s%s run 1 %d %.2f -msd msd_%dK.csv >> anal.log" % (specie, chg_data[specie], crt, specie_distance, temp))
    datafilename = "Mo_%dK_data.csv" % temp
    bjunfilename = "bj_%dK_data.csv" % temp
    if datafilename not in os.listdir("./"):
        f = open("Mo_%dK_data.csv" % temp, "w")
        f.write("step,RSD,diffusivity,diffusivity_err\\n")
        f.close()
    df = pd.read_csv(datafilename)
    try:
        # handle old version
        std_list = df['std'].tolist()
    except:
        std_list = df['RSD'].tolist()
    if len(std_list) == 0:
        RSD = 1
        ASD = 99
    else:
        RSD = std_list[-1]
        ASD = write_ASD_data(datafilename)

    return float(RSD), float(ASD)


def write_ASD_data(csvfile):
    df = pd.read_csv(csvfile)
    df = df.reset_index()
    x = df['step']
    try:
        y1 = df['RSD']
    except:
        # to handle old version datafile
        y1 = df['std']
    y2 = np.array(df['diffusivity'].tolist())

    avgs = []
    stds = []
    std_avg = []
    run = []
    avg_range = 20
    center_of_avg = int(avg_range / 2)
    # -- collect std at range of 5 steps (x-4, x-3, x-2, x-1, x)
    # -- run includes index of x
    for i, v in enumerate(y2):
        if i >= avg_range:
            grp = y2[i-avg_range:i+1]
            run.append(i + x[0])
            avgs.append(grp.mean())
            stds.append(grp.std())
            std_avg.append(grp.std() / grp.mean() * 100)
        else:
            grp = y2[:i+1]
            run.append(i + x[0])
            avgs.append(grp.mean())
            stds.append(grp.std())
            std_avg.append(grp.std() / grp.mean() * 100)

    data = {'step': run, 'RSD': y1, 'diffusivity': df['diffusivity'], 'diffusivity_err': df['diffusivity_err'], 'ASD': std_avg, 'avg_d': avgs}
    data_df = pd.DataFrame(data)
    data_df['diffusivity'] = data_df['diffusivity'].map('{:,.12f}'.format)
    data_df['diffusivity_err'] = data_df['diffusivity_err'].map('{:,.12f}'.format)
    data_df['RSD'] = data_df['RSD'].map('{:,.4f}'.format)
    data_df['avg_d'] = data_df['avg_d'].map('{:,.12f}'.format)
    data_df['ASD'] = data_df['ASD'].map('{:,.4f}'.format)

    new_filename = csvfile.replace("Mo_", "bj_")
    data_df.to_csv(new_filename, index=False)
    
    return std_avg[-1]


def check_converged(crt_step, RSD, ASD):
    converged = False
    if crt_step >= min_step:
        if RSD <= min_RSD:
            if ASD <= min_ASD:
                converged = True
    if crt_step >= 300:
        converged = True

    return converged


if __name__ == "__main__":
    structure = IStructure.from_file(structure_filename)
    # -- Find neighboring specie distance
    sites = structure.sites
    specie_sites = [s for s in sites if str(s.specie) == specie]
    distance = []
    for specie_site in specie_sites:    
        nbrs = structure.get_neighbors(specie_site, 5)
        nbrs = [nbr for nbr in nbrs if str(nbr[0].specie) == specie]
        for nbr in nbrs:
            distance.append(nbr[1])
    distance = np.array(distance)
    avg_specie_distance = distance.mean()
    
    working_dir = "%dK" % temp
    mkdir(working_dir)
    os.chdir(working_dir)
    if "loop.done" in os.listdir("./"):
        os.system("rm loop.done")

    crt_step = prev_check()
    if crt_step == 0:
        # -- write header of data file
        if "data_%sK.csv" % temp in os.listdir("./"):
            os.rename("data_%sK.csv" % temp, "data_%sK.csv~" % temp)
        f = open("data_%sK.csv" % temp, "w")
        f.write("run step,timestep,diffusivity(F),conductivity(F),diffusivity(c),conductivity(c),diffusivity(m),conductivity(m)\\n")
        f.close()
    while "loop.done" not in os.listdir("./"):
        pre_step = crt_step - 1
        running(temp, pre_step, crt_step)
        write_data(crt_step)
        RSD, ASD = write_diffusivity_data(crt_step, specie, avg_specie_distance, temp)
        
        converged_check = check_converged(crt_step, RSD, ASD)
        if converged_check:
            os.system("touch loop.done")

        crt_step += 1
"""
    return string
