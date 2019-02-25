#!/bin/env python
import os, sys
import numpy as np
import scipy.constants as const
import pandas as pd
from tabulate import tabulate
import matplotlib.pyplot as plt
import pickle
import collections
import warnings
warnings.filterwarnings("ignore")

from pymatgen.analysis.diffusion_analyzer import DiffusionAnalyzer, fit_arrhenius, get_arrhenius_plot, get_extrapolated_conductivity, get_extrapolated_diffusivity

version = sys.version
if version[0] == '3':
    raw_input = input

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [sub_option1] [sub_option2..]")
    print(""""--------------------------------------
[options]
1   : Plot diffusivity(ies) written in *K/Mo_*K_data.csv
      ex) CCpyNVTLoopAnal.py 1 600K/Mo_600K_data.csv
          CCpyNVTLoopAnal.py 1 600K/Mo_600K_data.csv 800K/Mo_800K_data.csv 1000K/Mo_1000K_data.csv
          
    [suboption]
    -x=r  : use x-axis as runstep (default -x=t)
            ex) CCpyNVTLoopAnal.py 1 -x=r 600K/Mo_600K_data.csv (use x-axis as runstep)
          
          
2   : Plot MSD/dt
      ex) CCpyNVTLoopAnal.py 2 msd_600K.csv
          CCpyNVTLoopAnal.py 2 msd_600K.csv msd_800K.csv
               
    [suboption]
    -log=False  : Do not plot as log scale
                  CCpyNVTLoopAnal.py 2 msd_600K.csv -log=False
                  
                  
3   : Plot arrhenius fitting      
      ex) CCpyNVTLoopAnal.py 3 LiGePS.csv
          CCpyNVTLoopAnal.py 3 LiGePS.csv LiAlPS.csv
      NOTE. csv file format same with output of [option 1]
            structure file name must identical with csv filename (ex. LGPS.csv, LGPS.cif)
            
    [suboption]
    -specie=Li  : Set specie (default: Li)
                  ex) CCpyNVTLoopAnal.py 3 NaPS.csv -specie=Na
    -T=300      : Set temperature to extrapolate
                  

4   : Plot diffusivity(ies) written by Pymatgen (old version)
      ex) CCpyNVTLoopAnal.py 1 -m=constant 600K/data_600K.csv (default smoothing: constant)
          CCpyNVTLoopAnal.py 1 -m=False 600K/data_600K.csv
          CCpyNVTLoopAnal.py 1 -m=max *K/data*    (multiple files also available)
          CCpyNVTLoopAnal.py 1 -m=max -x=r 600K/data_600K.csv 800K/data_800K.csv    (use x-axis as runstep)
          
"""
          )
    quit()

"""
future things
: msd
"""

colors = ["#0100FF", "#FF0000", "#47C83E", "#FF9933", "#8041D9", "#000000", "#C4B73B", "#00D8FF",
          "#FF00DD", "#003399", "#980000", "#22741C", "#664B00", "#2A0066", "#005766", "#660058"]
colors = ['blue', 'red', 'green', 'darkorange', 'purple', 'black', 'olive', 'maroon', 'darkblue', 'magenta', 'cyan', 'lime']
markers = ['o', 's', 'D', '^', 'v'] * 3

def get_vaspruns():
    all_inputs1 = [d + "/vasprun.xml" for d in os.listdir("./") if
                   os.path.isdir(d) and 'run' in d and 'vasprun.xml' in os.listdir(d)]
    all_inputs2 = [d + "/vasprun.xml.gz" for d in os.listdir("./") if
                   os.path.isdir(d) and 'run' in d and 'vasprun.xml.gz' in os.listdir(d)]
    all_inputs = all_inputs1 + all_inputs2
    all_inputs.sort()
    if len(all_inputs) == 0:
        print("Cannot find any run** directory.")
        quit()

    for i in range(len(all_inputs)):
        print(str(i + 1) + " : " + all_inputs[i])
    print("0 : All files")
    get_num = input("Choose file : ")

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

    return inputs


def get_csvfiles():
    csvfiles = []
    for arg in sys.argv:
        if ".csv" in arg:
            csvfiles.append(arg)
    if len(csvfiles) == 0:
        print("no input csv files.")
        print("Try: " + sys.argv[0].split("/")[-1] + " 600K/data.csv")
        quit()

    return csvfiles

def mkdir(dirname):
    if dirname not in os.listdir("./"):
       os.mkdir(dirname)


def plot_diffusivity(mode, files, xaxis):
    ylabel = "diffusivity(m)"
    if mode == False:
        ylabel = "diffusivity(F)"
    elif mode == "constant":
        ylabel = "diffusivity(c)"
    elif mode == "max":
        ylabel = "diffusivity(m)"
    else:
        print("Use max smoothing as default")

    plt.figure(figsize=(10, 7))
    for i, filename in enumerate(files):
        df = pd.read_csv(filename, index_col=False)
        print(filename)
        print(df)
        print("-" * 20)
        if xaxis == 'r':
            x = df['run step']
        else:
            x = df['timestep']
        y = df[ylabel]
        y_val = np.array([val for val in y.tolist() if val != 0.0])
        #y_avg = (y_val.max() + y_val.min()) / 2.0
        y_avg = y_val.mean()

        label = filename.split("/")[-1].replace("data_","").replace(".csv","")
        plt.plot(x, y, marker='o', color=colors[i], label=label)
        plt.plot(0, y_avg, marker='d', ms=10, color=colors[i])

    if xaxis == 'r':
        plt.xlabel("Run step", fontsize=24)
    else:
        plt.xlabel("Time step (ps)", fontsize=24)
    ax = plt.axes()
    ax.set_yscale('log')
    plt.ylabel(r"Diffusivity (cm$^2$/s)", fontsize=24)
    plt.tick_params(axis='both', which='major', labelsize=16)
    plt.grid()
    plt.legend(prop={'size': 18})
    plt.tight_layout()
    plt.savefig("Diffusivity_%s.png" % mode)
    plt.show()


def plot_msd(mode, vaspruns):
    """
    Create MSD/t plot run00 to current run.
    And save diffusivity and conductivity of current step
    """
    analyzer = DiffusionAnalyzer.from_files(vaspruns, specie="Li", smoothed=mode)
    plt = analyzer.get_msd_plot()
    plt.savefig("msd_%s.png" % str(mode))
    plt.show()

    print("Diffusivity : %.10f" % analyzer.diffusivity)
    print("Conductivity: %.4f" % analyzer.conductivity)

def analysis():
    import matplotlib.pyplot as plt
    modes = raw_input("1. Smoothing modes (ex: False,max // Default: all)\n:(enter: default) ")
    modes = modes.replace(" ","").split(",")
    if len(modes) < 2:
        modes = ["False", "constant", "max"]
    temperatures = raw_input("2. Temperatrue (ex: 600,700,800,1000,1200 // Default: 600,800,1000,1200)\n:(enter: default) ")
    temperatures = temperatures.replace(" ", "").split(",")
    if len(temperatures) < 2:
        temperatures = [600, 800, 1000, 1200]
    else:
        temperatures = [int(t) for t in temperatures]
    runstep = raw_input("3. Run step for each temperature (ex: 7,11,5,4)\n: ")
    runstep = runstep.replace(" ", "").split(",")
    runstep = [int(r) for r in runstep]

    use_pickle = True

    def get_analyzer(temp, step, mode, use_pickle=False):
        if mode == "False":
            mode = False
        print("%d K" % temp)
        if use_pickle:
            pickle_name = ("%dK/analyzer%2d.pkl" % (temp, step)).replace(" ", "0")
            with open(pickle_name, 'rb') as loaded_data:
                pickled_analyzer = pickle.load(loaded_data)

            return pickled_analyzer[mode]
        else:
            vaspruns = []
            for i in range(2, step + 1):
                dirname = ("%dK/run%2d" % (temp, i)).replace(" ", "0")
                vasprun = dirname + "/vasprun.xml"
                if not os.path.isfile(vasprun):
                    vasprun += ".gz"
                vaspruns.append(vasprun)

            return DiffusionAnalyzer.from_files(vaspruns, specie="Li", smoothed=mode)

    def mkdir(dirname):
        if dirname not in os.listdir("./"):
            os.mkdir(dirname)

    pwd = os.getcwd()
    mkdir("Analysis")
    tot_data = {"Smoothed": [], "Temp (K)": [], "Diffusivity (cm^2/s)": [], "Conductivity (mS/cm)": []}

    for mode in modes:
        print("smoothing mode: %s" % mode)
        analyzers = collections.OrderedDict()
        for i, temp in enumerate(temperatures):
            analyzers[temp] = get_analyzer(temp, runstep[i], mode, use_pickle=use_pickle)

        # -- make data
        # ---- msd plot
        for i, temp in enumerate(temperatures):
            if len(temperatures) <= 4:
                plt.subplot(2, 2, i + 1)
            elif len(temperatures) <= 9:
                plt.subplot(3, 3, i + 1)
            analyzers[temp].get_msd_plot(plt=plt)
            plt.title("%dK" % temp, fontsize=20)
            plt.tick_params(axis='both', which='major', labelsize=12)
            plt.tight_layout()
        plt.savefig("./Analysis/%s_msd.png" % mode)
        # plt.show()

        # ---- Arrhenius plot
        diffusivities = [d.diffusivity for d in analyzers.values()]
        plt = get_arrhenius_plot(temperatures, diffusivities)
        plt.tight_layout()
        plt.savefig("./Analysis/%s_arrhenius.png" % mode)
        # plt.show()

        # -- save data
        # ---- extrapolated
        ext_d = get_extrapolated_diffusivity(temperatures, diffusivities, new_temp=300)
        ext_c = get_extrapolated_conductivity(temperatures, diffusivities, new_temp=300,
                                              structure=analyzers[800].structure, species="Li")
        tot_data['Smoothed'].append(str(mode))
        tot_data['Temp (K)'].append(300)
        tot_data['Diffusivity (cm^2/s)'].append(ext_d)
        tot_data['Conductivity (mS/cm)'].append(ext_c)

        for temp in temperatures:
            d = analyzers[temp].diffusivity
            c = analyzers[temp].conductivity
            tot_data["Smoothed"].append(mode)
            tot_data['Temp (K)'].append(temp)
            tot_data['Diffusivity (cm^2/s)'].append(d)
            tot_data['Conductivity (mS/cm)'].append(c)

        os.chdir(pwd)

    df = pd.DataFrame(tot_data)
    df = df[['Smoothed', 'Temp (K)', 'Conductivity (mS/cm)', 'Diffusivity (cm^2/s)']]
    df.to_csv("./Analysis/Diffusion_data.csv")

    df300 = df[(df['Temp (K)'] == 300)]
    print(df300)

def arrhenius_plotter_old(datafiles):
    total_temps = []
    total_diffusivities = []
#    labels = []
    names = [datafile.replace(".csv","") for datafile in datafiles]
    cifs = [name + ".cif" for name in names]
    for datafile in datafiles:
        df = pd.read_csv(datafile)
        keys = df.keys()
        temps = df['T'].tolist()
        for key in keys:
            if key != 'T':
                total_temps.append(temps)
                total_diffusivities.append(df[key].tolist())
#                labels.append(key)

    plt = custom_arrhenius_plot(total_temps, total_diffusivities, names, cifs)
    plt.tight_layout()
    plt.show()




def custom_arrhenius_plot(total_temps, total_diffusivities, names, cifs, diffusivity_errors=None,
                       **kwargs):
    """
    Returns an Arrhenius plot.

    Args:
        temps ([float]): A sequence of temperatures.
        diffusivities ([float]): A sequence of diffusivities (e.g.,
            from DiffusionAnalyzer.diffusivity).
        diffusivity_errors ([float]): A sequence of errors for the
            diffusivities. If None, no error bar is plotted.
        \\*\\*kwargs:
            Any keyword args supported by matplotlib.pyplot.plot.

    Returns:
        A matplotlib.pyplot object. Do plt.show() to show the plot.
    """
    from pymatgen.core.structure import IStructure
    from pymatgen.util.plotting import pretty_plot
    plt.figure(figsize=(9, 6))
    for i in range(len(total_temps)):
        temps = total_temps[i]
        diffusivities = total_diffusivities[i]
        label = names[i]
        print(label)

        Ea, c, std = fit_arrhenius(temps, diffusivities)
        print("std: ", std)
        print("Ea : ", Ea)

        # log10 of the arrhenius fit
        arr = c * np.exp(-Ea / (const.k / const.e * np.array(temps)))
        d_2000 = c * np.exp(-Ea / (const.k / const.e * 2000.0))
        d_300 = c * np.exp(-Ea / (const.k / const.e * 300.0))

        t_1 = 1000 / np.array(temps)

        plt.scatter(t_1, diffusivities, marker='o', s=150, linewidths=3, facecolors='none', edgecolors=colors[i], label=label)
        plt.plot([1000./2000., 1000./300.], [d_2000, d_300], ls='--', color=colors[i])

        if diffusivity_errors is not None:
            n = len(diffusivity_errors)
            plt.errorbar(t_1[0:n], diffusivities[0:n], yerr=diffusivity_errors,
                         fmt='ko', ecolor='k', capthick=2, linewidth=2)
        ax = plt.axes()
        ax.set_yscale('log')
        # plt.text(0.6, 0.85, "E$_a$ = {:.0f} meV".format(Ea * 1000),
        #          fontsize=30, transform=plt.axes().transAxes)
        plt.xlim(0.5, 3)
        plt.ylabel("D (cm$^2$/s)", fontsize=24)
        plt.xlabel("1000/T (K$^{-1}$)", fontsize=24)

        plt.tick_params(axis='both', which='major', labelsize=16)
        plt.legend(loc=1, prop={'size': 18})
        plt.tight_layout()

        structure = IStructure.from_file(cifs[i])
        ext_c = get_extrapolated_conductivity(temps, diffusivities, new_temp=300, structure=structure, species="Li")
        print("Conductivity at 300K: %.10f" % ext_c)

    return plt

def diffusivity_plotter(csv_files, xaxis):
    """
    Plot diffusivity from anlysis code of Xingfeng He

    """
    T = []
    total_d = []
    total_d_err = []
    total_std = []

    plt.figure(figsize=(8, 6))
    for i, f in enumerate(files):
        # 850K/Mo_850K_data.csv
        # step,std,diffusivity,diffusivity_err
        label = f.split("/")[-1].split("_")[1]
        temp = label.replace("K", "")
        df = pd.read_csv(f, index_col=False)
        runstep = np.array(df['step'].tolist())
        if xaxis == 'r':
            x = runstep
        else:
            x = runstep * 2
        d = np.array(df['diffusivity'].tolist())
        d_err = np.array(df['diffusivity_err'].tolist())
        d_err_up = d + d_err
        d_err_dn = d - d_err
        try:
            # handle old version
            RSD = np.array(df['std'].tolist())
        except:
            RSD = np.array(df['RSD'].tolist())

        T.append(temp)
        total_d.append(str("%.12f" % d[-1]))
        total_d_err.append(str("%.12f" % d_err[-1]))
        total_std.append(str("%.4f" % RSD[-1]))

        #plt.plot(runstep, d, marker='o', ms='3', color=colors[i], label=label)
        plt.plot(x, d, color=colors[i], lw=2, label=label)
        plt.fill_between(x, d_err_up, d_err_dn, facecolor=colors[i], alpha=0.3)

    ax = plt.axes()
    ax.set_yscale('log')
    if xaxis == 'r':
        plt.xlabel("Run step", fontsize=24)
    else:
        plt.xlabel("Time step (ps)", fontsize=24)
    plt.ylabel(r"Diffusivity (cm$^2$/s)", fontsize=24)
    plt.tick_params(axis='both', which='major', labelsize=16)
    plt.grid()
    plt.legend(loc=1, prop={'size': 16})
    plt.tight_layout()
    plt.savefig("diffusivity_data.png")
    plt.show()

    data = {'T': T, 'D': total_d, 'D_error': total_d_err, 'RSD': total_std}
    df = pd.DataFrame(data)
    print("Final steps at each T")
    tab = tabulate(df, headers='keys', tablefmt='psql', floatfmt=("", ".12f", ".12f", ".4f"), showindex=False)
    print(tab)

    df.to_csv("diffusivity_data.csv")
    f = open("diffusivity_data.txt", "w")
    f.write(str(tab))
    f.close()

def msd_plotter(csv_files, log):
    T = []
    total_d = []
    total_d_err = []

    plt.figure(figsize=(8, 6))
    for i, f in enumerate(files):
        # dt (fs),msd (A^2)
        label = f.split("/")[-1].split("_")[1].replace(".csv", "")
        temp = label.replace("K", "")
        df = pd.read_csv(f, index_col=False)
        dt_fs = np.array(df['dt (fs)'].tolist())
        dt_ps = dt_fs * 0.001
        msd = np.array(df['msd (A^2)'].tolist())

        plt.plot(dt_ps, msd, color=colors[i], lw=2, label=label)

    if log:
        ax = plt.axes()
        ax.set_xscale('log')
        ax.set_yscale('log')
    plt.xlabel("Time step (ps)", fontsize=24)
    plt.ylabel(r"MSD (cm$^2$/s)", fontsize=24)
    plt.ylabel(r"MSD ($\mathrm{\AA}^2$)", fontsize=24)
    plt.tick_params(axis='both', which='major', labelsize=16)
    plt.grid()
    plt.legend(loc=1, prop={'size': 16})
    plt.tight_layout()
    plt.savefig("msd.png")
    plt.show()

def arrhenius_plotter(csv_files, specie="Li", temp=300, show_room_temp=True):
    from CCpy.Package.Diffusion.aimd.diffusion import ArreheniusAnalyzer
    from pymatgen.core.structure import IStructure
    crt_ymin = 9999
    crt_ymax = -9999
    total_label = []
    total_Ea = []
    total_Ea_err = []
    total_ext_diffusivity = []
    total_rng_diffusivity_from = []
    total_rng_diffusivity_to = []
    total_ext_conductivity = []
    total_rng_conductivity_from = []
    total_rng_conductivity_to = []
    plt.figure(figsize=(8, 6))
    for i in range(len(csv_files)):
        aa = ArreheniusAnalyzer.from_csv(csv_files[i])
        structure = IStructure.from_file(csv_files[i].replace(".csv", ".cif"))
        label = csv_files[i].replace(".csv", "")

        Ea = aa.Ea
        Ea_err = aa.Ea_error

        prd_diffusivity = aa.predict_diffusivity(temp)
        ext_diffusivity = prd_diffusivity[0]
        rng_diffusivity = prd_diffusivity[1]

        prd_conductivity = aa.predict_conductivity(temp, structure, specie)
        ext_conductivity = prd_conductivity[0]
        rng_conductivity = prd_conductivity[1]

        total_label.append(label)
        total_Ea.append(Ea)
        total_Ea_err.append(Ea_err)
        total_ext_diffusivity.append(ext_diffusivity)
        total_rng_diffusivity_from.append(rng_diffusivity[0])
        total_rng_diffusivity_to.append(rng_diffusivity[1])
        total_ext_conductivity.append(ext_conductivity)
        total_rng_conductivity_from.append(rng_conductivity[0])
        total_rng_conductivity_to.append(rng_conductivity[1])

        #(_, caps, _) = plt.errorbar([1000./temp], [ext_diffusivity], yerr=[rng_diffusivity], fmt='none', color=colors[i], capsize=5)
        #for cap in caps:
        #    cap.set_markeredgewidth(2)

        ymin, ymax = aa.get_custom_arrhenius_plot(colors[i], label, markers[i], temp, show_room_temp, rng_diffusivity)
        #ymin, ymax = aa.get_custom_arrhenius_plot(colors[i], label, markers[i], temp, show_room_temp)
        crt_ymin = min(crt_ymin, ymin)
        crt_ymax = max(crt_ymax, ymax)

    if show_room_temp:
        plt.xlim(0.75, 3.5)
    else:
        plt.xlim(0.75, 2.0)
    plt.ylim(crt_ymin, crt_ymax)

    ax = plt.axes()
    ax.set_yscale('log')

    plt.ylabel("D (cm$^2$/s)", fontsize=24)
    plt.xlabel("1000/T (K$^{-1}$)", fontsize=24)
    plt.tick_params(axis='both', which='major', labelsize=16)
    #plt.legend(loc=1, prop={'size': 16})
    plt.legend(prop={'size': 16})
    plt.tight_layout()
    plt.savefig("arrhenius_fit.png")
    plt.show()

    data = {'Name': total_label, 'Ea (eV)': total_Ea, 'Ea_err (+/-)': total_Ea_err,
            'ext_D (cm^2/s)': total_ext_diffusivity, 'D_err_from': total_rng_diffusivity_from, 'D_err_to': total_rng_diffusivity_to,
            'ext_c (mS/cm)': total_ext_conductivity, 'c_err_from': total_rng_conductivity_from, 'c_err_to': total_rng_conductivity_to}

    df = pd.DataFrame(data)
    tab = tabulate(df, headers='keys', tablefmt='psql', floatfmt=("", ".6f", ".6f", ".12f", ".12f", ".12f", ".6f", ".6f", ".6f"), showindex=False)
    print(tab)
    df.to_csv("arrhenius_fit.csv")
    f = open("arrhenius_fit.txt", "w")
    f.write(str(tab))
    f.close()

if __name__ == "__main__":
    # --- Parsing sub options
    mode = "constant"
    xaxis = "t"
    log = True
    specie = "Li"
    temp = 300
    show_temp = True
    for arg in sys.argv:
        if "-m=" in arg:
            mode = arg.split("=")[1]
            if mode == "False":
                mode = False
        if "-x=" in arg:
            xaxis = arg.split("=")[1]
        if "-log=" in arg:
            log = arg.split("=")[1]
            if log == "False":
                log = False
        if '-specie=' in arg:
            specie = arg.split("=")[1]
        if '-T=' in arg:
            temp = float(arg.split("=")[1])
        if '-ns' in arg:
            show_temp = False


    # --- Go to main option
    if sys.argv[1] == "1":
        files = get_csvfiles()
        diffusivity_plotter(files, xaxis)
    elif sys.argv[1] == "2":
        files = get_csvfiles()
        msd_plotter(files, log)
    elif sys.argv[1] == "3":
        files = get_csvfiles()
        arrhenius_plotter(files, specie=specie, temp=temp, show_room_temp=show_temp)
    elif sys.argv[1] == "4":
        files = get_csvfiles()
        plot_diffusivity(mode, files, xaxis)
