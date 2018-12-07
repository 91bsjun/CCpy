#!/bin/env python
import os, sys
import numpy as np
import scipy.constants as const
import pandas as pd
import matplotlib.pyplot as plt
import pickle
import collections

from pymatgen.analysis.diffusion_analyzer import DiffusionAnalyzer, fit_arrhenius, get_arrhenius_plot, get_extrapolated_conductivity, get_extrapolated_diffusivity

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
1   : Plot diffusivity(ies) written in *K/data.csv
      ex) CCpyNVTLoopAnal.py 1 -m=constant 600K/data_600K.csv
          CCpyNVTLoopAnal.py 1 -m=max *K/data*    (multiple files also available)
          CCpyNVTLoopAnal.py 1 -m=max -x=r 600K/data_600K.csv 800K/data_800K.csv    (use x-axis as runstep)          
          
2   : Plot MSD/2dt
      ex) CCpyNVTLoopAnal.py 2 -m=False 
          
3   : Analysis data


[suboptions]
-m=[mode]    : Define smoothing mode of MSD/2dt (False, constant, max / Default: max)
               ex) CCpyNVTLoopAnal.py 1 -m=False 600K/data_600K.csv
                   CCpyNVTLoopAnal.py 2 -m=max 600K/run01/vasprun.xml

-x=[t or r]  : Define x-axis label type when plot diffusivity vs run
               t for timestep, r for runstep (Default: t)
               ex) CCpyNVTLoopAnal.py 1 -m=False -x=r
"""
          )
    quit()


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
    colors = ["#0054FF", "#DB0000", "#00A500", "#FF7012", "#5F00FF", "#000000", "#00D8FF", "#FF00DD"]
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

        label = filename.split("/")[-1].replace("data_","").replace(".csv","")
        plt.plot(x, y, marker='o', color=colors[i], label=label)

    if xaxis == 'r':
        plt.xlabel("Run step", fontsize=24)
    else:
        plt.xlabel("Time step (ps)", fontsize=24)
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

def arrhenius_plotter(datafiles):
    total_temps = []
    total_diffusivities = []
    labels = []
    for datafile in datafiles:
        df = pd.read_csv(datafile)
        keys = df.keys()
        temps = df['T'].tolist()
        for key in keys:
            if key != 'T':
                total_temps.append(temps)
                total_diffusivities.append(df[key].tolist())
                labels.append(key)

    plt = custom_arrhenius_plot(total_temps, total_diffusivities, labels)
    plt.show()




def custom_arrhenius_plot(total_temps, total_diffusivities, labels, diffusivity_errors=None,
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
    colors = ["#0054FF", "#DB0000", "#00A500", "#FF7012", "#5F00FF", "#000000", "#00D8FF", "#FF00DD"]
    from pymatgen.util.plotting import pretty_plot
    plt.figure(figsize=(9, 6))
    for i in range(len(total_temps)):
        temps = total_temps[i]
        diffusivities = total_diffusivities[i]
        label = labels[i]

        Ea, c, _ = fit_arrhenius(temps, diffusivities)

        # log10 of the arrhenius fit
        arr = c * np.exp(-Ea / (const.k / const.e * np.array(temps)))
        d_2000 = c * np.exp(-Ea / (const.k / const.e * 2000.0))
        d_300 = c * np.exp(-Ea / (const.k / const.e * 300.0))

        t_1 = 1000 / np.array(temps)

        plt.scatter(t_1, diffusivities, marker='o', s=150, linewidths=2, facecolors='none', edgecolors=colors[i], label=label)
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

    return plt



if __name__ == "__main__":
    # --- Parsing sub options
    mode = "max"
    xaxis = "t"
    for arg in sys.argv:
        if "-m=" in arg:
            mode = arg.split("=")[1]
            if mode == "False":
                mode = False
        if "-x=" in arg:
            xaxis = arg.split("=")[1]

    # --- Go to main option
    if sys.argv[1] == "1":
        files = get_csvfiles()
        plot_diffusivity(mode, files, xaxis)
    elif sys.argv[1] == "2":
        vaspruns = get_vaspruns()
        plot_msd(mode, vaspruns)
    elif sys.argv[1] == "3":
        analysis()
    elif sys.argv[1] == "4":
        files = get_csvfiles()
        arrhenius_plotter(files)
