#!/bin/env python
import os, sys
import pandas as pd
import matplotlib.pyplot as plt

from pymatgen.analysis.diffusion_analyzer import DiffusionAnalyzer

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
          
2   : Plot MSD/2dt
      ex) CCpyNVTLoopAnal.py 2 -m=False 600K/run01/vasprun.xml
          CCpyNVTLoopAnal.py 2 -m=False 600K/run01/vasprun.xml 600K/run02/vasprun.xml   (multiple files also available)

[suboptions]
-m  : Define smoothing mode of MSD/2dt (False, constant, max)
      ex) CCpyNVTLoopAnal.py 1 -m=False 600K/data_600K.csv
          CCpyNVTLoopAnal.py 2 -m=max 600K/run01/vasprun.xml
"""
          )
    quit()

for arg in sys.argv:
    if "-m=" in arg:
        mode = arg.split("=")[1]

files = sys.argv[3:]

def plot_diffusivity(mode, files):
    if mode == "False":
        xlabel = "diffusivity(F)"
    elif mode == "constant":
        xlabel = "diffusivity(c)"
    elif mode == "max":
        xlabel = "diffusivity(m)"
    else:
        print("Smoothing mode is not defined. (-m=option)")

    plt.figure(figsize=(10, 7))
    for filename in files:
        df = pd.read_csv(filename, index_col=False)
        print(filename)
        print(df)
        print("-" * 20)

        #x = df['timestep']
        x = df['run step']
        y = df[xlabel]

        label = filename.split("/")[-1].replace("data_","").replace(".csv","")
        plt.plot(x, y, marker='o', label=label)

    plt.xlabel("Run step", fontsize=24)
    #plt.xlabel("Time step (ps)", fontsize=24)
    plt.ylabel(r"Diffusivity (cm$^2$/s)", fontsize=24)
    plt.tick_params(axis='both', which='major', labelsize=14)
    plt.grid()
    plt.legend(prop={'size': 16})
    plt.tight_layout()
    plt.savefig("Diffusivity_%s.png" % mode)
    plt.show()

def plot_msd(mode, vaspruns):
    """
    Create MSD/t plot run00 to current run.
    And save diffusivity and conductivity of current step
    """
    if mode == "False":
        mode = False
    analyzer = DiffusionAnalyzer.from_files(vaspruns, specie="Li", smoothed=mode)
    plt = analyzer.get_msd_plot()
    plt.savefig("msd.png")
    plt.show()

    print("Diffusivity : %.10f" % analyzer.diffusivity)
    print("Conductivity: %.4f" % analyzer.conductivity)

if sys.argv[1] == "1":
    plot_diffusivity(mode, files)
elif sys.argv[1] == "2":
    plot_msd(mode, files)
