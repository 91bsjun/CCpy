#!/usr/bin/env python

import os,sys
import pandas as pd
import numpy as np
from CCpy.Tools.CCpyTools import find_convex_hull

try:
    root = sys.argv[1]
except:
    root = raw_input("Directory name : ")
pwd = os.getcwd()
os.chdir(root)

# -- Get val from user ----
base = raw_input("Element name (ex: Li) : ") # base atom
tot_base = raw_input("Number of " + base + " when full (ex: 9) : ") # the number of base atoms in cell
tot_base = float(tot_base)
# -------------------------

cons = []           # Concentrations
energies = []       # Energies
dirnames = []       # Directory names
con0_energy = None  # 0.0 Concentration energy (for calculating formation energy)
con1_energy = None  # 1.0 Concentration energy (for calculating formation energy)

# -- supcells = [1-1-1, 1-2-1, ...]
supcells = [d for d in os.listdir("./") if os.path.isdir(d)]
if len(supcells) > 1:
    print("only 1 supercell is now available..")
    quit()
os.chdir(supcells[0])
# -- dirs = [Co3Mn3Ni3O18Vac9, ...]
dirs = [d for d in os.listdir("./") if os.path.isdir(d)]
dirs.sort()
for d in dirs:
    os.chdir(d)
    sub_ds = [sd for sd in os.listdir("./") if os.path.isdir(sd)]
    # -- sub_ds = [c0001, ..]
    for sd in sub_ds:
        os.chdir(sd)

        # -- find elements
        f = open("POSCAR", "r")
        lines = f.readlines()
        f.close()

        for i in range(len(lines)):
            if i == 5:
                elts = lines[i].split()
            elif i == 6:
                n_of_atoms = lines[i].split()

        if base in elts:
            index = elts.index(base)
            n_of_base = float(n_of_atoms[index])
            con = round(n_of_base / tot_base, 4)
            cons.append(con)
        else:
            con = 0.0
            cons.append(con)


        # -- find energy
        popen = os.popen("grep \"free  energy   TOTEN\" OUTCAR").readlines()
        e = float(popen[-1].split()[-2])
        energies.append(e)
        if con == 0.0:
            con0_energy = e
        elif con == 1.0:
            con1_energy = e


        # -- dirname
        spl_dir = os.getcwd().split("/")
        crr_dir = spl_dir[-3] + "/" + spl_dir[-2] + "/" + spl_dir[-1]
        dirnames.append(crr_dir)

        os.chdir("../")
    os.chdir("../")
os.chdir(pwd)


data = {"Concentration": cons, "Directory": dirnames, "Energy": energies}
df = pd.DataFrame(data)

# -- Formation energy = E - xE(Lix) - (1-x)ELi(1-x)
df['Formation energy'] = df['Energy'] - df['Concentration'] * con1_energy - (1.0 - df['Concentration']) * con0_energy
df = df.sort_values(by='Concentration')
df.to_csv("01" + root + "_formation_energy.csv")

cons = df['Concentration'].tolist()
fes = df['Formation energy'].tolist()
points = []
for i in range(len(cons)):
    points.append([cons[i], fes[i]])
points = np.array(points)

# -- Generate convex hull using scipy
hull_data = find_convex_hull(points)
hull_df = pd.DataFrame(hull_data)
hull_df = hull_df.sort_values(by='x')
hull_df = hull_df[hull_df['y'] <= 0]
hull_df.to_csv("02" + root + "_convex_hull_points.csv")

# -- Plot
ifplot = raw_input("plot? (y/n)")
if ifplot == "y":
    import matplotlib.pyplot as plt

    plt.scatter(df['Concentration'], df['Formation energy'], marker="D", color='b', s=10)
    plt.plot(hull_df['x'], hull_df['y'], marker='o', color="r", alpha=0.8)
    plt.xlim(0.0, 1.0)
    plt.xlabel(base + " Concentration")
    plt.ylabel("Formation energy (eV)")
    plt.savefig("03" + root + "_convexhull.png")
    plt.show()
