#!/usr/bin/env python

import os,sys
import pandas as pd
import numpy as np
from CCpy.Tools.CCpyTools import find_convex_hull

try:
    root = sys.argv[1]
except:
    root = raw_input("Directory name (contained DBinfo files) ?")
os.chdir(root)

# -- User Handle Area ----
base = "Li"
tot_base = 9        # the number of base in unit cell
# ------------------------

cons = []           # Concentrations
energies = []       # Energies
dirnames = []       # Directory names
con0_energy = None  # 0.0 Concentration energy (for calculating formation energy)
con1_energy = None  # 1.0 Concentration energy (for calculating formation energy)

dbinfo_files = [f for f in os.listdir("./") if "DBinfo" in f]
for dbinfo in dbinfo_files:
    f = open(dbinfo, "r")
    lines = f.readlines()
    f.close()
    es = [] # all energies in each DBinfo file
    for l in lines:
        # -- Find element list
        if "Element List" in l:
            elts = l.split("=")[1]
            elts = elts.split()
            index = False
            cnt = 0
            for e in elts:
                if e == base:
                    index = cnt
                cnt += 1
        # -- Find the number of each element
        elif "Number of atoms" in l:
            nums = l.split("=")[1]
            nums = nums.split()
            if index:
                n_of_base = float(nums[index])
            else:
                n_of_base = 0.0
            con = round(n_of_base / tot_base, 6)
            cons.append(con)
        # -- Find total energies (All iteration)
        elif "Total Energy" in l:
            tot_e = float(l.split()[5])
            es.append(tot_e)
        elif "Run Dir" in l:
            rundir = l.split("=")[1]
            rundir = rundir.replace(" ","").replace("\n","")
    energies.append(es[0])
    if con == 0.0:
        con0_energy = es[0]
    elif con == 1.0:
        con1_energy = es[0]

    dirnames.append(rundir)

os.chdir("../")

data = {"Concentration": cons, "Directory": dirnames, "Energy": energies}
df = pd.DataFrame(data)

# -- Formation energy = E - xE(Lix) - (1-x)ELi(1-x)
df['Formation energy'] = df['Energy'] - df['Concentration']*con1_energy - (1.0 - df['Concentration'])*con0_energy
df = df.sort(['Concentration','Formation energy'], ascending=[True,True])
df.to_csv("Formation_energy.csv")

cons = df['Concentration'].tolist()
fes = df['Formation energy'].tolist()
points = []
for i in range(len(cons)):
    points.append([cons[i],fes[i]])
points = np.array(points)

# -- Generate convex hull using scipy
hull_data = find_convex_hull(points)
hull_df = pd.DataFrame(hull_data)
hull_df = hull_df.sort(['x','y'], ascending=[True,True])
hull_df.to_csv("Convex_hull_points.csv")

# -- Plot
ifplot = raw_input("plot? (y/n)")
if ifplot == "y":
    import matplotlib.pyplot as plt
    plt.scatter(df['Concentration'], df['Formation energy'], marker="D", color='b', s=10)
    plt.plot(hull_df['x'], hull_df['y'], marker='o', color="r")
    plt.xlim(0.0,1.0)
    plt.savefig("Convexhull.png")
    plt.show()
