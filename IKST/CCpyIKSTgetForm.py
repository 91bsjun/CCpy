#!/usr/local/bin/python2.7

import os,sys
import pandas as pd
import numpy as np
from CCpy.Tools.CCpyTools import find_convex_hull

try:
    root = sys.argv[1]
except:
    root = raw_input("Directory name ?")
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

# -- supcells = [1-1-1, 1-2-1, ...]
supcells = [d for d in os.listdir("./") if os.path.isdir(d)]
if len(supcells) > 1:
    print("This script should be modified in case of multi supecell systems.")
    quit()
for supcell in supcells:
    os.chdir(supcell)
    dirs = os.listdir("./")
    dirs.sort()
    # -- dirs = [Co3Mn3Ni3O18Vac9, ...]
    for d in dirs:
        os.chdir(d)
        sub_ds = os.listdir("./")
        # -- sub_ds = [c0001, ..]
        for sd in sub_ds:
            os.chdir(sd)
            files = [f for f in os.listdir("./")]
            for f in files:
                if "DBinfo" in f:
                    dbinfo = f
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
            energies.append(es[0])
            if con == 0.0:
                con0_energy = es[0]
            elif con == 1.0:
                con1_energy = es[0]

            dirname = os.getcwd()
            dirname = dirname.split("/")
            dirname = dirname[-3] + "/" + dirname[-2] + "/" + dirname[-1]
            dirnames.append(dirname)

            os.chdir("../")
        os.chdir("../")
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
    plt.scatter(df['Concentration'], df['Formation energy'], marker="D")
    plt.plot(hull_df['x'], hull_df['y'], marker='o', color="r")
    plt.show()
