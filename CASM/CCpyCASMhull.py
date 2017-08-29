#!/bin/env python

import os, sys
import subprocess
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from CCpy.Tools.CCpyTools import find_convex_hull
from CCpy.VASP.VASPio import VASPOutput


class CASMhull():
    def __init__(self, base=None, tot_base=None):
        pwd = os.getcwd()  # current directory
        self.pwd, self.base, self.tot_base = pwd, base, tot_base
        if "Data" not in os.listdir("./"):
            os.mkdir("Data")

    def parsingData(self):
        pwd = self.pwd
        base, tot_base, = self.base, self.tot_base

        # -- supercell info
        scel = open("SCEL", "r").read()
        scel = scel.split("\n")
        supcell = []
        for i in range(len(scel)):
            if "volume" in scel[i]:
                supcell.append(float(scel[i].split()[-1]))

        cons = []  # Concentrations
        energies = []  # Energies
        dirnames = []  # Directory names
        con0_energy = None  # 0.0 Concentration energy (for calculating formation energy)
        con1_energy = None  # 1.0 Concentration energy (for calculating formation energy)

        sub_ds = [sd for sd in os.listdir("./") if os.path.isdir(sd) and "con" in sd]
        sub_ds.sort()
        # -- sub_ds = [cons0.0, cons1.0, ..]
        for sd in sub_ds:
            os.chdir(sd)
            scel_index = int(sd.split(".")[0].replace("con", ""))  # con[index].xx
            vol = supcell[scel_index]  # supcell:[1, 2, 2, 3, 4, ..]

            # -- find elements
            f = open("POSCAR", "r")
            lines = f.readlines()
            f.close()

            elts_dict = {}
            coord_index = 9999
            for i in range(len(lines)):
                if "Direct" in lines[i]:
                    coord_index = i + 1
                if i >= coord_index and len(lines[i].split()) == 4:
                    elt = lines[i].replace("\n", "").split()[-1]
                    if elt not in elts_dict.keys():
                        elts_dict[elt] = 1
                    else:
                        elts_dict[elt] += 1

            elts = elts_dict.keys()

            if base in elts:
                index = elts.index(base)
                n_of_base = float(elts_dict[base])
                con = round(n_of_base / (tot_base * vol), 6)
                cons.append(con)
            else:
                con = 0.0
                cons.append(con)

            # -- find energy
            popen = os.popen("grep \"free  energy   TOTEN\" OUTCAR").readlines()
            e = float(popen[-1].split()[-2]) / vol
            energies.append(e)
            if con == 0.0:
                con0_energy = e
            elif con == 1.0:
                con1_energy = e

            # -- dirname
            crr_dir = os.getcwd().split("/")[-1]

            dirnames.append(crr_dir)
            os.chdir("../")

        data = {"Concentration": cons, "Directory": dirnames, "Energy": energies}
        df = pd.DataFrame(data)

        self.df = df
        self.con1_energy, self.con_energy = con1_energy, con0_energy

    def getFormData(self):
        df = self.df
        con1_energy, con0_energy = self.con1_energy, self.con_energy

        # -- Formation energy = E - xE(Lix) - (1-x)ELi(1-x)
        df['Formation energy'] = df['Energy'] - df['Concentration'] * con1_energy - (1.0 - df[
            'Concentration']) * con0_energy
        df = df.sort_values(by='Concentration')
        df.to_csv("./Data/01_formation_energy.csv")
        print("Data saved : ./Data/01_formation_energy.csv")

        self.df = df

    def makeHull(self):
        df = self.df

        cons = df['Concentration'].tolist()
        fes = df['Formation energy'].tolist()
        points = []
        for i in range(len(cons)):
            points.append([cons[i], fes[i]])
        points = np.array(points)

        # -- Generate convex hull using scipy
        hull_data = find_convex_hull(points)

        # --
        all_x, all_y, all_d = df['Concentration'].tolist(), df['Formation energy'].tolist(), df['Directory'].tolist()
        x, y = hull_data['x'], hull_data['y']

        hullpoint_info = {"Concentration": [], "Formation energy": [], "Directory": []}
        for i in range(len(all_x)):
            for j in range(len(x)):
                if x[j] == all_x[i] and y[j] == all_y[i] and all_y[i] <= 0:
                    hullpoint_info['Concentration'].append(all_x[i])
                    hullpoint_info['Formation energy'].append(all_y[i])
                    hullpoint_info['Directory'].append(all_d[i])

        hull_df = pd.DataFrame(hullpoint_info)
        hull_df = hull_df[['Concentration', 'Formation energy', 'Directory']]

        hull_df.to_csv("./Data/02_convex_hull_points.csv")
        print("Data saved : ./Data/02_convex_hull_points.csv")
        print("\n* Hull points")
        print(hull_df)

        self.hull_df = hull_df

    def plotHull(self):
        df, hull_df = self.df, self.hull_df

        # -- plot
        plt.scatter(df['Concentration'], df['Formation energy'], marker="D", color='b', s=10)
        plt.plot(hull_df['Concentration'], hull_df['Formation energy'], marker='o', color="r", alpha=0.7, ms=8)

        plt.xlim(0.0, 1.0)
        plt.xlabel(base + " Concentration", fontsize=20)
        plt.ylabel("Formation energy (eV/f.u.)", fontsize=20)

        os.chdir("./Data/")
        figname = "03_convexhull.png"
        jpg = figname.replace(".png", ".jpg")
        plt.savefig(figname)
        print("\nFigure saved : ./Data/" + figname)
        makejpg = "convert " + figname + " " + jpg
        subprocess.call(makejpg, shell=True)
        print("Figure saved : ./Data/" + jpg)

        plt.show()

        os.chdir("../")

    def getHullPointStructures(self):
        hull_df = self.hull_df
        hull_point_dirs = hull_df['Directory'].tolist()

        for d in hull_point_dirs:
            os.chdir(d)
            VO = VASPOutput()
            VO.getFinalStructure(path="../Data/")
            os.chdir("../")

    def mainFlow(self):
        self.parsingData()
        self.getFormData()
        self.makeHull()
        self.plotHull()
        self.getHullPointStructures()


if __name__ == "__main__":
    # -- Get info ------------
    base = raw_input("\n* Element name (ex: Li) : ")  # base atom
    tot_base = raw_input(
        "* Number of " + base + " when full (ex: 9) in unit cell: ")  # the number of base atoms in unit cell
    tot_base = float(tot_base)

    ch = CASMhull(base=base, tot_base=tot_base)
    ch.mainFlow()







