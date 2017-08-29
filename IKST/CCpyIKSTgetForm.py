#!/bin/env python

import os, sys
import subprocess
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from CCpy.Tools.CCpyTools import find_convex_hull
from CCpy.VASP.VASPio import VASPOutput


class IKSThull():
    def __init__(self, dirname=None, base=None, tot_base=None):
        pwd = os.getcwd()  # current directory
        self.root = dirname  # working directory

        self.pwd, self.base, self.tot_base = pwd, base, tot_base

        os.chdir(dirname)
        if "Data" not in os.listdir("./"):
            os.mkdir("Data")

    def parsingData(self):
        pwd = self.pwd
        base, tot_base, = self.base, self.tot_base

        cons = []  # Concentrations
        energies = []  # Energies
        energies_fu = []
        dirnames = []  # Directory names
        base_atoms = []  # number of base atom
        supercells = []  # supercell
        con0_energy = None  # 0.0 Concentration energy (for calculating formation energy)
        con1_energy = None  # 1.0 Concentration energy (for calculating formation energy)

        dirs = [d for d in os.listdir("./") if os.path.isdir(d)]
        # -- scel_dirs = [1-1-1, 1-2-1, ...]
        scel_dirs = []
        for d in dirs:
            if len(d.split("-")) == 3:
                scel_dirs.append(d)
        scel_dirs.sort()
        # -- scel_dirs = [1-1-1, 1-2-1, ...]
        for scd in scel_dirs:
            os.chdir(scd)
            scel = scd.split("-")
            scel = float(scel[0]) * float(scel[1]) * float(scel[2])
            # -- dirs = [Co3Mn3Ni3O18Vac9, ...]
            dirs = [d for d in os.listdir("./") if os.path.isdir(d)]
            dirs.sort()
            print("\n Start parsing ...\n")
            for d in dirs:
                os.chdir(d)
                print(os.getcwd())
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
                        con = round(n_of_base / tot_base, 6)
                        cons.append(con)
                        base_atoms.append(n_of_base)
                    else:
                        con = 0.0
                        cons.append(con)
                        base_atoms.append(0)

                    # -- find energy
                    popen = os.popen("grep \"free  energy   TOTEN\" OUTCAR").readlines()
                    e = float(popen[-1].split()[-2])
                    e_fu = e / scel
                    energies.append(e)
                    energies_fu.append(e_fu)

                    if con == 0.0:
                        con0_energy = e_fu
                    elif con == 1.0:
                        con1_energy = e_fu

                    # -- dirname
                    spl_dir = os.getcwd().split("/")
                    crr_dir = spl_dir[-3] + "/" + spl_dir[-2] + "/" + spl_dir[-1]
                    dirnames.append(crr_dir)

                    supercells.append(scel)

                    os.chdir("../")
                os.chdir("../")
            os.chdir("../")

        data = {"Concentration": cons, "Directory": dirnames, "Energy": energies, "Energy/f.u.": energies_fu,
                "Supercell": supercells, "Number of atom": base_atoms}
        df = pd.DataFrame(data)

        self.df = df
        self.con1_energy, self.con_energy = con1_energy, con0_energy

    def getFormData(self):
        df = self.df
        con1_energy, con0_energy = self.con1_energy, self.con_energy

        # -- Formation energy = E - xE(Lix) - (1-x)ELi(1-x)
        df['Formation energy'] = df['Energy/f.u.'] - df['Concentration'] * con1_energy - (1.0 - df[
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

        # -- Collect hull point from total data frame
        concat = []
        x, y = hull_data['x'], hull_data['y']

        for i in range(len(x)):
            x_df = df[(df['Concentration'] == x[i])]
            xy_df = x_df[(x_df['Formation energy'] == y[i])]
            xy_df = xy_df[(xy_df['Formation energy'] <= 0)]
            concat.append(xy_df)

        hull_df = pd.concat(concat)
        hull_df = hull_df[
            ['Concentration', 'Formation energy', 'Energy', 'Energy/f.u.', 'Supercell', 'Number of atom', 'Directory']]

        hull_df.to_csv("./Data/02_convex_hull_points.csv")
        print("Data saved : ./Data/02_convex_hull_points.csv")
        print("\n* Hull points")
        pd.set_option('expand_frame_repr', False)

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
        crr_dir = os.getcwd()
        for d in hull_point_dirs:
            os.chdir(d)
            VO = VASPOutput()
            VO.getFinalStructure(path=crr_dir + "/Data/")

            spl_dir = os.getcwd().split("/")
            filename = spl_dir[-1] + "_contcar.cif"

            new_filename = spl_dir[-3] + "_" + spl_dir[-2] + "_" + spl_dir[-1] + "_contcar.cif"

            os.chdir(crr_dir)
            mv = "mv ./Data/" + filename + " ./Data/" + new_filename
            subprocess.call(mv, shell=True)

    def getVoltageProfile(self):
        df = self.hull_df
        cons = df['Concentration'].tolist()
        energies = df['Energy'].tolist()
        supercells = df['Supercell'].tolist()
        n_of_atoms = df['Number of atom'].tolist()

        x = []
        y = []
        for i in range(len(energies)):
            j = i + 1
            if j < len(energies):
                e1 = energies[i] * supercells[j]
                e2 = energies[j] * supercells[i]

                n1 = n_of_atoms[i] * supercells[j]
                n2 = n_of_atoms[j] * supercells[i]

                diff_Li = n2 - n1

                vol = ((e1 - e2) / diff_Li) - 1.886

                if i == 0:
                    x.append(cons[i])
                else:
                    x.append(cons[i])
                    x.append(cons[i])
                y.append(vol)
                y.append(vol)
        x.append(1.0)

        voltage = {'Concentration': x, 'Voltage': y}
        voltage_df = pd.DataFrame(voltage)
        voltage_df.to_csv("./Data/04_voltage_profile.csv")
        print("\nData saved : ./Data/04_voltage_profile.csv")

        plt.plot(x, y, lw=2)

        plt.xlim(0.0, 1.0)
        plt.xlabel(base + " Concentration", fontsize=20)
        plt.ylabel("Voltage (V)", fontsize=20)

        os.chdir("./Data/")
        figname = "05_voltage_profile.png"
        jpg = figname.replace(".png", ".jpg")
        plt.savefig(figname)
        print("\nFigure saved : ./Data/" + figname)
        makejpg = "convert " + figname + " " + jpg
        subprocess.call(makejpg, shell=True)
        print("Figure saved : ./Data/" + jpg)

        plt.show()

    def mainFlow(self):
        self.parsingData()
        self.getFormData()
        self.makeHull()
        self.plotHull()
        self.getHullPointStructures()
        self.getVoltageProfile()


if __name__ == "__main__":
    # -- Get working directory
    try:
        root = sys.argv[1]
    except:
        root = raw_input("* Directory name : ")
        print("* Notice : You can start by '" + sys.argv[0] + " " + root + "'")
    root = root.replace("/", "")

    # -- Get info ------------
    base = raw_input("\n* Element name (ex: Li) : ")  # base atom
    tot_base = raw_input("* Number of " + base + " when full (ex: 9) : ")  # the number of base atoms in cell
    tot_base = float(tot_base)

    ih = IKSThull(dirname=root, base=base, tot_base=tot_base)
    ih.mainFlow()







