#!/bin/env python

import os, sys
import subprocess
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from CCpy.Tools.CCpyTools import find_convex_hull
from CCpy.VASP.VASPio import VASPOutput

from pymatgen.core.structure import IStructure
from pymatgen.core.periodic_table import Element


version = sys.version
if version[0] == '3':
    raw_input = input

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

        self.supcell = supcell

        cons = []  # Concentrations
        energies = []  # Energies
        energies_fu = [] # Energies / f.u.
        dirnames = []  # Directory names
        base_atoms = []  # number of base atom
        supercells = []  # supercell
        con0_energy = None  # 0.0 Concentration energy (for calculating formation energy)
        con1_energy = None  # 1.0 Concentration energy (for calculating formation energy)

        sub_ds = [sd for sd in os.listdir("./") if os.path.isdir(sd) and "con" in sd]
        sub_ds.sort()

        # -- sub_ds = [cons0.0, cons1.0, ..]
        for sd in sub_ds:
            os.chdir(sd)
            scel_index = int(sd.split(".")[0].replace("con", ""))  # con[index].xx
            vol = supcell[scel_index]  # supcell:[1, 2, 2, 3, 4, ..]
            supercells.append(vol)

            poscar = IStructure.from_file("POSCAR")
            species = poscar.species            # [Element Ag, Element Ag, Element Ag, ...]
            elts_dict = {}               # key: element name, value: number of element
            for sp in poscar.types_of_specie:
                elts_dict[str(sp)] = species.count(sp)
            elts = elts_dict.keys()

            if base in elts:
                n_of_base = float(elts_dict[base])
                con = round(n_of_base / (tot_base * vol), 6)
                cons.append(con)
                base_atoms.append(float(elts_dict[base]))
            else:
                con = 0.0
                cons.append(con)
                base_atoms.append(0)

            # -- find energy
            popen = os.popen("grep \"free  energy   TOTEN\" OUTCAR").readlines()
            e = float(popen[-1].split()[-2])
            e_fu = e / vol
            energies.append(e)
            energies_fu.append(e_fu)

            if con == 0.0:
                con0_energy = e_fu
            elif con == 1.0:
                con1_energy = e_fu

            # -- dirname
            crr_dir = os.getcwd().split("/")[-1]

            dirnames.append(crr_dir)
            os.chdir("../")

        data = {"Concentration": cons, "Directory": dirnames, "Energy": energies, "Energy/f.u.": energies_fu,
                "Supercell": supercells, "Number of base atom": base_atoms}
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
        hull_df = hull_df.sort_values('Concentration')
        hull_df = hull_df[
            ['Concentration', 'Formation energy', 'Energy', 'Energy/f.u.', 'Supercell', 'Number of base atom', 'Directory']]

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

        plt.axhline(y=0, color='gray', lw=1, ls='--')

        plt.xlim(0.0, 1.0)
        plt.xlabel(base + " Concentration", fontsize=20)
        plt.ylabel("Formation energy (eV/f.u.)", fontsize=20)

        plt.tight_layout()

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

        if len(hull_point_dirs) != 0:
            print("Hull point structures")
            for d in hull_point_dirs:
                os.chdir(d)
                VO = VASPOutput()
                VO.getFinalStructure(path="../Data/")
                os.chdir("../")

    def getVoltageProfile(self, chempot):
        df = self.hull_df
        cons = df['Concentration'].tolist()
        energies = df['Energy'].tolist()
        supercells = df['Supercell'].tolist()
        n_of_atoms = df['Number of base atom'].tolist()

        x = []
        y = []
        for i in range(len(energies)):
            j = i + 1
            if j < len(energies):
                #e1 = energies[i] * supercells[i]
                #e2 = energies[j] * supercells[j]

                #n1 = n_of_atoms[i] * supercells[i]
                #n2 = n_of_atoms[j] * supercells[j]

                e1 = energies[i]
                e2 = energies[j]

                n1 = n_of_atoms[i]
                n2 = n_of_atoms[j]
                diff_Li = n2 - n1

                pot = ((e2 - e1) / diff_Li) - chempot
                vol = -pot

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

    def mainFlow(self, chempot=None):
        self.parsingData()
        self.getFormData()
        self.makeHull()
        self.plotHull()
        self.getHullPointStructures()
        if self.base == "Li":
            self.getVoltageProfile(chempot)


if __name__ == "__main__":
    # -- Get info ------------
    base = raw_input("\n* Element name which be changed (ex: Li) : ")  # base atom
    tot_base = raw_input(
        "* Number of " + base + " when full (ex: 9) in the primitive cell: ")  # the number of base atoms in unit cell
    tot_base = float(tot_base)

    ch = CASMhull(base=base, tot_base=tot_base)
    if base == "Li":
        get_chempot = raw_input("Chemical potential of Li, just enter to use the default value (-1.886)\n: ")
        if len(get_chempot) == 0:
            chempot = -1.886
        else:
            chempot = float(get_chempot)
        ch.mainFlow(chempot=chempot)
    else:
        ch.mainFlow()









