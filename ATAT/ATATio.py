import os, sys, re

from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import numpy as np
from math import sqrt
import pandas as pd

from bsjunCODE.bsjunStructure import PeriodicStructure as PS
from bsjunCODE.bsjunStructure import latticeGen
from bsjunCODE.bsjunTools import file_writer, linux_command

from pymatgen.core import IStructure as pmgIS


class ATATInput():
    def __init__(self, filename, dirname=None):
        if ".xsd" in filename:
            ps = PS(filename)
            ps.xsdFile()
            ps.cifWrite(filename="tmpstructure.cif")
            structure = pmgIS.from_file("tmpstructure.cif")
            linux_command("rm -rf tmpstructure.cif")
            jobname = filename.replace(".xsd","")
        elif ".cif" in filename: 
            structure = pmgIS.from_file(filename)
            jobname = filename.replace(".cif","")
        elif "POSCAR" in filename or "CONTCAR" in filename:
            structure = pmgIS.from_file(filename)
            pwd = os.getcwd()
            pwd = pwd.split("/")[-1]
            jobname = pwd
        else:
            print("Not supported file format. (.xsd, .cif, POSCAR, CONTCAR)")
            quit()

        if not dirname:
            dirname = jobname

        self.filename = filename
        self.structure = structure
        self.dirname = dirname


    def inputGen(self):
        structure = self.structure
        poscar = structure.to(fmt="poscar")
        lines = poscar.split("\n")
        coord = ""
        coord_start = False
        for l in lines:
            if "direct" in l:
                coord_start = True
            if coord_start and "direct" not in l:
                coord+=l
                coord+="\n"

        lattice_vector = structure.lattice.matrix
        lattice = latticeGen(lattice_vector[0],lattice_vector[1],lattice_vector[2])
        a = lattice['length'][0]
        b = lattice['length'][1]
        c = lattice['length'][2]
        aa = lattice['angle'][0]
        bb = lattice['angle'][1]
        cc = lattice['angle'][2]

        a = round(a, 6)
        b = round(b, 6)
        c = round(c, 6)
        aa = round(aa, 1)
        bb = round(bb, 1)
        cc = round(cc, 1)

        cell_info = ""
        for i in [a,b,c,aa,bb,cc]:
            cell_info += (str(i)+" ")
        cell_info += "\n"
        cell_info += "1 0 0\n0 1 0\n0 0 1\n"

        input_string = cell_info + coord

        file_writer("lat.in", input_string)

        vasp_wrap="""PREC = med
ISMEAR = 1
SIGMA = 0.05
EDIFF = 0.0001
ENCUT = 500
ICHARG = 2
NSW = 41
IBRION = 2
ISIF = 3
KPPRA = 1000
USEPOT = PAWPBE
DOSTATIC"""
        file_writer("vasp.wrap",vasp_wrap)

class ATATOut():
    def __init__(self):
        pass

    def fit_out(self):
        fit = open("fit.out", "r")
        lines = fit.readlines()
        fit.close()
        data = {'frac':[],'calc_energy':[],'fitted_energy':[],'calc-fit':[],'directory':[]}
        for l in lines:
            el = l.split()
            data['frac'].append(el[0])
            data['calc_energy'].append(float(el[1]))
            data['fitted_energy'].append(float(el[2]))
            data['calc-fit'].append(float(el[3]))
            data['directory'].append(el[5].replace("\n",""))

        df = pd.DataFrame(data)
        df = df[['frac','calc_energy','fitted_energy','calc-fit','directory']]
        fracs = []
        for item in df['frac']:
            if item not in fracs:
                fracs.append(item)
        fracs.sort()

        dfs = []
        print("The lowest energies of each concentration")
        for f in fracs:
            f_df = df[(df['frac']==f)]
            print(f,min(f_df['calc_energy']))
            dfs.append(f_df)

    def plotter(self):
        def get_xy(lines=None, x_index=0, y_index=None):
            x = []
            y = []

            for l in lines:
                el = l.split()
                if len(el) > 2:
                    x.append(float(el[0]))
                    y.append(float(el[y_index]))

            return [x, y]

        # gs energy
        gs = open("gs.out", "r")
        gs_lines = gs.readlines()
        gs.close()
        gs = get_xy(lines=gs_lines, y_index=2)

        # predicted gs
        prdgs = open("newgs.out", "r")
        prdgs_lines = prdgs.readlines()
        prdgs.close()
        prdgs = get_xy(lines=prdgs_lines, y_index=2)

        # fitted energy
        fit = open("fit.out", "r")
        fit_lines = fit.readlines()
        fit.close()
        calc = get_xy(lines=fit_lines, y_index=1)
        fit = get_xy(lines=fit_lines, y_index=2)

        # predicted energy
        prd = open("predstr.out", "r")
        prd_lines = prd.readlines()
        prd.close()
        prd = get_xy(lines=prd_lines, y_index=2)

        # ECI
        eci = open("clusinfo.out", "r")
        eci_lines = eci.readlines()
        eci.close()
        eci = get_xy(eci_lines, x_index=1, y_index=3)

        # -- Whole info
        plt.scatter(prd[0], prd[1], marker="+", color="#00B700", s=30, label="Predicted energy")
        plt.scatter(prdgs[0], prdgs[1], marker="o", color="#5F00FF", s=30, label="Predicted GS energy")
        plt.scatter(calc[0], calc[1], marker="D", color="#0054FF", s=10, label="Calculated energy")
        plt.plot(gs[0], gs[1], marker='o', markersize=8, color="#DB0000", label="Calculated GS energy")
        plt.xlabel("Li Concentration", fontsize=20)
        plt.ylabel("Formation energy (eV)", fontsize=20)
        plt.legend()
        plt.xlim(0, 1)
        plt.tight_layout()
        plt.savefig("convex_hull.png", dpi=200)
        plt.show()

        # -- Calc vs Fitted
        plt.scatter(calc[0], calc[1], marker="o", color="b", s=50, alpha=0.7, label="Calculated energy")
        plt.scatter(fit[0], fit[1], marker="+", color="red", s=50, label="Fitted energy")
        plt.plot(gs[0], gs[1], color="#0054FF")
        plt.xlabel("Li Concentration", fontsize=20)
        plt.ylabel("Formation energy (eV)", fontsize=20)
        plt.legend()
        plt.xlim(0, 1)
        plt.tight_layout()
        plt.savefig("calc_fitted.png", dpi=200)
        plt.show()

        # -- Eci vs diameters
        plt.scatter(eci[0], eci[1], marker="+", color="b", s=50)
        plt.axhline(y=0, ls='--', color='gray')
        plt.xlabel("Diameter", fontsize=20)
        plt.ylabel("ECI", fontsize=20)
        plt.xlim(min(eci[0]) - 0.3, max(eci[0]) + 0.3)
        plt.ylim(min(eci[1]) - 0.1, max(eci[1]) + 0.1)
        plt.tight_layout()
        plt.savefig("eci.png", dpi=200)
        plt.show()
        

    def multi_plotter(self, c1, c2, c3):
        c1, c2, c3 = int(c1)-1, int(c2)-1, int(c3)-1
        
        def get_xyz(lines=None, e_index=None):
            vals=[]
            for l in lines:
                el = l.split()
                if len(el) > 5:
                    vals.append([float(el[c1]), float(el[c2]), float(el[c3]), float(el[e_index])])

            vals = np.array(vals)

            x = []
            y = []
            z = []
            for row in vals:
                x_val = (0.5*(2*row[1]+row[2])/(row[0]+row[1]+row[2]))
                y_val = (sqrt(3)/2*(row[2])/(row[0]+row[1]+row[2]))
                z_val = row[3]

                x.append(x_val)
                y.append(y_val)
                z.append(z_val)

            return [x, y, z]
        # gs energy
        gs = open("gs_lines.out", "r")
        gs_lines = gs.readlines()
        gs.close()
        gs = get_xyz(lines=gs_lines, e_index=-3)

        # fitted energy
        fit = open("fit.out", "r")
        fit_lines = fit.readlines()
        fit.close()
        fit = get_xyz(lines=fit_lines, e_index=-4)

        # calculated energy
        calc = get_xyz(lines=fit_lines, e_index=-5)

        # predicted energy
        prd = open("predstr.out", "r")
        prd_lines = prd.readlines()
        prd.close()
        prd = get_xyz(lines=prd_lines, e_index=-3)

        # predicted gs energy
        prdg = open("newgs.out", "r")
        prdg_lines = prdg.readlines()
        prdg.close()
        prdg = get_xyz(lines=prdg_lines, e_index=-3)

        # plotting fitted energies
        fig = plt.figure()
        ax = fig.add_subplot(111, projection='3d')

        ax.scatter(gs[0], gs[1], gs[2], color="b", marker = "o", label="known gs")
        ax.scatter(fit[0], fit[1], fit[2], color="r", marker="^", label="known str")
        ax.scatter(prd[0], prd[1], prd[2], color="g", marker="x", label="predicted")
        ax.scatter(prdg[0], prdg[1], prdg[2], color="b", marker="x", label="predicted gs")

        plt.legend()
        plt.tight_layout()
        plt.show()        

        # plotting 'calc vs fit' energy
        fig = plt.figure()
        ax = fig.add_subplot(111, projection='3d')
        
        ax.scatter(calc[0], calc[1], calc[2], color="b", marker="o", label="calculated energy")
        ax.scatter(fit[0], fit[1], fit[2], color="r", marker="^", label="fitted energy")
        
        plt.legend()
        plt.tight_layout()
        plt.show()

        # plotting 'calc vs fit' energy 2
        plt.scatter(calc[2], fit[2], color="g")
        plt.xlabel("Calculated energy")
        plt.ylabel("Fitted energy")
        plt.plot([min(calc[2]),max(calc[2])], [min(calc[2]),max(calc[2])], ls='--', color='gray')
        plt.show()




        
