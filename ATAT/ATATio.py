import os, sys, re
from collections import OrderedDict

from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import numpy as np
from math import sqrt
import pandas as pd

from CCpy.Tools.CCpyStructure import PeriodicStructure as PS
from CCpy.Tools.CCpyStructure import latticeGen
from CCpy.Tools.CCpyTools import file_writer, linux_command

from pymatgen.core import IStructure as pmgIS


class ATATInput():
    def __init__(self, filename=None, dirname=None):
        if filename:
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

    def vasp_wrap_template(self):
        vasp_wrap = """SYSTEM           = ATAT                            !

#1 Startparameter for this Run:
NWRITE           = 2                             ! LPETIM=F    write-flag & timer
ISTART           = 0                             ! job   : 0-new  1-contEcut  2-sameBS
INIWAV           = 1                             ! 0-jellium  1-random
IWAVPR           = 1                             ! prediction:  0-non 1-charg 2-wave 3-comb
ICHARG           = 2                             ! 0-from WF  1-from CHGCAR  2-from atom  11-12-fixed
LWAVE            = .FALSE.                       ! determines whether the wavefunctions are written to the WAVECAR file

#2 Electronic Relaxation 1
NELM             = 100                           ! number of iterations
EDIFF            = 1E-04                         ! stopping-criterion for ELM
BMIX             = 3.00                          ! sets the cutoff wave vector for Kerker mixing for the magnetization density
ENCUT            = 500                           ! Cut-Off Energy

#3 Electronic Relaxation 1
# ALGO           = 48                            ! algorithm for the e-relax
LDIAG            = T                             ! sub-space diagonalisation
LREAL            = auto                          ! real-space projection
PREC             = normal                        ! accuracy
# NBANDS         = 30                            ! number of bands for diagonalization

#4 Ionic Relaxation
NSW              = 200                           ! number of steps for IOM
NBLOCK           = 1                             ! inner block
KBLOCK           = 10                            ! outer block
IBRION           = 2                             ! ionic relax: 0-MD 1-quasi-New 2-CG
ISIF             = 3                             ! ion&cell relax: 0-MD 2-ion&stress 3-ion&cell&stress
ISYM             = 2                             ! switch symmetry stuff ON (1 or 2) or OFF (0)
# SYMPREC        =  1e-6                         !
LCORR            = T                             ! Harris-correction to forces
EDIFFG           = -0.04                         ! Criterion for geom opt (eV/Ang)
POTIM            = 0.50                          ! time-step for ionic motion (fs)
SMASS            = 3.00                          ! Nose mass-parameter (am)

#5 DOS related values
ISMEAR           = 0                             ! Broadening methode -5-tet -1-fermi 0-gaus 1-mp 2-mp2
SIGMA            = 0.05                          ! Broadening in eV
LORBIT           = 11                            ! l-decomposed DOS
# RWIGS          = 1.63  1.00                    ! Wigner-Zeits radius
# EMIN           =                               ! Minimum energy for DOS
# EMAX           =                               ! Maximum energy for DOS
# NEDOS          = 1001                          ! Number of DOS points
# NELECT         = 100                           ! Total number of electrons
# NUPDOWN        = 2                             ! Difference between UP&DOWN electrons

#6 Parallelizationoption
LPLANE           = T                             ! Parallelization for PWs
NCORE            = 8                             !
LSCALU           = F                             !
NSIM             = 4                             !
ISPIN            = 2                             ! spin polarized = 2, non spin polarized = 1

#7 optB86b-vdW functional requires vdw_kernel.bindat
# GGA            = MK                            !
# PARAM1         = 0.1234                        !
# PARAM2         = 1.0000                        !
# LUSE_VDW       = .TRUE.                        !
# AGGAC          = 0.0000                        !

#8 TS calculation         ! default:Nudged Elestic Band method
# ICHAIN         = 0                             ! Method (0=NEB, 1=Dynamical matrix, 2=Dimer, 3=Lanczos)
# SPRING         = -5                            ! in eV/Ang*2 (sping constant)
# IMAGES         = 3                             ! Number of images btw Reactant & Product
# LCLIMB         = .true.                        ! cNEB: driven up to the saddle point
# LTANGENTOLD    = .true.                        ! Old central difference tangent
# LDNEB          = .true.                        ! Modified doubble nudging
# NEBCELL        = .true.                        ! NEB for variable cell (w/ ISIF=3)

#9 Dipole Correctionoption
# IDIPOL         = 3                             !
# LDIPOL         =                               !

#11 vdWcorrections
# IVDW             = 12                            !"""

        f = open("vasp.wrap", "w")
        f.write(vasp_wrap)
        f.close()
        print("vasp.wrap has been generated")

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

        if "vasp.wrap" not in os.listdir("./"):
            chk = raw_input("Do you want to make VASP INCAR template? (y/n) ")
            if chk == "y":
                self.vasp_wrap_template()


    def add_ldau(self):
        mag = {"Mn": 4.5, "Co": 0.0, "Ni": 1.0, "Li": 0.0, "O": 0.0, "Cl": 0.0, "F": 0.0, "N": 0.0, "S": 0.0}
        LDAUL = {"Mn": 2, "Co": 2, "Ni": 2, "Li": 2, "O": 2, "Cl": 2, "F": 2, "N": 2, "S": 2}
        LDAUU = {"Mn": 5.0, "Co": 4.0, "Ni": 6.4, "Li": 0, "O": 0, "Cl": 0, "F": 0, "N": 0, "S": 0}
        LDAUJ = {"Ni": 0, "Mn": 0, "Co": 0, "Li": 0, "O": 0, "Cl": 0, "F": 0, "N": 0, "S": 0}

        f = open("atomlabel.tmp", "r")
        lines = f.readlines()
        f.close()

        elts_dict = OrderedDict()
        for i in range(len(lines)):
            elt = lines[i].replace("\n", "")
            if elt not in elts_dict.keys():
                elts_dict[elt] = 1
            else:
                elts_dict[elt] += 1
        elts = elts_dict.keys()

        mag_string = ""
        for i in range(len(elts)):
            mag_string += str(elts_dict[elts[i]]) + "*" + str(mag[elts[i]]) + " "

        LDAUL_string = ""
        for i in range(len(elts)):
            LDAUL_string += str(LDAUL[elts[i]]) + " "

        LDAUU_string = ""
        for i in range(len(elts)):
            LDAUU_string += str(LDAUU[elts[i]]) + " "

        LDAUJ_string = ""
        for i in range(len(elts)):
            LDAUJ_string += str(LDAUJ[elts[i]]) + " "

        LDAU = """
# MAGMOM and LDA+U parameters
MAGMOM = %s
LDAU = .TRUE.
LMAXMIX = 4
LDAUTYPE = 2
LDAUL = %s
LDAUU = %s
LDAUJ = %s

        """ % (mag_string, LDAUL_string, LDAUU_string, LDAUJ_string)

        f = open("vasp.in", "r")
        lines = f.readlines()
        f.close()
        for i in range(len(lines)):
            if "[POSCAR]" in lines[i]:
                poscar_start = i
        f = open("vasp.in", "w")
        for i in range(len(lines)):
            if i == poscar_start - 1:
                f.write(LDAU)
                f.write(lines[i])
            else:
                f.write(lines[i])
        f.close()

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




        
