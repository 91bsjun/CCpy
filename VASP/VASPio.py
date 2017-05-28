import os, sys, re
import matplotlib.pyplot as plt
import pandas as pd
import json
from collections import OrderedDict

from CCpy.Tools.CCpyStructure import PeriodicStructure as PS
from CCpy.Tools.CCpyStructure import latticeGen
from CCpy.Tools.CCpyTools import file_writer, linux_command, vasp_incar_json


from pymatgen.core import IStructure as pmgIS
from pymatgen.io.vasp.inputs import Incar, Poscar, Potcar, Kpoints, Kpoints_supported_modes
from pymatgen.io.vasp.sets import *

version = sys.version
if version[0] == '3':
    raw_input = input

class VASPInput():
    def __init__(self, filename, dirname=None):
        if ".xsd" in filename:
            ps = PS(filename)
            ps.xsdFile()
            ps.cifWrite(filename="tmpstructure.cif")
            structure = pmgIS.from_file("tmpstructure.cif")
            os.remove("tmpstructure.cif")
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

        # ------------ check initial config ------------- #
        home = os.getenv("HOME")
        if ".CCpy" not in os.listdir(home):
            os.mkdir(home+"/.CCpy")
        configs = os.listdir(home+"/.CCpy")
        if "vasp_incar.json" in configs:
            jstring = open(home + "/.CCpy/vasp_incar.json", "r").read()
            incar_dict = json.loads(jstring, object_pairs_hook=OrderedDict)
        else:
            jstring = vasp_incar_json()
            incar_dict = json.loads(jstring, object_pairs_hook=OrderedDict)
            f = open(home + "/.CCpy/vasp_incar.json", "w")
            f.write(jstring)
            f.close()
        if "vasp_MAGMOM.json" in configs:
            jstring = open(home + "/.CCpy/vasp_MAGMOM.json", "r").read()
            magmom = json.loads(jstring)
        else:
            magmom = {'Mn3+': 4, 'Ni4+': 0.6, 'Cr': 5, 'Mn4+': 3, 'Ta': 5, 'Ni3+': 1, 'Mo': 5,
                      'Ni': 2, 'V': 5, 'Mn2+': 5, 'Co': 5, 'Co4+': 1, 'W': 5, 'Fe3+': 5, 'Fe2+': 4,
                      'Mn': 5, 'Fe4+': 4, 'Fe': 5, 'Co3+': 0.6,
                      'Li': 0.6, 'O': 0.6}
            jstring = json.dumps(magmom, indent=4)
            f = open(home+"/.CCpy/vasp_MAGMOM.json", "w")
            f.write(jstring)
            f.close()

        if "vasp_LDAUU.json" in configs:
            jstring = open(home + "/.CCpy/vasp_LDAUU.json", "r").read()
            LDAUU = json.loads(jstring)
        else:
            LDAUU = {'Mo': 4.38, 'V': 3.1, 'Cu': 4, 'W': 4.0, 'Ag': 1.5, 'Cr': 3.5, 'Ta': 2,
                     'Nb': 1.5, 'Mn': 3.9, 'Re': 2, 'Co': 3.4, 'Ni': 6, 'Fe': 4.0,
                     'Li': 0, 'O': 0}
            jstring = json.dumps(LDAUU, indent=4)
            f = open(home + "/.CCpy/vasp_LDAUU.json", "w")
            f.write(jstring)
            f.close()
        if "vasp_LDAUL.json" in configs:
            jstring = open(home + "/.CCpy/vasp_LDAUL.json", "r").read()
            LDAUL = json.loads(jstring)
        else:
            LDAUL = {'Mo': 2, 'V': 2, 'Cu': 2, 'W': 2, 'Ag': 2, 'Cr': 2, 'Ta': 2,
                     'Nb': 2, 'Mn': 2, 'Re': 2, 'Co': 2, 'Ni': 2, 'Fe': 2,
                     'Li': 0, 'O': 0}
            jstring = json.dumps(LDAUL, indent=4)
            f = open(home + "/.CCpy/vasp_LDAUL.json", "w")
            f.write(jstring)
            f.close()
        if "vasp_LDAUJ.json" in configs:
            jstring = open(home + "/.CCpy/vasp_LDAUJ.json", "r").read()
            LDAUJ = json.loads(jstring)
        else:
            LDAUJ = {'Mo': 0, 'V': 0, 'Cu': 0, 'W': 0, 'Ag': 0, 'Cr': 0, 'Ta': 0,
                     'Nb': 0, 'Mn': 0, 'Re': 0, 'Co': 0, 'Ni': 0, 'Fe': 0,
                     'Li': 0, 'O': 0}
            jstring = json.dumps(LDAUJ, indent=4)
            f = open(home + "/.CCpy/vasp_LDAUJ.json", "w")
            f.write(jstring)
            f.close()
        # DFT-D2 parameters : From VASP wiki
        vdw_C6 = {'H': 0.14, 'He': 0.08, 'Li': 1.61, 'Be': 1.61, 'B': 3.13, 'C': 1.75, 'N': 1.23, 'O': 0.70, 'F': 0.75,
                  'Ne': 0.63, 'Na': 5.71,
                  'Mg': 5.71, 'Al': 10.79, 'Si': 9.23, 'P': 7.84, 'S': 5.57, 'Cl': 5.07, 'Ar': 4.61, 'K': 10.80,
                  'Ca': 10.80,
                  'Sc': 10.80, 'Ti': 10.80, 'V': 10.80, 'Cr': 10.80, 'Mn': 10.80, 'Fe': 10.80, 'Co': 10.80, 'Ni': 10.80,
                  'Cu': 10.80, 'Zn': 10.80,
                  'Ga': 16.99, 'Ge': 17.10, 'As': 16.37, 'Se': 12.64, 'Br': 12.47, 'Kr': 12.01, 'Rb': 24.67,
                  'Sr': 24.67,
                  'Y': 24.67, 'Zr': 24.67, 'Nb': 24.67, 'Mo': 24.67, 'Tc': 24.67, 'Ru': 24.67, 'Rh': 24.67, 'Pd': 24.67,
                  'Ag': 24.67, 'Cd': 24.67,
                  'In': 37.32, 'Sn': 38.71, 'Sb': 38.44, 'Te': 31.74, 'I': 31.50, 'Xe': 29.99}

        vdw_R0 = {'H': 1.001, 'He': 1.012, 'Li': 0.825, 'Be': 1.408, 'B': 1.485, 'C': 1.452, 'N': 1.397, 'O': 1.342,
                  'F': 1.287, 'Ne': 1.243, 'Na': 1.144,
                  'Mg': 1.364, 'Al': 1.716, 'Si': 1.716, 'P': 1.705, 'S': 1.683, 'Cl': 1.639, 'Ar': 1.595, 'K': 1.485,
                  'Ca': 1.474,
                  'Sc': 1.562, 'Ti': 1.562, 'V': 1.562, 'Cr': 1.562, 'Mn': 1.562, 'Fe': 1.562, 'Co': 1.562, 'Ni': 1.562,
                  'Cu': 1.562, 'Zn': 1.562,
                  'Ga': 1.650, 'Ge': 1.727, 'As': 1.760, 'Se': 1.771, 'Br': 1.749, 'Kr': 1.727, 'Rb': 1.628,
                  'Sr': 1.606,
                  'Y': 1.639, 'Zr': 1.639, 'Nb': 1.639, 'Mo': 1.639, 'Tc': 1.639, 'Ru': 1.639, 'Rh': 1.639, 'Pd': 1.639,
                  'Ag': 1.639, 'Cd': 1.639,
                  'In': 1.672, 'Sn': 1.804, 'Sb': 1.881, 'Te': 1.892, 'I': 1.892, 'Xe': 1.881}

        self.incar_dict, self.magmom, self.LDAUL, self.LDAUU, self.LDAUJ, self.vdw_C6, self.vdw_R0 = incar_dict, magmom, LDAUL, LDAUU, LDAUJ, vdw_C6, vdw_R0


    # ------------------------------------------------------------------------------#
    #                        CMS relaxation VASP input set                          #
    # ------------------------------------------------------------------------------#
    def cms_vasp_set(self, single_point=False, isif=False, vdw=False,
                     spin=False, mag=False, ldau=False,
                     functional="PBE_54",
                     kpoints=False, get_pre_options=None,
                     magmom_dict=None, ldau_dict=None,
                     flask_app=False):
        """

        :param single_point:
        :param isif:
        :param vdw:
        :param spin:
        :param mag:
        :param ldau:
        :param functional:
        :param kpoints: list [4,4,1]
        :param incar_dict: dictionary type of incar
        :param input_incar: string type of incar (if exist, pass the confirm menu)
        :param flask_app: in case of flask app, avoid confirm menu (use default k-points = input_kpts)

        :return: no return, but write VASP input files at dirname
        """

        structure = self.structure
        dirname = self.dirname

        # -- Load pre option
        if get_pre_options:
            pre_dict = get_pre_options
            incar_dict = pre_dict["incar"]
            magmom_dict = pre_dict["magmom"]
            ldau_dict = pre_dict["ldauu"]
        else:
            incar_dict = None
            magmom_dict = None
            ldau_dict = None

        ## -------------------------------- POSCAR -------------------------------- ##
        # -- Create POSCAR string from pymatgen structure object
        poscar = structure.to(fmt="poscar")

        # -- Parsing elements and its number for MAGMOM and LDA+U parameters
        elements = []
        for el in structure.species:
            if str(el) not in elements:
                elements.append(str(el))
        lines = poscar.split("\n")
        for i in range(len(lines)):
            if i == 5:
                elts = lines[i].split()
            elif i == 6:
                n_of_atoms = lines[i].split()

        ## -------------------------------- INCAR -------------------------------- ##
        # -- INCAR preset dictionary
        # -- if incar_dict arg exist use it
        if incar_dict:
            pass
        else:
            incar_dict = self.incar_dict

        if "SYSTEM" in incar_dict.keys():
            incar_dict["SYSTEM"] = dirname

        # -- Parsing system arguments from user commands
        if single_point:
            incar_dict['NSW']="0                      ! number of steps for IOM"
        if isif:
            incar_dict['ISIF']=str(isif) + "                     ! ion&cell relax: 0-MD 2-ion&stress 3-ion&cell&stress"
        if spin:
            incar_dict['ISPIN']="2                    ! spin polarized = 2, non spin polarized = 1"

        # -- magnetic momentum
        if magmom_dict:
            magmom = magmom_dict        # get magmom parameters from previous option
        else:
            magmom = self.magmom

        # -- magmom value edit
        if mag and not flask_app and not magmom_dict:
            print("\n# ---------- Here are the current MAGMOM values ---------- #")
            magmom_keys = magmom.keys()
            magmom_keys.sort()
            for key in magmom_keys:
                print(str(key).ljust(8) + " = " + str(magmom[key]))
            print("Other atoms which not in here are = 0.6")
            get_sets = raw_input(
                "* Anything want to modify or add? if not, enter \"n\" or (Co=6,Ni=4) \n: ")
            if get_sets != "n":
                vals = get_sets.replace(" ", "")
                vals = vals.split(",")
                for val in vals:
                    key = val.split("=")[0]
                    value = val.split("=")[1]
                    magmom[key] = value

        mag_string = ""
        for i in range(len(n_of_atoms)):
            try:
                mag_string += str(n_of_atoms[i]) + "*" + str(magmom[elts[i]]) + " "
            except:
                mag_string += str(n_of_atoms[i]) + "*" + str(0.6) + " "

        # uncomment mag options. if already uncommented, just change MAGMOM parameters
        if mag:
            if "# MAGMOM" in incar_dict.keys():
                incar_dict = OrderedDict([("MAGMOM",mag_string) if k == "# MAGMOM" else (k,v) for k, v in incar_dict.items()])
            elif "MAGMOM" in incar_dict.keys():
                incar_dict['MAGMOM'] = mag_string
        # comment mag options. if already commented, just change MAGMOM parameters
        else:
            if "# MAGMOM" in incar_dict.keys():
                incar_dict['# MAGMOM'] = mag_string
            elif "MAGMOM" in incar_dict.keys():
                incar_dict = OrderedDict(
                    [("# MAGMOM", mag_string) if k == "MAGMOM" else (k, v) for k, v in incar_dict.items()])
        # -- ldau
        if ldau_dict:
            LDAUU = ldau_dict       # get ldauu parameters from previous option
        else:
            LDAUU = self.LDAUU
        if ldau and not flask_app and not ldau_dict:
            print("\n# -------------------------------------------------------- #")
            print("#          Here are the current LDAU+U parameters          #")
            print("# -------------------------------------------------------- #")
            LDAUU_keys = LDAUU.keys()
            LDAUU_keys.sort()
            for key in LDAUU_keys:
                print(str(key).ljust(8) + " = " + str(LDAUU[key]))
            print("Other atoms which not in here are = 0")
            get_sets = raw_input("* Anything want to modify or add? if not, enter \"n\" or (Ni=7.3,Mn=3.6) \n: ")
            if get_sets != "n":
                vals = get_sets.replace(" ", "")
                vals = vals.split(",")
                for val in vals:
                    key = val.split("=")[0]
                    value = val.split("=")[1]
                    LDAUU[key] = value
        LDAUL_string = ""
        for i in range(len(elts)):
            try:
                LDAUL_string += str(LDAUL[elts[i]]) + " "
            except:
                LDAUL_string += str(0) + " "

        LDAUU_string = ""
        for i in range(len(elts)):
            try:
                LDAUU_string += str(LDAUU[elts[i]]) + " "
            except:
                LDAUU_string += str(0) + " "

        LDAUJ_string = ""
        for i in range(len(elts)):
            try:
                LDAUJ_string += str(LDAUJ[elts[i]]) + " "
            except:
                LDAUJ_string += str(0) + " "

        # uncomment ldau options. if already uncommented, just change LDAUU,LDAUL,LDAUJ parameters
        if ldau:
            if "# LDAU" in incar_dict.keys():
                incar_dict = OrderedDict(
                    [("LDAU", incar_dict["# LDAU"]) if k == "# LDAU" else (k, v) for k, v in incar_dict.items()])
            if "# LMAXMIX" in incar_dict.keys():
                incar_dict = OrderedDict(
                    [("LMAXMIX", incar_dict["# LMAXMIX"]) if k == "# LMAXMIX" else (k, v) for k, v in incar_dict.items()])
            if "# LDAUTYPE" in incar_dict.keys():
                incar_dict = OrderedDict(
                    [("LDAUTYPE", incar_dict["# LDAUTYPE"]) if k == "# LDAUTYPE" else (k, v) for k, v in incar_dict.items()])
            if "# LDAUL" in incar_dict.keys():
                incar_dict = OrderedDict(
                    [("LDAUL", LDAUL_string) if k == "# LDAUL" else (k, v) for k, v in incar_dict.items()])
            elif "LDAUL" in incar_dict.keys():
                incar_dict["LDAUL"] = LDAUL_string

            if "# LDAUU" in incar_dict.keys():
                incar_dict = OrderedDict(
                    [("LDAUU", LDAUU_string) if k == "# LDAUU" else (k, v) for k, v in incar_dict.items()])
            elif "LDAUU" in incar_dict.keys():
                incar_dict["LDAUU"] = LDAUU_string

            if "# LDAUJ" in incar_dict.keys():
                incar_dict = OrderedDict(
                    [("LDAUJ", LDAUJ_string) if k == "# LDAUJ" else (k, v) for k, v in incar_dict.items()])
            elif "LDAUJ" in incar_dict.keys():
                incar_dict["LDAUJ"] = LDAUJ_string
        # comment ldau options. if already commented, just change LDAUU,LDAUL,LDAUJ parameters
        else:
            if "LDAU" in incar_dict.keys():
                incar_dict = OrderedDict(
                    [("# LDAU", incar_dict["LDAU"]) if k == "LDAU" else (k, v) for k, v in incar_dict.items()])
            if "LMAXMIX" in incar_dict.keys():
                incar_dict = OrderedDict(
                    [("# LMAXMIX", incar_dict["LMAXMIX"]) if k == "LMAXMIX" else (k, v) for k, v in incar_dict.items()])
            if "LDAUTYPE" in incar_dict.keys():
                incar_dict = OrderedDict(
                    [("# LDAUTYPE", incar_dict["LDAUTYPE"]) if k == "LDAUTYPE" else (k, v) for k, v in incar_dict.items()])
            if "LDAUL" in incar_dict.keys():
                incar_dict = OrderedDict(
                    [("# LDAUL", LDAUL_string) if k == "LDAUL" else (k, v) for k, v in incar_dict.items()])
            elif "# LDAUL" in incar_dict.keys():
                incar_dict["# LDAUL"] = LDAUL_string

            if "LDAUU" in incar_dict.keys():
                incar_dict = OrderedDict(
                    [("# LDAUU", LDAUU_string) if k == "LDAUU" else (k, v) for k, v in incar_dict.items()])
            elif "# LDAUU" in incar_dict.keys():
                incar_dict["# LDAUU"] = LDAUU_string

            if "LDAUJ" in incar_dict.keys():
                incar_dict = OrderedDict(
                    [("# LDAUJ", LDAUJ_string) if k == "LDAUJ" else (k, v) for k, v in incar_dict.items()])
            elif "# LDAUJ" in incar_dict.keys():
                incar_dict["# LDAUJ"] = LDAUJ_string

        if vdw:
            vdw_C6 = self.vdw_C6
            vdw_R0 = self.vdw_R0
            C6 = ""
            R0 = ""
            for el in elements:
                C6+=str(vdw_C6[el])+" "
                R0+=str(vdw_R0[el])+" "
            incar_dict['LVDW']=".TRUE."
            incar_dict['VDW_RADIUS']=30.0
            incar_dict['VDW_SCALING']=0.75
            incar_dict['VDW_D']=20.0
            incar_dict['VDW_C6']=C6
            incar_dict['VDW_R0']=R0


        ## -------------------------------- KPOINTS -------------------------------- ##
        # -- if user input the k-points in command
        if kpoints:
            kpts = kpoints
        else:
            lattice_vector = structure.lattice.matrix
            lattice = latticeGen(lattice_vector[0],lattice_vector[1],lattice_vector[2])
            length = [lattice['length'][0], lattice['length'][1], lattice['length'][2]]
            kpts = []
            for param in length:
                if 20 // param == 0 or 20 // param == 1:
                    kpts.append(2)
                else:
                    kpts.append(int(20 // param))
        kpoints = dirname+"\n0\nMonkhorst-Pack\n"+str(kpts[0])+" "+str(kpts[1])+" "+str(kpts[2])+"\n0 0 0\n"

        ## -------------------------------- POTCAR -------------------------------- ##
        potcar = Potcar(symbols=elements, functional="PBE_54")

        try:
            os.mkdir(dirname)
        except:
            files = os.listdir(dirname)
            if "INCAR" in files or "POSCAR" in files or "KPOINTS" in files or "POTCAR" in files:
                ans = raw_input(dirname+" already exist. Will you override ? (y/n)")
                if ans == "y":
                    pass
                else:
                    quit()
            else:
                pass

        ## --------------------------- Confirm input values ---------------------- ##
        if flask_app:
            incar = Incar(incar_dict)
            pass
        else:
            # -- INCAR
            print(dirname)
            if not input_incar :        # This process is for avoiding multiple inputs generation.
                get_sets = None
                while get_sets != "n":
                    print("\n# -------------------------------------------------------- #")
                    print("#            Here are the current INCAR options            #")
                    print("# -------------------------------------------------------- #")
                    incar_keys = incar_dict.keys()
                    # incar_keys.sort()
                    incar_string = ""
                    for key in incar_keys:
                        if key[0] == "#" and key[1].isdigit():
                            incar_string += "\n"
                            incar_string += key + str(incar_dict[key]) + "\n"
                        else:
                            incar_string += key + " = " + str(incar_dict[key]) + "\n"
                    print(incar_string)
                    get_sets = raw_input("* Anything want to modify or add? if not, enter \"n\" or (ex: ISPIN=2,ISYM=1,PREC=Accurate /without spacing) \n: ")
                    if get_sets != "n":
                        vals = get_sets.replace(", ",",")
                        vals = vals.split(",")
                        for val in vals:
                            key = val.split("=")[0]
                            value = val.split("=")[1]
                            if "# "+key in incar_keys:
                                incar_dict = OrderedDict(
                                    [(key, incar_dict["# "+key]) if k == "# "+key else (k, v) for k, v in
                                     incar_dict.items()])
                                original = incar_dict[key]
                                try:
                                    description = original.split("!")[1]
                                except:
                                    description = ""
                                incar_dict[key] = value.ljust(22) + "!" + description
                            else:
                                if key in incar_keys:
                                    original = incar_dict[key]
                                else:
                                    original = ""
                                try:
                                    description = original.split("!")[1]
                                except:
                                    description = ""
                                incar_dict[key] = value.ljust(22) + "!" + description
                # make INCAR string type
                incar_keys = incar_dict.keys()
                # incar_keys.sort()
                incar_string = ""
                for key in incar_keys:
                    if key[0] == "#" and key[1].isdigit():
                        incar_string += "\n"
                        incar_string += key + str(incar_dict[key]) + "\n"
                    else:
                        incar_string += key + " = " + str(incar_dict[key]) + "\n"
                incar = incar_string
            else:
                incar = Incar(incar_dict)

        # save current options, for rest inputs
        current_options = {"incar":incar_dict, "magmom":magmom, "ldauu":LDAUU}
        jstring = json.dumps(current_options)
        f = open("current_options.json", "w")
        f.write(jstring)
        f.close()

        ## ----------------------------- Write inputs ---------------------------- ##
        os.chdir(dirname)
        file_writer("POSCAR",str(poscar))
        file_writer("POTCAR",str(potcar))
        file_writer("INCAR",str(incar))
        file_writer("KPOINTS",str(kpoints))
        os.chdir("../")


        ## ------------------------- Move structure file ------------------------ ##
        try:
            os.mkdir("structures")
        except:
            pass

        os.rename(self.filename, "./structures/"+self.filename)
        update_preset = raw_input("Do you want to update INCAR preset ? (y/n)")
        if update_preset == "y":
            jstring = json.dumps(incar_dict, indent=4)
            home = os.getenv("HOME")
            f = open(home + "/.CCpy/vasp_incar.json", "w")
            f.write(jstring)
            f.close()



    # ------------------------------------------------------------------------------#
    #                     CMS band and DOS calc VASP input set                      #
    # ------------------------------------------------------------------------------#
    def cms_band_set(self, vdw=False, spin=False, mag=False, kpoints=False, ldau=False, functional="PBE_54",
                     input_incar=None, input_kpts=None, input_line_kpts=None):
        structure = self.structure
        dirname = self.dirname

        ## -------------------------------- POSCAR -------------------------------- ##
        # -- Create POSCAR string from pymatgen structure object
        poscar = structure.to(fmt="poscar")

        # -- Parsing elements and its number for MAGMOM and LDA+U parameters
        elements = []
        for el in structure.species:
            if str(el) not in elements:
                elements.append(str(el))
        lines = poscar.split("\n")
        for i in range(len(lines)):
            if i == 5:
                elts = lines[i].split()
            elif i == 6:
                n_of_atoms = lines[i].split()

        ## -------------------------------- INCAR -------------------------------- ##
        incar_dict = {
            "NWRITE":2,"LPETIM":"F","ISTART":0,"INIWAV":1,"IWAVPR":1,"ICHARG":2,"LWAVE":".FALSE.",
            "ALGO":"NORMAL","NELM":100,"EDIFF":0.0001,"BMIX":3.00,"ENCUT":500,"GGA":"PE","ISYM":2,
            "LDIAG":"T","LREAL":"auto","PREC":"Medium",
            "NSW":0,"NBLOCK":1,"KBLOCK":10,"IBRION":2,"ISIF":3,"POTIM":0.5,"SMASS":3.0,
            "ISMEAR":0,"SIGMA":0.05,"LORBIT":11,
            "NPAR":8,"LPLANE":"T","ISPIN":1}

        if spin:
            incar_dict['ISPIN']=2
        if mag:
            mag_string = ""
            for i in range(len(n_of_atoms)):
                mag_string += str(n_of_atoms[i]) + "*" + str(magmom[elts[i]]) + " "
            incar_dict['MAGMOM'] = mag_string
        if ldau:
            LDAUL_string = ""
            for i in range(len(elts)):
                LDAUL_string += str(LDAUL[elts[i]]) + " "

            LDAUU_string = ""
            for i in range(len(elts)):
                LDAUU_string += str(LDAUU[elts[i]]) + " "

            LDAUJ_string = ""
            for i in range(len(elts)):
                LDAUJ_string += str(LDAUJ[elts[i]]) + " "

            incar_dict['LDAU'] = ".TRUE."
            incar_dict['LMAXMIX'] = 4
            incar_dict['LDAUTYPE'] = 2
            incar_dict['LDAUL'] = LDAUL_string
            incar_dict['LDAUU'] = LDAUU_string
            incar_dict['LDAUJ'] = LDAUJ_string
        if vdw:            
            C6 = ""
            R0 = ""
            for el in elements:
                C6+=str(vdw_C6[el])+" "
                R0+=str(vdw_R0[el])+" "
            incar_dict['LVDW']=".TRUE."
            incar_dict['VDW_RADIUS']=30.0
            incar_dict['VDW_SCALING']=0.75
            incar_dict['VDW_D']=20.0
            incar_dict['VDW_C6']=C6
            incar_dict['VDW_R0']=R0



        ## -------------------------------- KPOINTS -------------------------------- ##
        if kpoints:
            kpts = kpoints
        else:
            lattice_vector = structure.lattice.matrix
            lattice = latticeGen(lattice_vector[0],lattice_vector[1],lattice_vector[2])
            length = [lattice['length'][0], lattice['length'][1], lattice['length'][2]]
            kpts = []
            for param in length:
                if 20 // param == 0:
                    kpts.append(2)
                else:
                    kpts.append(int(20 // param))
        kpoints = dirname+"\n0\nMonkhorst-Pack\n"+str(kpts[0])+" "+str(kpts[1])+" "+str(kpts[2])+"\n0 0 0\n"

        # -- Line mode Kpoints
        from pymatgen.symmetry.bandstructure import HighSymmKpath
        hsk = HighSymmKpath(structure)
        line_kpoints = Kpoints.automatic_linemode(20, hsk)


        ## -------------------------------- POTCAR --------------------------------- ##
        linux_command("export VASP_PSP_DIR=/home/bsjun/bin/bsjunCODE/VASP_Potential")
        potcar = Potcar(symbols=elements, functional=functional)


        ## --------------------------- Confirm input values ---------------------- ##
        # -- INCAR
        print(dirname)
        if not input_incar:  # This process is for avoiding multiple inputs generation.
            get_sets = None
            while get_sets != "n":
                print("""
Here are the INCAR options.
NEDOS, PREC, SIGMA, LAECHG, ICHARG
values will be modified when generate Precalc and Band-DOS input sets as :
NEDOS=2001, PREC=accur, SIGMA=0.02, LAECHG=.True., ICHARG=11""")
                for key in incar_dict.keys():
                    print(str(key).ljust(8) + " = " + str(incar_dict[key]))
                get_sets = raw_input(
                    "* Anything want to modify or add? if not, enter \"n\" or (ex: ISPIN=2,ISYM=1,PREC=Accurate) \n: ")
                if get_sets != "n":
                    vals = get_sets.replace(" ", "")
                    vals = vals.split(",")
                    for val in vals:
                        key = val.split("=")[0]
                        value = val.split("=")[1]
                        incar_dict[key] = value

        # need to make module : INCAR string to INCAR dict

        # -- KPOINTS
        if not input_kpts:
            get_kpts = None
            while get_kpts != "n":
                print("\n* Here are the current KPOINTS (PreCalc).")
                print(kpoints)
                get_kpts = raw_input("* Anything want to modify? if not, enter \"n\" or (ex: 4,4,2) \n: ")
                if get_kpts != "n":
                    vals = get_kpts.replace(" ", "")
                    kpts = vals.split(",")
                    kpoints = dirname + "\n0\nMonkhorst-Pack\n" + str(kpts[0]) + " " + str(kpts[1]) + " " + str(kpts[2]) + "\n0 0 0\n"
        else:
            kpoints = input_kpts

        # -- Line mode KPOINTS
        if not input_line_kpts:
            get_line_kpts = None
            while get_line_kpts != "n":
                print("\n* Here are the current Line mode KPOINTS (Band-DOS).")
                splt_kpts = line_kpoints.split("\n")
                for i in range(len(splt_kpts)):
                    print(str(i) + " : " + splt_kpts[i])
                get_line_kpts = raw_input("* Anything want to remove line? if not, enter \"n\" or (ex: 1,2,4,5,6) \n: ")
                if get_line_kpts != "n":
                    vals = get_line_kpts.replace(" ", "")
                    line_index = vals.split(",")
                    for i in line_index:
                        del splt_kpts[i]
                    line_kpoints = ""
                    for line in splt_kpts:
                        line_kpoints += line+"\n"

        else:
            line_kpoints = input_line_kpts


        ## --------------------------- Write input files ---------------------- ##
        # -- Precalc INCAR
        incar_dict['NSW'] = 0
        incar_dict['NEDOS'] = 2001
        incar_dict['PREC'] = "accur"
        incar_pre = Incar(incar_dict)
        # -- Band-DOS INCAR
        incar_dict['SIGMA'] = 0.02
        incar_dict['LAECHG'] = ".True."
        incar_dict['ICHARG'] = 11
        incar_band_dos = Incar(incar_dict)

        try:
            os.mkdir("PreCalc")
        except:
            pass
        file_writer("./PreCalc/POSCAR",str(poscar))
        file_writer("./PreCalc/POTCAR",str(potcar))
        file_writer("./PreCalc/INCAR",str(incar_pre))
        file_writer("./PreCalc/KPOINTS",str(kpoints))
        try:
            os.mkdir("Band-DOS")
        except:
            pass
        file_writer("./Band-DOS/POSCAR",str(poscar))
        file_writer("./Band-DOS/POTCAR",str(potcar))
        file_writer("./Band-DOS/INCAR",str(incar_band_dos))
        file_writer("./Band-DOS/KPOINTS",str(line_kpoints))
        
    

    def MIT_relax_set(self):
        structure = self.structure
        dirname = self.dirname
        mit_relax = MITRelaxSet(structure)
        mit_relax.write_input(dirname)
        

    def MP_HSE_relax_set(self):
        structure = self.structure
        dirname = self.dirname
        mp_hse_relax = MPHSERelaxSet(structure)
        mp_hse_relax.write_input(dirname)
        

    def MP_static_set(self):
        structure = self.structure
        dirname = self.dirname
        mp_static = MPStaticSet(structure)
        mp_static.write_input(dirname)


    def MP_HSE_band_set(self):
        structure = self.structure
        dirname = self.dirname
        mp_hse_band = MPHSEBSSet(structure,reciprocal_density=20)
        mp_hse_band.write_input(dirname)
        

    def MIT_NEB_set(self):
        structure = self.structure
        dirname = self.dirname
        mit_neb = MITNEBSet(structure)
        mit_neb.write_input(dirname)




class VASPOutput():
    def __init__(self):
        pass

    def getFinalStructure(self, filename="CONTCAR", mv=True):
        from pymatgen.io.cif import CifWriter
        
        structure_object = pmgIS.from_file(filename)
        cif = CifWriter(structure_object)
        dirname = os.getcwd()
        dirname = dirname.split("/")[-1]
        
        if filename == "CONTCAR":
            target_name = dirname+"_contcar.cif"
        elif filename == "POSCAR":
            target_name = dirname+"_poscar.cif"
        else:
            target_name = filename

        cif.write_file(target_name)
        print(target_name+" is generated.")
        if mv:
            linux_command("mv "+target_name+" ../")

    def getConvergence(self, show_plot=True):

        OUTCAR = open("OUTCAR", "r").read()

        # -- energy parsing
        findE = re.compile("free  energy   TOTEN  =\s+\S+", re.M)
        strings = findE.findall(OUTCAR)
        e = []
        for s in strings:
            e.append(float(s.split()[4]))
        xe = range(len(e))
        xe = [x+1 for x in xe]

        print("Initial energy : "+str(e[0]))
        print("  Final energy : "+str(e[-1]))

        # -- volume parsing
        findV = re.compile("volume of cell :\s+\S+", re.M)
        strings = findV.findall(OUTCAR)
        vol = []
        for s in strings:
            vol.append(float(s.split()[4]))
        xv = range(len(vol))

        print("Initial volume : "+str(vol[0]))
        print("  Final volume : "+str(vol[-1]))


        if show_plot:
            # make plot
            fig, ax1 = plt.subplots()
            ax2 = ax1.twinx()
            ax1.plot(xe, e, color="b", marker="o", mec="b", label="Energy", lw=1.5)
            ax2.plot(xv, vol, color="#DB0000", marker="o", mec="#DB0000", label="Volume", lw=1.5)

            ax1.set_xlabel('Steps', fontsize=19)
            ax1.set_ylabel('Energy', color='b', fontsize=19)
            ax2.set_ylabel('Cell volume', color='#DB0000', fontsize=19)

            plt.grid()
            plt.tight_layout()
            plt.savefig("convergence.png")
            plt.show()


    def get_energy_list(self, show_plot=True):
        dirs = [d for d in os.listdir("./") if os.path.isdir(d)]
        out_dirs = [d for d in dirs if "OUTCAR" in os.listdir(d)]
        out_dirs.sort()

        x = range(len(out_dirs))
        energies = []
        for o in out_dirs:
            OUTCAR = open(o+"/OUTCAR", "r").read()
            # -- energy parsing
            findE = re.compile("free  energy   TOTEN  =\s+\S+", re.M)
            strings = findE.findall(OUTCAR)
            e = []
            for s in strings:
                e.append(float(s.split()[4]))
            energies.append(e[-1])

        energy_list = {}
        energy_list['Directory'] = out_dirs
        energy_list['Total energy(eV)'] = energies

        df = pd.DataFrame(energy_list)
        print(df)
        df.to_csv("FinalEnergies.csv")

        if show_plot:
            fig = plt.figure(figsize=(8, 7))
            plt.plot(x, energies, marker='o', color='#0054FF')
            plt.xticks(x, out_dirs, rotation=45)
            plt.tight_layout()
            plt.grid()
            plt.show()




















    
