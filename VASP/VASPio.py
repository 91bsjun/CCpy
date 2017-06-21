import os, sys, re
import matplotlib.pyplot as plt
import pandas as pd
import json
from collections import OrderedDict

from CCpy.VASP.VASPtools import vasp_incar_json, magmom_parameters, ldauu_parameters, ldauj_parameters, ldaul_parameters, vasp_grimme_parameters

from CCpy.Tools.CCpyStructure import PeriodicStructure as PS
from CCpy.Tools.CCpyStructure import latticeGen
from CCpy.Tools.CCpyTools import file_writer, linux_command, change_dict_key, save_json, load_json


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

        # ------------ Grimme's parameters ------------- #
        vdw_C6, vdw_R0 = vasp_grimme_parameters()
        # ------------ check preset config ------------- #
        home = os.getenv("HOME")
        if ".CCpy" not in os.listdir(home):
            os.mkdir(home+"/.CCpy")
            print("* Preset options will be saved under :" + home + "/.CCpy/")
        configs = os.listdir(home+"/.CCpy")
        # INCAR preset check
        if "vasp_incar.json" in configs:
            incar_dict = load_json(home + "/.CCpy/vasp_incar.json", ordered=True)
        else:
            jstring = vasp_incar_json()         # Generate new INCAR
            incar_dict = json.loads(jstring, object_pairs_hook=OrderedDict)
            save_json(incar_dict, home + "/.CCpy/vasp_incar.json")
        # MAGMOM value preset check
        if "vasp_MAGMOM.json" in configs:
            magmom = load_json(home + "/.CCpy/vasp_MAGMOM.json")
        else:
            magmom = magmom_parameters()
            save_json(magmom, home+"/.CCpy/vasp_MAGMOM.json")
        # LDAUU value preset check
        if "vasp_LDAUU.json" in configs:
            LDAUU = load_json(home + "/.CCpy/vasp_LDAUU.json")
        else:
            LDAUU = ldauu_parameters()
            save_json(LDAUU, home + "/.CCpy/vasp_LDAUU.json")
        # LDAUL value preset check
        if "vasp_LDAUL.json" in configs:
            LDAUL = load_json(home + "/.CCpy/vasp_LDAUL.json")
        else:
            LDAUL = ldaul_parameters()
            save_json(LDAUL, home + "/.CCpy/vasp_LDAUL.json")
        # LDAUJ value preset check
        if "vasp_LDAUJ.json" in configs:
            LDAUJ = load_json(home + "/.CCpy/vasp_LDAUJ.json")
        else:
            LDAUJ = ldauj_parameters()
            save_json(LDAUJ, home + "/.CCpy/vasp_LDAUJ.json")


        self.incar_dict, self.magmom, self.LDAUL, self.LDAUU, self.LDAUJ, self.vdw_C6, self.vdw_R0 = incar_dict, magmom, LDAUL, LDAUU, LDAUJ, vdw_C6, vdw_R0


    # ------------------------------------------------------------------------------#
    #                        CMS relaxation VASP input set                          #
    # ------------------------------------------------------------------------------#
    def cms_vasp_set(self, single_point=False, isif=False, vdw=False,
                     spin=False, mag=False, ldau=False,
                     functional="PBE_54", pseudo=None,
                     kpoints=False, get_pre_options=None,
                     magmom_dict=None, ldau_dict=None,
                     flask_app=False):
        """

        :param single_point: set NSW=0
        :param isif: set ISIF parameter
        :param vdw: perform DFT-D2 calc
        :param spin: set ISPIN=2
        :param mag: MAGMOM value
        :param ldau: LDA+U method
        :param functional: POTCAR functional setting
        :param kpoints: list [4,4,1]
        :param get_pre_options: Load previous option when multiple input generation
        :param flask_app: in case of flask app, avoid confirm menu (use default k-points = input_kpts)

        :return: no return, but write VASP input files at dirname
        """

        structure = self.structure
        dirname = self.dirname

        # -- Load previous option when multiple input generation
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

        # -- edit magmom parameters
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

        # Uncomment mag options. if already uncommented, just change MAGMOM parameters
        if mag:
            if "# MAGMOM" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "# MAGMOM", "MAGMOM", mag_string)
            elif "MAGMOM" in incar_dict.keys():
                incar_dict['MAGMOM'] = mag_string
        # Comment mag options. if already commented, just change MAGMOM parameters
        else:
            if "# MAGMOM" in incar_dict.keys():
                incar_dict['# MAGMOM'] = mag_string
            elif "MAGMOM" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "MAGMOM", "# MAGMOM", mag_string)

        # -- LDA+U parameters
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
                incar_dict = change_dict_key(incar_dict, "# LDAU", "LDAU", incar_dict["# LDAU"])
            if "# LMAXMIX" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "# LMAXMIX", "LMAXMIX", incar_dict["# LMAXMIX"])
            if "# LDAUTYPE" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "# LDAUTYPE", "LDAUTYPE", incar_dict["# LDAUTYPE"])
            if "# LDAUL" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "# LDAUL", "LDAUL", LDAUL_string)
            elif "LDAUL" in incar_dict.keys():
                incar_dict["LDAUL"] = LDAUL_string

            if "# LDAUU" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "# LDAUU", "LDAUU", LDAUU_string)

            elif "LDAUU" in incar_dict.keys():
                incar_dict["LDAUU"] = LDAUU_string

            if "# LDAUJ" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "# LDAUJ", "LDAUJ", LDAUJ_string)
            elif "LDAUJ" in incar_dict.keys():
                incar_dict["LDAUJ"] = LDAUJ_string
        # comment ldau options. if already commented, just change LDAUU,LDAUL,LDAUJ parameters
        else:
            if "LDAU" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "LDAU", "# LDAU", incar_dict["LDAU"])
            if "LMAXMIX" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "LMAXMIX", "# LMAXMIX", incar_dict["MAXMIX"])
            if "LDAUTYPE" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "LDAUTYPE", "# LDAUTYPE", incar_dict["LDAUTYPE"])
            if "LDAUL" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "LDAUL", "# LDAUL", LDAUL_string)
            elif "# LDAUL" in incar_dict.keys():
                incar_dict["# LDAUL"] = LDAUL_string

            if "LDAUU" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "LDAUU", "# LDAUU", LDAUU_string)
            elif "# LDAUU" in incar_dict.keys():
                incar_dict["# LDAUU"] = LDAUU_string

            if "LDAUJ" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "LDAUJ", "# LDAUJ", LDAUJ_string)
            elif "# LDAUJ" in incar_dict.keys():
                incar_dict["# LDAUJ"] = LDAUJ_string

        if vdw:
            if vdw == "D2":
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
            else :
                if "# IVDW" in incar_dict.keys():
                    incar_dict = change_dict_key(incar_dict, "# IVDW", "IVDW", incar_dict["# IVDW"])
                if "LVDW" in incar_dict.keys():     # If LVDW=.TRUE. is defined, IVDW is automatically set to 1
                    incar_dict = change_dict_key(incar_dict, "LVDW", "# LVDW", ".FALSE.")

                if vdw == "D3":
                    incar_dict["IVDW"] = "11"
                elif vdw == "D3damp":
                    incar_dict["IVDW"] = "12"
                elif vdw == "dDsC":
                    incar_dict["IVDW"] = "4"
        else:
            if "IVDW" in incar_dict.keys():
                incar_dict = change_dict_key(incar_dict, "IVDW", "# IVDW", incar_dict["IVDW"])



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
        if pseudo:
            pot_elt = []
            for e in elements:
                chk=False
                for p in pseudo:
                    if e == p.split("_")[0]:
                        chk=True
                        pot_elt.append(p)
                if not chk:
                    pot_elt.append(e)
        else:
            pot_elt = elements
        potcar = Potcar(symbols=pot_elt, functional=functional)

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
            if not get_pre_options:        # This process is for avoiding multiple inputs generation.
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
                            val = str(incar_dict[key]).split("!")[0]
                            try:
                                description = str(incar_dict[key]).split("!")[1]
                            except:
                                description = ""
                            incar_string += key.ljust(16) + " = " + str(val).ljust(30) + "!" + description + "\n"
                    print(incar_string)
                    get_sets = raw_input("* Anything want to modify or add? (ex: ISPIN=2,ISYM=1,PREC=Accurate // without spacing) if not, enter \"n\" \n: ")
                    if get_sets != "n":
                        vals = get_sets.replace(", ",",")
                        vals = vals.split(",")
                        for val in vals:
                            key = val.split("=")[0]
                            value = val.split("=")[1]
                            if "# "+key in incar_keys:
                                incar_dict = change_dict_key(incar_dict, "# "+key, key, incar_dict["# "+key])
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
                # make INCAR as string type
                incar_keys = incar_dict.keys()
                incar_string = ""
                for key in incar_keys:
                    if key[0] == "#" and key[1].isdigit():
                        incar_string += "\n"
                        incar_string += key + str(incar_dict[key]) + "\n"
                    else:
                        val = str(incar_dict[key]).split("!")[0]
                        try:
                            description = str(incar_dict[key]).split("!")[1]
                        except:
                            description = ""
                        incar_string += key.ljust(16) + " = " + str(val).ljust(30) + "!" + description + "\n"
                incar = incar_string
            else:
                incar_keys = incar_dict.keys()
                incar_string = ""
                for key in incar_keys:
                    if key[0] == "#" and key[1].isdigit():
                        incar_string += "\n"
                        incar_string += key + str(incar_dict[key]) + "\n"
                    else:
                        val = str(incar_dict[key]).split("!")[0]
                        try:
                            description = str(incar_dict[key]).split("!")[1]
                        except:
                            description = ""
                        incar_string += key.ljust(16) + " = " + str(val).ljust(30) + "!" + description + "\n"
                incar = incar_string

        # save current options, for rest inputs
        current_options = {"incar":incar_dict, "magmom":magmom, "ldauu":LDAUU}
        save_json(current_options,"current_options.json")


        ## ----------------------------- Write inputs ---------------------------- ##
        os.chdir(dirname)
        file_writer("POSCAR",str(poscar))
        file_writer("POTCAR",str(potcar))
        file_writer("INCAR",str(incar))
        file_writer("KPOINTS",str(kpoints))
        # -- vdw kernel
        if vdw == "optB86b":
            linux_command("cp ")

        os.chdir("../")


        ## ------------------------- Move structure file ------------------------- ##
        try:
            os.mkdir("structures")
        except:
            pass
        os.rename(self.filename, "./structures/"+self.filename)

        ## --------------------------- Update preset ---------------------------- ##
        if not get_pre_options:
            update_preset = raw_input("* Do you want to update INCAR preset ? (y/n)\n: ")
            if update_preset == "y":
                save_json(incar_dict, home + "/.CCpy/vasp_incar.json")
                print("* Preset options have been updated :" + home + "/.CCpy/vasp_incar.json")



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




















    
