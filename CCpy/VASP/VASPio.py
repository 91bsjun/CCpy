import os, sys, re
import shutil
import time
import matplotlib.pyplot as plt
import pandas as pd
import json, yaml
import gzip
from pathlib import Path
from collections import OrderedDict

from CCpy.VASP.VASPtools import vasp_incar_json, vasp_phonon_incar_json, magmom_parameters, ldauu_parameters, ldauj_parameters, ldaul_parameters, vasp_grimme_parameters
from CCpy.VASP.VASPtools import line_kpts_generator

from CCpy.Tools.CCpyStructure import PeriodicStructure as PS
from CCpy.Tools.CCpyStructure import latticeGen
from CCpy.Tools.CCpyTools import file_writer, linux_command, change_dict_key, save_json, load_json, progress_bar, bcolors

from pymatgen.core import IStructure as pmgIS
from pymatgen.io.vasp import Vasprun
from pymatgen.io.vasp.inputs import Incar, Poscar, Potcar, Kpoints, Kpoints_supported_modes
from pymatgen.io.vasp.sets import *

import warnings
warnings.filterwarnings("ignore")

version = sys.version
if version[0] == '3':
    raw_input = input

class VASPInput():
    def __init__(self, filename=None, dirname=None, preset_yaml=None, additional_dir=False, keep_files=[]):
        """
        filename: structure filename (*.cif, *POSCAR*, *CONTCAR*)
        dirname: when using additional calc
        additional:
        """
        self.additional_calc = False
        self.additional_dir = additional_dir
        if additional_dir:
            jobname = dirname
            structure = pmgIS.from_file(dirname + "/CONTCAR")
            dirname = dirname + "/" + additional_dir
            self.additional_calc = True
        else:
            if not filename:
                structure, dirname, jobname = None, None, None
                pass               # for run init only
            elif ".xsd" in filename:
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
                jobname = filename
                dirname = filename + "_vasp"
            else:
                print("Not supported file format. (*.xsd, *.cif, *POSCAR*, *CONTCAR*)")
                quit()

            if not dirname:
                dirname = jobname

        self.filename = filename
        self.structure = structure
        self.dirname = dirname

        # ------------ Grimme's parameters ------------- #
        vdw_C6, vdw_R0 = vasp_grimme_parameters()
        # ------------ check preset config ------------- #
        # home = os.getenv("HOME")
        home = os.path.expanduser('~')
        vasp_config_dir = home + "/.CCpy/vasp/"
        MODULE_DIR = str(Path(__file__).resolve().parent)

        self.home = home
        self.vasp_config_dir = vasp_config_dir
        if not os.path.isdir(vasp_config_dir):
            os.makedirs(vasp_config_dir)
            print("* Preset options will be saved under :" + vasp_config_dir)
        configs = os.listdir(vasp_config_dir)

        # INCAR preset check
        yaml_file = ""
        if "default.yaml" in configs:
            default_incar_dict = load_yaml(vasp_config_dir + "default.yaml", "INCAR")
        else:
            shutil.copy('%s' % MODULE_DIR + '/vasp_default.yaml', '%s' % vasp_config_dir + "default.yaml")
            default_incar_dict = load_yaml(vasp_config_dir + "default.yaml", "INCAR")
        default_yaml_file = vasp_config_dir + "default.yaml"

        if preset_yaml:
            if preset_yaml in configs:
                incar_dict = load_yaml(vasp_config_dir + preset_yaml, "INCAR")
            else:
                print("%s not in %s" % (preset_yaml, vasp_config_dir))
                quit()
            yaml_file = vasp_config_dir + preset_yaml
        # Use default
        else:
            incar_dict = default_incar_dict
            yaml_file = default_yaml_file

        try:
            kpt_density = load_yaml(yaml_file, "KPOINTS")['reciprocal_density']
        except:
            kpt_density = load_yaml(default_yaml_file, "KPOINTS")['reciprocal_density']
        try:
            kpt_linemode = load_yaml(yaml_file, "KPOINTS")['linemode']
        except:
            kpt_linemode = False
        try:
            kpt_linemode_file = load_yaml(yaml_file, "KPOINTS")['linemode_file']
        except:
            kpt_linemode_file = False
        try:
            kpt_linemode_use_all_path = load_yaml(yaml_file, "KPOINTS")['use_all_path']
        except:
            kpt_linemode_use_all_path = False
        try:
            kpt_linemode_show_brill = load_yaml(yaml_file, "KPOINTS")['show_brillouin_zone']
        except:
            kpt_linemode_show_brill = False

        try:
            magmom = load_yaml(yaml_file, "MAGMOM")
        except:
            magmom = load_yaml(default_yaml_file, "MAGMOM")
        try:
            LDAU = load_yaml(yaml_file, "LDAU")
        except:
            LDAU = load_yaml(default_yaml_file, "LDAU")
        LDAUU = LDAU['LDAUU']
        LDAUJ = LDAU['LDAUJ']
        LDAUL = LDAU['LDAUL']

        self.incar_dict, self.magmom, self.LDAUL, self.LDAUU, self.LDAUJ, self.vdw_C6, self.vdw_R0 = incar_dict, magmom, LDAUL, LDAUU, LDAUJ, vdw_C6, vdw_R0
        self.kpt_density = kpt_density
        self.kpt_linemode = kpt_linemode
        self.kpt_linemode_file = kpt_linemode_file
        self.kpt_linemode_use_all_path = kpt_linemode_use_all_path
        self.kpt_linemode_show_brill = kpt_linemode_show_brill
        
        self.yaml_file = yaml_file
        self.default_incar_dict = default_incar_dict
        self.incar_dict_desc = load_yaml(MODULE_DIR + '/vasp_incar_desc.yaml')
        if len(keep_files) == 0:
            self.keep_files = load_yaml(yaml_file)["KEEP_FILES"]
        else:
            self.keep_files = keep_files
         


    # ------------------------------------------------------------------------------#
    #                        CMS relaxation VASP input set                          #
    # ------------------------------------------------------------------------------#
    def cms_vasp_set(self, single_point=False, isif=False, vdw=False,
                     spin=False, mag=False, ldau=False,
                     functional="PBE_54", pseudo=None,
                     kpoints=False, get_pre_incar=None, pre_dir="./",
                     batch=False):
        """
        Interactive VASP input generator

        :param single_point: set NSW=0
        :param isif: set ISIF parameter
        :param vdw: perform DFT-D2 calc
        :param spin: set ISPIN=2
        :param functional: POTCAR functional setting
        :param kpoints: list [4,4,1]
        :param get_pre_incar: Load previous option when multiple input generation
        :param batch: in case of batch, avoid confirm menu (use default k-points = input_kpts)

        :return: no return, but write VASP input files at dirname
        """

        structure = self.structure
        dirname = self.dirname
        home = self.home
        incar_dict_desc = self.incar_dict_desc
        pwd = os.getcwd()

        ## ----------------------- Prepare write inputs ------------------------- ##
        try:
            os.mkdir(dirname)
        except:
            files = os.listdir(dirname)
            if "INCAR" in files or "POSCAR" in files or "KPOINTS" in files or "POTCAR" in files:
                ans = raw_input(bcolors.WARNING + dirname+" already exist. Will you override ? (y/n)" + bcolors.ENDC)
                if ans == "y":
                    pass
                else:
                    quit()
            else:
                pass


        # -- Load previous option when multiple input generation
        if get_pre_incar:
            incar_dict = OrderedDict(yaml.load(open(get_pre_incar)))
        # -- Load previous INCAR when additional calc and INCAR in keep_files
        elif self.additional_calc and 'INCAR' in self.keep_files:
            # 1. load default  -->  2. update using previous calc --> 3. update using preset_yaml
            tmp_incar_dict = self.default_incar_dict
            os.chdir(dirname)
            pre_calc_incar = Incar.from_file("../" + pre_dir + '/INCAR')
            os.chdir(pwd)
            tmp_incar_dict = update_incar(tmp_incar_dict, pre_calc_incar)
            incar_dict = update_incar(tmp_incar_dict, self.incar_dict)
        else:
            incar_dict = self.incar_dict

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
        if "SYSTEM" in incar_dict.keys():
            incar_dict["SYSTEM"] = dirname

        # -- Parsing system arguments from user commands
        if single_point:
            incar_dict['NSW'] = 0
        if isif:
            incar_dict['ISIF'] = isif
        if spin:
            incar_dict['ISPIN'] = 2

        # -- edit magmom parameters
        magmom = self.magmom
        if mag and not batch and not magmom_dict:
            print(bcolors.OKGREEN + "\n# ---------- Read MAGMOM value from %s ---------- #" % self.yaml_file + bcolors.ENDC)
            magmom_keys = list(magmom.keys())
            magmom_keys.sort()
            for key in magmom_keys:
                print(str(key).ljust(8) + " = " + str(magmom[key]))
            print("Other atoms which not in here are = 0.6")
            cont = raw_input("Continue (enter)")

        mag_string = ""
        for i in range(len(n_of_atoms)):
            try:
                mag_string += str(n_of_atoms[i]) + "*" + str(magmom[elts[i]]) + " "
            except:
                mag_string += str(n_of_atoms[i]) + "*" + str(0.6) + " "

        if mag:
            incar_dict = update_incar(incar_dict, {"MAGMOM": mag_string}) 
        else:
            incar_dict = update_incar(incar_dict, {"MAGMOM": mag_string}, maintain_block=True)


        # -- LDA+U parameters
        LDAUU = self.LDAUU
        LDAUL = self.LDAUL
        LDAUJ = self.LDAUJ
        if ldau and not batch and not ldau_dict:
            print(bcolors.OKGREEN + "\n# ---------- Read LDA U parameters from %s ---------- #" % self.yaml_file + bcolors.ENDC)
            LDAUU_keys = LDAUU.keys()
            #LDAUU_keys.sort()
            for key in LDAUU_keys:
                print(str(key).ljust(8) + " = " + str(LDAUU[key]))
            print("Other atoms which not in here are = 0")
            cont = raw_input("Continue (enter)")


        LDAUL_string = ""
        for i in range(len(elts)):
            try:
                LDAUL_string += str(LDAUL[elts[i]]) + " "
            except:
                LDAUL_string += str(2) + " "

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

        val_ldau = incar_dict["LDAU"] if "LDAU" in incar_dict.keys() else incar_dict["# LDAU"]
        val_lmix = incar_dict["LMAXMIX"] if "LMAXMIX" in incar_dict.keys() else incar_dict["# LMAXMIX"]
        val_ldau_type = incar_dict["LDAUTYPE"] if "LDAUTYPE" in incar_dict.keys() else incar_dict["# LDAUTYPE"]
        update_ldau = {"LDAU": val_ldau, "LMAXMIX": val_lmix, "LDAUTYPE": val_ldau_type,
                       "LDAUL": LDAUL_string, "LDAUU": LDAUU_string, "LDAUJ": LDAUJ_string}
        if ldau:           # if use ldau option, uncomment LDAU options
            incar_dict = update_incar(incar_dict, update_ldau)
        else:              # else, up to yaml file
            incar_dict = update_incar(incar_dict, update_ldau, maintain_block=True)

        # vdw parameters
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
            else:
                if "LVDW" in incar_dict.keys():     # If LVDW=.TRUE. is defined, IVDW is automatically set to 1
                    incar_dict = change_dict_key(incar_dict, "LVDW", "# LVDW", ".FALSE.")
                if vdw == "D3":
                    ivdw = "11"
                elif vdw == "D3damp":
                    ivdw = "12"
                elif vdw == "dDsC":
                    ivdw = "4"
                incar_dict = update_incar(incar_dict, {"IVDW": ivdw})



        ## -------------------------------- KPOINTS -------------------------------- ##
        # -- if user input the k-points in command
        if kpoints:
            kpts = kpoints
#        else:
#            lattice_vector = structure.lattice.matrix
#            lattice = latticeGen(lattice_vector[0],lattice_vector[1],lattice_vector[2])
#            length = [lattice['length'][0], lattice['length'][1], lattice['length'][2]]
#            kpts = []
#            for param in length:
#                if self.kpt_len // param == 0 or self.kpt_len // param == 1:
#                    kpts.append(1)
#                else:
#                    kpts.append(int(self.kpt_len // param))
#        kpoints = dirname+"\n0\nMonkhorst-Pack\n"+str(kpts[0])+" "+str(kpts[1])+" "+str(kpts[2])+"\n0 0 0\n"
        elif self.kpt_linemode:
            if self.kpt_linemode_file:
                kpoints = open(self.kpt_linemode_file, 'r').read()
            else:
                print(self.kpt_linemode_use_all_path)
                kpoints = line_kpts_generator(structure, use_all=self.kpt_linemode_use_all_path, plot_brillouin_zone=self.kpt_linemode_show_brill)
        else:
            kpoints = str(Kpoints.automatic_density_by_vol(structure, self.kpt_density))

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


        ## --------------------------- Update INCAR  values -------------------------- ##
        if batch:
            incar = incar_dict_to_str(incar_dict, incar_dict_desc)
        else:
            # -- INCAR
            print(dirname)
            highlights = ["NSW", "ISPIN", "ISIF", "PREC", "EDIFF", "IVDW"]
            warnings = []
            if not get_pre_incar:        # This process is for avoiding multiple inputs generation.
                print(bcolors.OKGREEN + "\n# ---------- Read INCAR option from %s ---------- #" % self.yaml_file + bcolors.ENDC)
                get_sets = None
                while get_sets != "n":
                    print("\n# ------------------------------------------------------------------ #")
                    print("#                 Here are the current INCAR options                 #")
                    print("# ------------------------------------------------------------------ #")
                    incar_string = incar_dict_to_str(incar_dict, incar_dict_desc, highlights=highlights, warnings=warnings)
                    print(incar_string)

                    get_sets = raw_input(bcolors.OKGREEN + "* Anything want to modify or add? (ex: ISPIN=2,ISYM=1,#MAGMOM= ) else, enter \"n\" \n: " + bcolors.ENDC)
                    if get_sets != "n":
                        edit_pairs = get_sets.replace(", ",",").replace(" =", "=").replace("= ", "=")
                        edit_pairs = edit_pairs.split(",")
                        input_dict = {}
                        for pair in edit_pairs:
                            key = pair.split("=")[0]
                            if "#" in key and key[1] != " ":
                                key = key.replace("#", "# ")
                            try:
                                value = pair.split("=")[1]
                            except:
                                value = ""
                            input_dict[key] = value
                            warnings.append(key)
                        incar_dict = update_incar(incar_dict, input_dict)
                # make INCAR as string type
                incar_string = incar_dict_to_str(incar_dict, incar_dict_desc)
                incar = incar_string
            else:
                incar_string = incar_dict_to_str(incar_dict, incar_dict_desc)
                incar = incar_string

        # save current options, for rest inputs
        yaml_str = yaml.dump(incar_dict, default_flow_style=False)
        file_writer(".prev_incar.yaml", yaml_str)


        ## ----------------------------- Write inputs ---------------------------- ##
        os.chdir(dirname)
        file_writer("POSCAR",str(poscar))
        file_writer("POTCAR",str(potcar))
        file_writer("INCAR",str(incar))
        file_writer("KPOINTS",str(kpoints))
        os.chdir(pwd)

        ## ----------------------- When Additional Calc ------------------------- ##
        if self.additional_calc:
            os.chdir(dirname)
            if pre_dir[-1] != "/":
                pre_dir += "/"
            for prev_file in self.keep_files:
                if prev_file in os.listdir("../" + pre_dir) and prev_file != 'INCAR':
                    shutil.copy("../" + pre_dir + prev_file, "./")
            if "CONTCAR" in self.keep_files:
                os.remove("POSCAR")
                os.rename("CONTCAR", "POSCAR")
            os.chdir(pwd)
        else:
            # -- backup structure file
            if "structures" not in os.listdir():
                os.mkdir("structures")
            os.rename(self.filename, "./structures/"+self.filename)


    # ------------------------------------------------------------------------------#
    #                     CMS band and DOS calc VASP input set                      #
    # ------------------------------------------------------------------------------#
    def cms_band_set(self, input_line_kpts=None, dos=False):
        ## -------------------------- Copy previous Calc --------------------------- ##
        try:
            os.mkdir("Band-DOS")
        except:
            print("Band-DOS directory is exist already. All files wii be override.")

        os.chdir("Band-DOS")

        prev_files =["CHGCAR", "CONTCAR", "INCAR", "KPOINTS", "POSCAR", "POTCAR"]
        for pf in prev_files:
            if pf in os.listdir("../"):
                shutil.copy("../" + pf, " ./")
        os.rename("POSCAR", "POSCAR.orig")
        os.rename("CONTCAR", "POSCAR")


        ## --------------------------------- INCAR --------------------------------- ##
        f = open("INCAR", "r").read()
        lines = f.split("\n")
        key_val = []
        for l in lines:
            if len(l) == 0:
                pass
            elif l[0] == "#" and l[1].isdigit():
                key_val.append((l, ""))
            else:
                tmp = l.split("=")
                if "#" in tmp[0]:
                    key = tmp[0].replace(" ","").replace("#","")
                    key = "# " + key
                else:
                    key = tmp[0].replace(" ", "")
                key_val.append((key, tmp[1][1:]))

        incar_dict = OrderedDict(key_val)
        incar_keys = incar_dict.keys()

        # -- Band-DOS INCAR
        get_sets = "ICHARG=11,SIGMA=0.02,NSW=0,NEDOS=2001"
        if get_sets != "n":
            vals = get_sets.replace(", ", ",")
            vals = vals.split(",")
            for val in vals:
                key = val.split("=")[0]
                value = val.split("=")[1]
                if "# " + key in incar_keys:
                    incar_dict = change_dict_key(incar_dict, "# " + key, key, incar_dict["# " + key])
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


        # -- make string
        incar_keys = incar_dict.keys()
        incar_string = ""
        for key in incar_keys:
            if key == "SYSTEM":
                incar_string += key.ljust(16) + " = " + str(incar_dict[key]).ljust(30) + "\n"
            elif key[0] == "#" and key[1].isdigit():
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


        ## -------------------------------- KPOINTS -------------------------------- ##
        # -- Line mode KPOINTS is needed to calc band structure
        if not dos:
            if not input_line_kpts:
                print("\n# -------------------------------------------------------- #")
                print("#              Make new line-mode KPOINTS file             #")
                print("# -------------------------------------------------------- #")
                from pymatgen.symmetry.bandstructure import HighSymmKpath

                structure = pmgIS.from_file("POSCAR")
                hsk = HighSymmKpath(structure)
                line_kpoints = Kpoints.automatic_linemode(20, hsk)
                line_kpoints = str(line_kpoints)
                splt_kpts = line_kpoints.split("\n")

                pts = {}
                keys = []
                for kp in splt_kpts:
                    if "!" in kp:
                        tmp = kp.split("!")
                        name = tmp[1].replace(" ","")
                        if name not in pts.keys():
                            pts[name] = tmp[0]
                            keys.append(name)
                print("\n* Available k-points in this structure")
                for key in keys:
                    print(key + " : " + pts[key])
                get_pts = raw_input("\n* Choose k-points to use Band calculations (ex: \Gamma,M,K,L) \n: ")
                get_pts = get_pts.replace(" ", "")
                get_pts = get_pts.split(",")

                line_kpoints = """Line_mode KPOINTS file
    20
    Line_mode
    Reciprocal
    """

                for i in range(len(get_pts)):
                    try:
                        ini = pts[get_pts[i]] + " ! " +get_pts[i] + "\n"
                        fin = pts[get_pts[i + 1]] + " ! " + get_pts[i + 1] + "\n\n"
                        line_kpoints += ini + fin
                    except:
                        ini = pts[get_pts[i]] + " ! " + get_pts[i] + "\n"
                        fin = pts[get_pts[0]] + " ! " + get_pts[0] + "\n\n"
                        line_kpoints += ini + fin

                file_writer("../../KPOINTSP", str(line_kpoints))
                print("\n* Line-mode KPOINTS file has been saved : KPOINTSP")

            # -- Read KPOINTS from user
            else:
                line_kpoints = input_line_kpts


        ## --------------------------- Write input files ---------------------- ##
        file_writer("INCAR",str(incar))
        if not dos:
            file_writer("KPOINTS",str(line_kpoints))
        os.chdir("../")
        sys.stdout.write(" Done !\n")


    # ------------------------------------------------------------------------------#
    #             Accurate optimization for Phonon after relaxation                 #
    # ------------------------------------------------------------------------------#
    def cms_phonon_opt(self):
        """
        Very accurate geometry optimization is required for Phonon calculation.
        This code is for increase accuracy after basic optimization.
        """
        ## -------------------------- Copy previous Calc --------------------------- ##
        try:
            os.mkdir("Phonon_opt")
        except:
            print("Phonon_opt directory is exist already. All files wii be override.")

        os.chdir("Phonon_opt")
        prev_files =["CONTCAR","INCAR", "KPOINTS", "POSCAR", "POTCAR"]
        for pf in prev_files:
            if pf in os.listdir("../"):
                linux_command("cp ../" + pf + " ./")
        os.rename("POSCAR", "POSCAR.orig")
        os.rename("CONTCAR", "POSCAR")
        os.rename("INCAR", "INCAR.orig")
        os.rename("KPOINTS", "KPOINTS.orig")

        ## --------------------------------- INCAR --------------------------------- ##
        f = open("INCAR.orig", "r").read()
        lines = f.split("\n")
        key_val = []
        for l in lines:
            if len(l) == 0:
                pass
            elif l[0] == "#" and l[1].isdigit():
                key_val.append((l, ""))
            else:
                tmp = l.split("=")
                if "#" in tmp[0]:
                    key = tmp[0].replace(" ","").replace("#","")
                    key = "# " + key
                else:
                    key = tmp[0].replace(" ", "")
                key_val.append((key, tmp[1][1:]))

        incar_dict = OrderedDict(key_val)
        incar_keys = incar_dict.keys()

        # -- Accurate opt INCAR
        # make encut to ENMAX * 1.5
        potcar = open("POTCAR").read()
        enmax_flag = re.compile("ENMAX\s*=\s*(\d*[.]\d*);", re.M)
        enmaxs = enmax_flag.findall(potcar)
        import numpy as np
        enmaxs = np.array(enmaxs, dtype='float32')
        encut = int((max(enmaxs) * 1.5 + 5) // 10 * 10)
        encut = max(encut, 520)

        get_sets = "PREC=Accurate,EDIFF=1.0E-08,EDIFFG=-1.0E-06,ADDGRID=.True.,LREAL=.FALSE.,LWAVE=.FALSE.,LCHARG=.FALSE.,ENCUT=%d" % encut
        if get_sets != "n":
            vals = get_sets.replace(", ", ",")
            vals = vals.split(",")
            for val in vals:
                key = val.split("=")[0]
                value = val.split("=")[1]
                if "# " + key in incar_keys:
                    incar_dict = change_dict_key(incar_dict, "# " + key, key, incar_dict["# " + key])
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


        # -- make string
        incar_keys = incar_dict.keys()
        incar_string = ""
        for key in incar_keys:
            if key == "SYSTEM":
                incar_string += key.ljust(16) + " = " + str(incar_dict[key]).ljust(30) + "\n"
            elif key[0] == "#" and key[1].isdigit():
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


        ## -------------------------------- KPOINTS -------------------------------- ##
        # -- if user input the k-points in command
        structure = pmgIS.from_file("POSCAR")
        lat = structure.lattice
        length = [lat.a, lat.b, lat.c]
        kpts = []
        for param in length:
            if param >= 19:
                kpts.append(1)
            else:
                kpts.append(int(60 // param))
        kpoints = "High Kpoints\n0\nMonkhorst-Pack\n"+str(kpts[0])+" "+str(kpts[1])+" "+str(kpts[2])+"\n0 0 0\n"

        ## --------------------------- Write input files ---------------------- ##
        file_writer("INCAR",str(incar))
        file_writer("KPOINTS",str(kpoints))
        os.chdir("../")
        sys.stdout.write(" Done !\n")
        
    

    def MIT_relax_set(self):
        structure = self.structure
        dirname = self.dirname
        mit_relax = MITRelaxSet(structure)
        mit_relax.write_input(dirname)

    def MP_relax_set(self, user_incar):
        structure = self.structure
        dirname = self.dirname
        mit_relax = MPRelaxSet(structure, user_incar_settings=user_incar)
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

    def getFinalStructure(self, filename="CONTCAR", target_name="None", path="../"):
        from pymatgen.io.cif import CifWriter
        
        structure_object = pmgIS.from_file(filename)
        cif = CifWriter(structure_object)

        cif.write_file(target_name)
        print(target_name + " has been generated.")

        if path == "./" or path == ".":
            pass
        else:
            linux_command("mv "+target_name+" "+path)

    def getConvergence(self, show_plot=True):
        if "OUTCAR.gz" in os.listdir("./"):
            OUTCAR = gzip.open("OUTCAR", "rb").read()
            OUTCAR = str(OUTCAR)
        else:
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


    def get_energy_list(self, show_plot=True, dirs=None, sort=False):
        # dirs = [d for d in os.listdir("./") if os.path.isdir(d)]
        out_dirs = [d for d in dirs if "OUTCAR" in os.listdir(d) or "OUTCAR.gz" in os.listdir(d)]
        out_dirs.sort()

        x = range(len(out_dirs))
        energies = []
        energies_per_atom = []
        converged = []
        status = []
        pwd = os.getcwd()
        print("\n    Parsing VASP jobs....")
        cnt = 0
        for o in out_dirs:
            msg = "  [  " + str(cnt + 1).rjust(6) + " / " + str(len(dirs)).rjust(6) + "  ]"
            cnt += 1
            sys.stdout.write(msg)
            sys.stdout.flush()
            sys.stdout.write("\b" * len(msg))
            os.chdir(o)
            if "OUTCAR.gz" in os.listdir("./"):
                OUTCAR = gzip.open("OUTCAR.gz", "rb").read()
                OUTCAR = str(OUTCAR)
            else:
                OUTCAR = open("OUTCAR", "r").read()
            # -- energy parsing
            findE = re.compile("free  energy   TOTEN  =\s+\S+", re.M)
            strings = findE.findall(OUTCAR)
            e = []
            for s in strings:
                e.append(float(s.split()[4]))
            # -- find number of atoms
            st = pmgIS.from_file("POSCAR")
            atoms = [str(i) for i in st.species]
            if len(e) == 0:
                energies.append(0)
                energies_per_atom.append(0)
            else:
                energies.append(e[-1])
                e_per_atom = float(e[-1]) / float(len(atoms))
                energies_per_atom.append(e_per_atom)

            stat, done, cvgd, electronic_converged, ionic_converged, zipped, err_msg = self.vasp_status()
            converged.append(cvgd)
            status.append(stat)

            os.chdir(pwd)

        energy_list = {}
        energy_list['Directory'] = out_dirs
        energy_list['Total energy (eV)'] = energies
        energy_list['Energy/atom (eV)'] = energies_per_atom
        energy_list['  Converged'] = converged
        energy_list['  Job Status'] = status

        df = pd.DataFrame(energy_list)
        #df = df[['Directory', 'Total energy (eV)', 'Energy/atom (eV)', '    Job end', '  Converged']]
        df = df[['Directory', 'Total energy (eV)', 'Energy/atom (eV)','  Converged', '  Job Status']]
        pd.set_option('display.max_rows', None)
        pd.set_option('expand_frame_repr', False)
        if sort == "tot":
            df = df.sort_values(by='Total energy (eV)')
        elif sort == "atom":
            df = df.sort_values(by='Energy/atom (eV)')
        print(df)
        pwd = os.getcwd()
        pwd = pwd.split("/")[-1]
        csv_filename = "03_"+pwd+"_FinalEnergies.csv"
        txt_filename = "03_"+pwd+"_FinalEnergies.txt"
        df.to_csv(csv_filename)
        f = open(txt_filename, "w")
        f.write(df.to_string())
        f.close()
        print("Energy list files have been saved: " + csv_filename + ", " + txt_filename)

        if show_plot:
            fig = plt.figure(figsize=(8, 7))
            plt.plot(x, energies, marker='o', color='#0054FF')
            plt.xticks(x, out_dirs, rotation=45)
            plt.tight_layout()
            plt.grid()
            plt.show()

    def vasp_status(self):
        """
        check VASP status in current directory

        return Status, Convergence, Done, Zipped
        """
        stat, converged, electronic_converged, ionic_converged, done, zipped, err_msg = " ", " ", " ", " ", " ", " ", " "
        # -- only inputs in dir or OUTCAR not in directory
        if "vasp.done" in os.listdir("./"):
            stat = "End"

        # -- OUTCAR
        if "OUTCAR" not in os.listdir("./") and "OUTCAR.gz" not in os.listdir("./"):
            stat = "Not Started"
        elif "vasp.done" not in os.listdir("./"):
            stat = "Not finished"
        else:
            #outcar = os.popen("tail OUTCAR").read()
            #if len(outcar) == 0:
            #    stat = "Not Started"
            # -- properly terminated
            #if "User time (sec):" in outcar:
            #    stat = "Properly terminated"
            #else:
            #    stat = "Not properly terminated"


            # -- check converged
            # -- using vasp.out
            from custodian.vasp.handlers import VaspErrorHandler
            if "vasp.out" not in os.listdir("./"):
                converged, electronic_converged, ionic_converged = "False", "False", "False"
            else:
                subset = VaspErrorHandler.error_msgs
                incar = Incar.from_file("INCAR")
                try:
                    # if NSW not mentioned in INCAR file, default NSW=0
                    nsw = incar['NSW']
                except:
                    nsw = 0
                subset['max_ionic'] = ['%s F=' % (nsw)]
                veh = VaspErrorHandler(errors_subset_to_catch=subset)
                converged = str(not veh.check())
                electronic_converged, ionic_converged = converged, converged           
                if converged == "False":
                    err_msg = list(veh.errors)[0]
                    if err_msg == "max_ionic":
                        electronic_converged = "True"
            
            '''
            # using vasprun.xml
            try:
                v = Vasprun("vasprun.xml", parse_dos=False, parse_eigen=False, parse_potcar_file=False)
                converged = str(v.converged)
                if converged == "False":
                    electronic_converged = v.converged_electronic
                    ionic_converged = v.converged_ionic
                else:
                    electronic_converged, ionic_converged = "True", "True"
            except:
                try:
                    v = Vasprun("vasprun.xml.gz")
                    converged = str(v.converged)
                    if converged == "False":
                        electronic_converged = v.converged_electronic
                        ionic_converged = v.converged_ionic
                    else:
                        electronic_converged, ionic_converged = "True", "True"
                except:
                    converged, electronic_converged, ionic_converged = "False", "False", "False"
            
            '''
        # -- zipped or not
        out_files = ['CHG', 'CHGCAR', 'DOSCAR', 'OUTCAR', 'PROCAR', 'vasprun.xml', 'XDATCAR']
        if converged == "True":
            zipped = "True"
            for f in out_files:
                if f in os.listdir("./"):
                    zipped = "False"

        return stat, done, converged, electronic_converged, ionic_converged, zipped, err_msg



    def check_terminated(self, dirs=[]):
        tot_status, tot_converged, tot_e_converged, tot_i_converged, tot_finished, tot_zipped, tot_err_msg = [], [], [], [], [], [], []
        pwd = os.getcwd()
        print("\n    Parsing VASP jobs....")
        cnt = 0
        for d in dirs:
            msg = "  [  " + str(cnt+1).rjust(6) + " / " + str(len(dirs)).rjust(6) + "  ]"
            cnt += 1
            sys.stdout.write(msg)
            sys.stdout.flush()
            sys.stdout.write("\b" * len(msg))
            os.chdir(d)

            stat, done, converged, electronic_converged, ionic_converged, zipped, err_msg = self.vasp_status()

            tot_status.append(stat)
            tot_converged.append(converged)
            tot_i_converged.append(electronic_converged)
            tot_e_converged.append(ionic_converged)
            tot_finished.append(done)
            tot_zipped.append(zipped)
            tot_err_msg.append(err_msg)
            os.chdir(pwd)

        #df = pd.DataFrame({"Directory": dirs, "    Job end": tot_finished, "Status": tot_status, "  Converged": tot_converged,
        #                   "  Elec-converged": tot_e_converged, "  Ion-converged": tot_i_converged, "  Zipped": tot_zipped, "  Err msg": tot_err_msg})
        #df = df[['Directory', 'Status', '    Job end', '  Converged', '  Elec-converged', '  Ion-converged', '  Zipped', '  Err msg']]
        df = pd.DataFrame({"Directory": dirs, "Status": tot_status, "  Converged": tot_converged,
                           "  Elec-converged": tot_e_converged, "  Ion-converged": tot_i_converged, "  Zipped": tot_zipped, "  Err msg": tot_err_msg})
        df = df[['Directory', 'Status','  Converged', '  Elec-converged', '  Ion-converged', '  Zipped', '  Err msg']]
        #df = df[['Directory', 'Status','  Converged', '  Zipped', '  Err msg']]

        pd.set_option('display.max_rows', None)


        # -- Show job infos
        total = len(df)
        #done = len(df[(df['    Job end'] == "True")])
        done = len(df[(df['Status'] == "End")])
        cvg_df = df[(df['  Converged'] == "True")]
        converged = len(cvg_df)
        not_cvg_df = df[(df['  Converged'] == "False")]
        #not_cvg_df = not_cvg_df[(not_cvg_df['    Job end'] == "True")]
        not_cvg_df = not_cvg_df[(not_cvg_df['Status'] == "End")]
        not_converged = len(not_cvg_df)
        zipped = len(df[(df['  Zipped'] == "True")])

        counts = {'Total':[total], '    Job end':[done], '  Zipped':[zipped], '  Converged':[converged], 'Unconverged':[not_converged]}

        count_df = pd.DataFrame(counts)
        count_df = count_df[['Total', '    Job end', '  Converged', 'Unconverged', '  Zipped']]
        print("\n\n* Current status :")
        print(count_df)

        df.to_csv(".00_job_status.csv")
        filename = "00_job_status.txt"
        f = open("00_job_status.txt", "w")
        f.write(df.to_string())
        print("\n* Unconverged jobs : " + str(len(not_cvg_df)) + " (01_unconverged_jobs.csv)")

        if len(not_cvg_df) != 0:
            print(not_cvg_df)
            not_cvg_df.to_csv("01_unconverged_jobs.csv")
            print("You can recalculate using '01_unconverged_jobs.csv' file.")
        else:
            print("There are no jobs that have not been converged.")
        print("\n* Detail information saved in: 00_jobs_status.txt")

   
    def vasp_error_handle(self, dirs):
        from custodian.vasp.handlers import VaspErrorHandler, UnconvergedErrorHandler
        import json

        pwd = os.getcwd()
        err_log = {}
        cnt = 0
        print("\n    Parsing ERROR jobs....")
        for d in dirs:
            msg = "  [  " + str(cnt + 1).rjust(6) + " / " + str(len(dirs)).rjust(6) + "  ]"
            cnt += 1
            sys.stdout.write(msg)
            sys.stdout.flush()
            sys.stdout.write("\b" * len(msg))
            os.chdir(d)
            # -- 1. check error
            veh = VaspErrorHandler()
            err = veh.check()
            err_action = " "
            if err:
                err_action = veh.correct()
            # -- 2. check unconverged
            else:
                ueh = UnconvergedErrorHandler()
                ueh_check = ueh.check()
                err_action = ueh.correct()
            err_log[d] = OrderedDict(err_action)
            os.chdir(pwd)
        err_log = OrderedDict(err_log)
        # -- ordered dict encoding to yaml
        def represent_dictionary_order(self, dict_data):
            return self.represent_mapping('tag:yaml.org,2002:map', dict_data.items())
        yaml.add_representer(OrderedDict, represent_dictionary_order)
        # -- save error log as yaml file
        f = open("02_error_handled.yaml", "w")
        f.write(yaml.dump(err_log, default_flow_style=False))
        f.close()
        print("* Handled error log saved: 02_error_handled.yaml")
        print("\nDone.")

    def vasp_zip(self, dirs, minimize=False):
        cnt = 0
        pwd = os.getcwd()
        minimum_length = 8
        for d in dirs:
            if len(d) > minimum_length:
                minimum_length = len(d)

        def gzip_exec(file_list):
            for f in file_list:
                if f in os.listdir("./"):
                    fgz = f + ".gz"
                    if fgz in os.listdir("./"):
                        os.system("mv %s %s" % (fgz, fgz.replace(".gz", ".1.gz")))
                    os.system("gzip %s" % f)
        def rm_exec(file_list):
            for f in file_list:
                if f in os.listdir("./"):
                    os.remove(f)
       

        total = len(dirs)
        cnt = 1
        progress_bar(total, 0, 50)
        for d in dirs:
            os.chdir(d)
            cmt = 'Dir: ' + d.ljust(minimum_length)
            progress_bar(total, cnt, 50, cmt=cmt)
            #msg = "  [  " + str(cnt+1).rjust(6) + " / " + str(len(dirs)).rjust(6) + "  ]"
            #msg = "Current directory: " + d.ljust(minimum_length) + msg
            #sys.stdout.write(msg)
            #sys.stdout.flush()
            #sys.stdout.write("\b" * len(msg))

            if minimize:
                gzip_exec(['OUTCAR', 'vasprun.xml', 'XDATCAR'])
                rm_exec(['CHG', 'CHG.gz', 'CHGCAR', 'CHGCAR.gz', 'DOSCAR', 'DOSCAR.gz', 'PROCAR', 'PROCAR.gz'])
            else:
                gzip_exec(['CHG', 'CHGCAR', 'DOSCAR', 'OUTCAR', 'PROCAR', 'vasprun.xml', 'XDATCAR'])

            cnt+=1
            os.chdir(pwd)
        print("\nDone.")

def load_yaml(yaml_file, key=None):
    dict = yaml.load(open(yaml_file))
    dict = OrderedDict(dict[key]) if key else OrderedDict(dict)

    return dict

def incar_dict_to_str(incar_dict, incar_dict_desc, highlights=[], warnings=[]):
    incar_keys = incar_dict.keys()
    incar_string = ""
    for key in incar_keys:
        if key == "SYSTEM":
            incar_string += key.ljust(16) + " = " + str(incar_dict[key]).ljust(30) + "\n"
        elif 'SECTION' in key:
            incar_string += "\n"
            incar_string += incar_dict[key] + "\n"
        else:
            val = str(incar_dict[key])
            if key in incar_dict_desc.keys():
                description = str(incar_dict_desc[key])
            elif "# " in key:
                description = str(incar_dict_desc[key.replace("# ", "")])
            else:
                description = ""

            if key in warnings or key.replace("# ", "") in warnings:
                key = bcolors.WARNING + key + bcolors.ENDC 
                val = bcolors.WARNING + val + bcolors.ENDC 
                incar_string += key.ljust(25) + " = " + str(val).ljust(39) + "! " + description + "\n"
            elif key in highlights or key.replace("# ", "") in highlights:
                key = bcolors.OKGREEN + key + bcolors.ENDC 
                val = bcolors.OKGREEN + val + bcolors.ENDC 
                incar_string += key.ljust(25) + " = " + str(val).ljust(39) + "! " + description + "\n"
            else:
                incar_string += key.ljust(16) + " = " + str(val).ljust(30) + "! " + description + "\n"

    return incar_string

def update_incar(incar_dict, input_option, maintain_block=False):
    """
    1. block   -> block          # LDAUU =    --> # LDAUU = 
    2. blcok   -> unblock        # IVDW = 12  --> IVDW = 12
    3. unblock -> unblock        ISIF = 2     --> ISIF = 3
    4. unblock -> block          IVDW = 12    --> # IVDW = 
    """
    # change_dict_key(ordered_dict, ori_key, new_key, new_val)
    for key in input_option:
        if "# " in key:
            key_type = "block"
            block_key = key
            unblock_key = key.replace("# ", "")
            if block_key in incar_dict.keys():                          # unblock -> unblock
                incar_dict[block_key] = input_option[key]
            elif unblock_key in incar_dict.keys() and maintain_block:       # block -> block
                incar_dict[unblock_key] = input_option[key]
            elif unblock_key not in incar_dict.keys() and block_key not in incar_dict.keys():     # new key
                incar_dict[key] = input_option[key]
            else:
                incar_dict = change_dict_key(incar_dict, unblock_key, block_key, input_option[key])   # unblock -> block
        else:
            key_type = "unblock"
            block_key = "# " + key
            unblock_key = key
            if unblock_key in incar_dict.keys():                          # unblock -> unblock
                incar_dict[key] = input_option[key]
            elif block_key in incar_dict.keys() and maintain_block:       # block -> block
                incar_dict[block_key] = input_option[key]
            elif unblock_key not in incar_dict.keys() and block_key not in incar_dict.keys():     # new key
                incar_dict[key] = input_option[key]
            else:
                incar_dict = change_dict_key(incar_dict, block_key, unblock_key, input_option[key])   # unblock -> block

    return incar_dict









    
