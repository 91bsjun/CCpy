import os, sys
version = sys.version
if version[0] == '3':
    raw_input = input
from math import acos,radians,degrees

import numpy as np
import pandas as pd
import json

from CCpy.Tools.CCpyStructure import PeriodicStructure as ps
from CCpy.Tools.CCpyTools import input_num_parser

class CASMInput():
    def __init__(self, filename):
        self.filename = filename
        fileparsing = ps(filename)
        
        if ".xsd" in filename:
            fileparsing.xsdFile()
            name = filename.replace(".xsd","")
        elif ".cif" in filename:
            fileparsing.cifFile()
            name = filename.replace(".cif","")
        elif "POSCAR" in filename:
            fileparsing.vaspFile()
            name = os.getcwd().split("/")[-1]

        self.atoms = fileparsing.atoms
        self.fcoords = fileparsing.fcoords
        self.lattice_v = fileparsing.lattice_v
        

    def primGen(self):
        atoms = self.atoms        
        fcoords = self.fcoords        
        lattice_v = self.lattice_v
        
        fcoords = fcoords.tolist()
        lattice_v = lattice_v.tolist()

        prim_dic = {}
        
        basis = []

        for i in range(len(atoms)):
            print(str(i).ljust(2)+" : "+atoms[i].ljust(2)+str(fcoords[i][0]).rjust(12)+str(fcoords[i][1]).rjust(12)+str(fcoords[i][2]).rjust(12))

        picking = True
        dopings = {}

        while picking:
            do_pick = raw_input("\nPick sites? (y/n) :")
            if do_pick == "y":
                picked_atoms = input_num_parser(len(atoms))
                elements = raw_input("To which element(s) ? ex) Mn, Co \nCAUTION: Include initial element\n:")
                elements = elements.replace(" ", "").split(",")

                for i in picked_atoms:
                    dopings[i] = elements
            else:
                picking = False

        dopings_keys = list(dopings.keys())

        for i in range(len(atoms)):
            each_basis = {}
            each_basis['coordinate'] = fcoords[i]
            occupants = []

            if i in dopings_keys:
                occupants = [e for e in dopings[i]]
                # occupants.append(atoms[i])
                each_basis['occupant_dof'] = occupants   
            else:
                each_basis['occupant_dof'] = [atoms[i]]

            basis.append(each_basis)

        prim_dic['basis'] = basis
        prim_dic['coordinate_mode'] = "Fractional"
        prim_dic['description'] = "Description"
        prim_dic['lattice_vectors'] = lattice_v
        title = raw_input("Work title: ")
        prim_dic['title'] = title

        jsonString = json.dumps(prim_dic, indent=4)

        f = open("prim.json", "w")
        f.write(jsonString)
        f.close()
        print("done.")
