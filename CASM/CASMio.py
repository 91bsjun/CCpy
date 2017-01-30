import os, sys

from math import acos,radians,degrees

import numpy as np
import pandas as pd
import json

from CCpy.Tools.CCpyStructure import PeriodicStructure as ps

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

        dopings = raw_input("Pick atom(s) : ")
        if len(dopings) != 0:
            dopings = dopings.split(",")
            dopings = [int(i) for i in dopings]

            elements = raw_input("To which element ?")
            elements = elements.split(",")
        else:
            dopings = []

        for i in range(len(atoms)):
            each_basis = {}
            each_basis['coordinate'] = fcoords[i]
            occupants = []

            if i in dopings:
                occupants = [e for e in elements]
                occupants.append(atoms[i])
                each_basis['occupant_dof'] = occupants   
            else:
                each_basis['occupant_dof'] = [atoms[i]]

            basis.append(each_basis)

        prim_dic['basis'] = basis
        prim_dic['coordinate_mode'] = "Fractional"
        prim_dic['description'] = "Hello"
        prim_dic['lattice_vectors'] = lattice_v
        prim_dic['title'] = "MyTitle"

        jsonString = json.dumps(prim_dic, indent=4)

        f = open("prim.json", "w")
        f.write(jsonString)
        f.close()
