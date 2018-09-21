#!/usr/local/bin/python2.7

import os, sys, re
from subprocess import call as shl

import numpy as np
import pandas as pd
from math import sqrt, exp

from CCpy.Tools.CCpyStructure import NonPeriodicCoordinates as npc

class QchemInput():
    def __init__(self, filename, molecule_len=None, center_number=None):
        self.filename = filename
        self.molecule_len = molecule_len
        self.center_number = center_number
        
    def parsingStructure(self):
        filename = self.filename
        mynpc = npc(filename)
        if ".car" in filename:
            mynpc.carFile()
        elif ".xsd" in filename:
            mynpc.xsdFile()
        elif ".xyz" in filename:
            mynpc.xyzFile()

        self.n_atoms, self.atoms, self.coords, self.name = mynpc.n_atoms, mynpc.atoms, mynpc.coords, mynpc.name
            
    def pairInputGen(self):
        name, n_atoms, atoms, coords = self.name, self.n_atoms, self.atoms, self.coords
        center_number = n_atoms / 2        

        atoms_pair1 = atoms[:center_number]
        coords_pair1 = coords[:center_number]
        n_atoms = len(atoms_pair1)
        mynpc = npc()
        mynpc.n_atoms, mynpc.atoms, mynpc.coords = n_atoms, atoms_pair1, coords_pair1
        coordinates1 = mynpc.return_coordinates()        
        
        atoms_pair2 = atoms[center_number:]
        coords_pair2 = coords[center_number:]
        mynpc = npc()
        mynpc.n_atoms, mynpc.atoms, mynpc.coords = n_atoms, atoms_pair2, coords_pair2
        coordinates2 = mynpc.return_coordinates()

        hole_filename = name+"_pair_H.in"
        elec_filename = name+"_pair_E.in"

        self.writeHoleInput(hole_filename, coordinates1, coordinates2)
        self.writeElecInput(elec_filename, coordinates1, coordinates2)

        center_xs = np.array([row[0] for row in coords_pair1])
        center_ys = np.array([row[1] for row in coords_pair1])
        center_zs = np.array([row[2] for row in coords_pair1])
        centroid_center = np.array([center_xs.mean(), center_ys.mean(), center_zs.mean()])

        second_xs = np.array([row[0] for row in coords_pair2])
        second_ys = np.array([row[1] for row in coords_pair2])
        second_zs = np.array([row[2] for row in coords_pair2])
        centroid_second = np.array([second_xs.mean(), second_ys.mean(), second_zs.mean()])
        
        distance = sqrt(pow(centroid_center[0]-centroid_second[0], 2) + pow(centroid_center[1]-centroid_second[1], 2) + pow(centroid_center[2]-centroid_second[2], 2))

        name = os.getcwd()
        name = name.split("/")[-1]

        
        try:
            df = pd.read_csv("00Data_"+name+".csv", index_col=0)
            info = {'Hole Files':hole_filename, 'Elec Files':elec_filename, 'Distance':distance}            
            df = df.append(info, ignore_index=True)
        except:
            info = {'Hole Files':[hole_filename], 'Elec Files':[elec_filename], 'Distance':[distance]}
            df = pd.DataFrame(info)
        
        df = df[['Hole Files','Elec Files', 'Distance']]
        df.to_csv("00Data_"+name+".csv")

        shl("cp 00Data_"+name+".csv .00Data_"+name+".csv", shell=True)
              

    def clusterInputGen(self):
        name, n_atoms, atoms, coords = self.name, self.n_atoms, self.atoms, self.coords
        molecule_len = self.molecule_len
        center_number = self.center_number

        if n_atoms % molecule_len != 0:
            print("Missmatched with total number of molecule and single molecule")
            quit()
            
        n_molecules = n_atoms/molecule_len

        mynpc = npc()
        center_atoms = atoms[center_number-1:center_number+molecule_len-1]
        center_coords = coords[center_number-1:center_number+molecule_len-1]
        mynpc.n_atoms, mynpc.atoms, mynpc.coords = molecule_len, center_atoms, center_coords
        center_coordinates = mynpc.return_coordinates()

        center_xs = np.array([row[0] for row in center_coords])
        center_ys = np.array([row[1] for row in center_coords])
        center_zs = np.array([row[2] for row in center_coords])

        centroid_center = np.array([center_xs.mean(), center_ys.mean(), center_zs.mean()])

        os.mkdir(name)
        os.chdir(name)

        info = {}
        hole_inputs = []
        elec_inputs = []
        distances = []

        for i in range(n_molecules):
            second_pair_atoms = atoms[i*molecule_len:(i+1)*molecule_len]
            second_pair_coords = coords[i*molecule_len:(i+1)*molecule_len]
            mynpc = npc()
            mynpc.n_atoms, mynpc.atoms, mynpc.coords = molecule_len, second_pair_atoms, second_pair_coords
            second_coordinates = mynpc.return_coordinates()

            second_xs = np.array([row[0] for row in second_pair_coords])
            second_ys = np.array([row[1] for row in second_pair_coords])
            second_zs = np.array([row[2] for row in second_pair_coords])

            centroid_second = np.array([second_xs.mean(), second_ys.mean(), second_zs.mean()])

            distance = sqrt(pow(centroid_center[0]-centroid_second[0], 2) + pow(centroid_center[1]-centroid_second[1], 2) + pow(centroid_center[2]-centroid_second[2], 2))

            hole_filename = name+"_cluster%2d"%i+"_H.in"
            hole_filename = hole_filename.replace(" ","0")
            elec_filename = name+"_cluster%2d"%i+"_E.in"
            elec_filename = elec_filename.replace(" ","0")

            if center_coordinates == second_coordinates:
                pass
            else:
                hole_inputs.append(hole_filename)
                elec_inputs.append(elec_filename)
                distances.append(distance)
                self.writeHoleInput(hole_filename, center_coordinates, second_coordinates)
                print(hole_filename)
                self.writeElecInput(elec_filename, center_coordinates, second_coordinates)
                print(elec_filename)
        
        info['Hole Files'] = hole_inputs
        info['Elec Files'] = elec_inputs
        info['Distance'] = distances

        df = pd.DataFrame(info)
        df = df[['Hole Files','Elec Files', 'Distance']]

        df.to_csv("00Data_"+name+".csv")
        shl("cp 00Data_"+name+".csv .00Data_"+name+".csv", shell=True)
        
        os.chdir("../")
        


    def writeHoleInput(self, filename, coords1, coords2):
        hole_file = open(filename, "w")
        hole_file.write('''$molecule
1 2
--
1 2, 0 1
%s
--
0 1, 1 2
%s
$end
$rem
JOBTYPE SP
EXCHANGE HF
BASIS 6-31Gd
MAX_SCF_CYCLES 200
SCF_ALGORITHM DIIS
SCF_CONVERGENCE 5
SCF_PRINT_FRGM TRUE
SYM_IGNORE TRUE
SCF_GUESS FRAGMO
STS_DC TRUE
''' % (coords1, coords2)
                        )

    def writeElecInput(self, filename, coords1, coords2):
        elec_file = open(filename, "w")
        elec_file.write('''$molecule
-1 2
--
-1 2, 0 1
%s
--
0 1, -1 2
%s
$end
$rem
JOBTYPE SP
EXCHANGE HF
BASIS 6-31Gd
MAX_SCF_CYCLES 200
SCF_ALGORITHM DIIS
SCF_CONVERGENCE 5
SCF_PRINT_FRGM TRUE
SYM_IGNORE TRUE
SCF_GUESS FRAGMO
STS_DC TRUE
''' % (coords1, coords2)
                        )


class QchemOutput():
    def __init__(self, filename):
        self.filename = filename
        
        out = open(filename, "r").read()
        dc_pattern = re.compile("Effective Coupling \(in eV\) = \s+\S+")
        m = dc_pattern.search(out)
        dc = m.group(0)
        dc = dc.split()[-1]

        terminate_pattern = re.compile("Thank you very much for using Q-Chem")
        m = terminate_pattern.search(out)
        if m:
            chk = True
        else:
            chk = False

        self.finished = chk
        self.dc = dc

    def checkTerminated(self):
        finished = self.finished

        return finished

    def getChargetransferIntegral(self):
        filename = self.filename
        if not self.checkTerminated():
            print("This file is NOT PROPERLY TERMINATED : "+filename)
            return "ERR"

        v = self.dc

        return v

def calcMobility(V, r, roe):
    # constants
    pi = 3.14
    k = 0.0000861734315
    h = float(4.13566733E-15)
    T = 300.0
    n = 3.0 # dimension
    
    ket = 2.0 * pow(pi,3.0/2.0) / h * pow(1.0/(roe*k*T),1.0/2.0) * pow(V,2.0) * exp(-(roe/(4.0*k*T)))
    
    W = ket
    P = 1.0 # portion
    r = r * pow(10.0,-8.0)

    D = 1.0/(2.0*n) * pow(r,2.0) * W * P

    u = D / (k*T)
    
    return D, u
    




