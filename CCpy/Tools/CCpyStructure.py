import os,sys
import numpy as np
from math import acos,radians,degrees

class NonPeriodicCoordinates():
    def __init__(self, filename=None):
        self.filename = filename

    def carFile(self):
        filename = self.filename
        f = open(filename,"r")
        lines = f.readlines()
        f.close()

        n_atoms = 0
        atoms = []
        coords = []
        for j in lines:
            spl = j.split()
            if len(spl) < 8:
                pass
            else:
                atoms.append(spl[7])
                coords.append([float(spl[1]), float(spl[2]), float(spl[3])])
                n_atoms += 1

        coordinate = ""
        for j in range(n_atoms):
            if j == n_atoms-1:
                xyz = atoms[j]+"\t"+str(coords[j][0]).rjust(14)+"\t"+str(coords[j][1]).rjust(14)+"\t"+str(coords[j][2]).rjust(14)
            else:
                xyz = atoms[j]+"\t"+str(coords[j][0]).rjust(14)+"\t"+str(coords[j][1]).rjust(14)+"\t"+str(coords[j][2]).rjust(14)+"\n"
            coordinate = coordinate + xyz

        self.n_atoms = n_atoms
        self.atoms = atoms
        self.coords = coords
        self.name = filename.replace(".car","")
        
        return coordinate

    def xyzFile(self):
        filename = self.filename
        f = open(filename,"r")
        lines = f.readlines()
        f.close()

        n_atoms = 0
        atoms = []
        coords = []
        
        coordinate = ""
        for line in lines[2:]:
            if len(line.split()) == 4:
                coordinate += line
                spl = line.split()
                atoms.append(spl[0])
                coords.append([float(spl[1]), float(spl[2]), float(spl[3])])
                n_atoms+=1

        self.n_atoms = n_atoms
        self.atoms = atoms
        self.coords = coords
        self.name = filename.replace(".xyz","")        

        return coordinate

    def xsdFile(self):
        filename = self.filename
        f = open(filename,"r")
        lines = f.readlines()
        f.close()

        n_atoms = 0
        atoms = []
        coords = []

        for line in lines:
            if "Atom3d" in line and "XYZ" in line:
                tmp = line.split()
                for i in range(len(tmp)):
                    if "XYZ=" in tmp[i]:
                        xyz_index = i
                    elif "Components=" in tmp[i]:
                        atom_index = i
                
                xyz = tmp[xyz_index]
                xyz = xyz.replace("XYZ=\"","").replace("\"","")
                xyz = xyz.split(",")
                coords.append([float(xyz[0]), float(xyz[1]), float(xyz[2])])

                atom = tmp[atom_index]
                atom = atom.replace("Components=","").replace("\"","").replace(">","").replace("/","")
                atoms.append(atom)

                n_atoms += 1

        coordinate = ""
        for j in range(n_atoms):
            if j == n_atoms-1:
                xyz = atoms[j]+"\t"+str(round(coords[j][0],10)).rjust(14)+"\t"+str(round(coords[j][1],10)).rjust(14)+"\t"+str(round(coords[j][2],10)).rjust(14)
            else:
                xyz = atoms[j]+"\t"+str(round(coords[j][0],10)).rjust(14)+"\t"+str(round(coords[j][1],10)).rjust(14)+"\t"+str(round(coords[j][2],10)).rjust(14)+"\n"
            coordinate = coordinate + xyz

        self.n_atoms = n_atoms
        self.atoms = atoms
        self.coords = coords
        self.name = filename.replace(".xsd","")

        return coordinate

    def to_xyzFile(self):
        name = self.name
        n_atoms = self.n_atoms
        atoms = self.atoms
        coords = self.coords
        
        filename = name+".xyz"

        f = open(filename, "w")
        f.write(str(n_atoms))
        f.write("\n\n")
        for j in range(n_atoms):
            if j == n_atoms-1:
                f.write(atoms[j]+"\t"+str(round(coords[j][0],10)).rjust(14)+"\t"+str(round(coords[j][1],10)).rjust(14)+"\t"+str(round(coords[j][2],10)).rjust(14))
            else:
                f.write(atoms[j]+"\t"+str(round(coords[j][0],10)).rjust(14)+"\t"+str(round(coords[j][1],10)).rjust(14)+"\t"+str(round(coords[j][2],10)).rjust(14)+"\n")
        f.close()

    def return_coordinates(self):
        n_atoms = self.n_atoms
        atoms = self.atoms
        coords = self.coords

        coordinate = ""
        for j in range(n_atoms):
            if j == n_atoms-1:
                xyz = atoms[j]+"\t"+str(round(coords[j][0],10)).rjust(14)+"\t"+str(round(coords[j][1],10)).rjust(14)+"\t"+str(round(coords[j][2],10)).rjust(14)
            else:
                xyz = atoms[j]+"\t"+str(round(coords[j][0],10)).rjust(14)+"\t"+str(round(coords[j][1],10)).rjust(14)+"\t"+str(round(coords[j][2],10)).rjust(14)+"\n"
            coordinate = coordinate + xyz

        return coordinate

class PeriodicStructure():
    def __init__(self, filename=None):
        self.filename = filename


    def cifFile(self):
        from pymatgen.core import IStructure as pmgIS
        so = pmgIS.from_file(self.filename)
        
        self.atoms = [str(i) for i in so.species]
        self.fcoords = so.frac_coords
        self.lattice_v = so.lattice.matrix
        self.vector_a, self.vector_b, self.vector_c = self.lattice_v[0], self.lattice_v[1], self.lattice_v[2]

        return self.atoms, self.fcoords, self.lattice_v
        

    def vaspFile(self):
        from pymatgen.core import IStructure as pmgIS
        so = pmgIS.from_file(self.filename)
        
        self.atoms = [str(i) for i in so.species]
        self.fcoords = so.frac_coords
        self.lattice_v = so.lattice.matrix
        self.vector_a, self.vector_b, self.vector_c = self.lattice_v[0], self.lattice_v[1], self.lattice_v[2]

        return self.atoms, self.fcoords, self.lattice_v
        
        
    def xsdFile(self):
        atoms = []
        fcoords = []
        lattice_v = []
        
        f = open(self.filename, "r")
        lines = f.readlines()
        f.close()

        p1 = False
        for line in lines:            
            
            if "Atom3d" in line and "XYZ=" in line and "Components"in line:
                tmp = line.split()
                for t in tmp:
                    if "XYZ=" in t:
                        c = t.replace("XYZ=","").replace("\"","")
                        c = c.split(",")
                        fcoords.append(np.array([round(float(c[0]),8),round(float(c[1]),8),round(float(c[2]),8)]))
                    if "Components" in t:
                        a = t.replace("Components=","").replace("\"","").replace("/","").replace(">","")
                        if "%" in a:
                            a = a.split(",")[0]
                        atoms.append(a)
            elif "SpaceGroup" in line and "AVector" in line:
                if "GroupName=\"P1\"" in line:
                    p1 = True
                else:
                    print("Please make symmetry P1. Filename : "+self.filename)
                    quit()
                tmp = line.split()
                for t in tmp:
                    if "AVector=" in t:
                        a = t.replace("AVector=","").replace("\"","")
                        a = a.split(",")
                        lattice_v.append(np.array([round(float(a[0]),8),round(float(a[1]),8),round(float(a[2]),8)]))
                    elif "BVector=" in t:
                        b = t.replace("BVector=","").replace("\"","")
                        b = b.split(",")
                        lattice_v.append(np.array([round(float(b[0]),8),round(float(b[1]),8),round(float(b[2]),8)]))
                    elif "CVector=" in t:
                        c = t.replace("CVector=","").replace("\"","")
                        c = c.split(",")
                        lattice_v.append(np.array([round(float(c[0]),8),round(float(c[1]),8),round(float(c[2]),8)]))
                        

        fcoords = np.array(fcoords)
        lattice_v = np.array(lattice_v)

        self.atoms = atoms
        self.fcoords = fcoords
        self.lattice_v = lattice_v
        self.vector_a, self.vector_b, self.vector_c = lattice_v[0], lattice_v[1], lattice_v[2]
        self.lattice = self.latticeGen()

        return self.atoms, self.fcoords, self.lattice_v

    def latticeGen(self, vector_a=None, vector_b=None, vector_c=None):
        vector_a, vector_b, vector_c = self.vector_a, self.vector_b, self.vector_c
        
        ab = np.dot(vector_a, vector_b)
        bc = np.dot(vector_b, vector_c)
        ac = np.dot(vector_a, vector_c)

        aa = np.linalg.norm(vector_a)
        bb = np.linalg.norm(vector_b)
        cc = np.linalg.norm(vector_c)

        alpha = degrees(acos(bc/(bb*cc)))
        beta  = degrees(acos(ac/(aa*cc)))
        gamma = degrees(acos(ab/(aa*bb)))

        self.lattice = [aa, bb, cc, alpha, beta, gamma]

        return [aa, bb, cc, alpha, beta, gamma]


    def cifWrite(self, atoms=None, lattice=None, fcoords=None, filename=None):
        """

        :param atoms: list of atoms
        :param lattice: list of cell parameters [a, b, c, alpha, beta, gamma]
        :param fcoords:  list of fractional coordinates of atoms
        :param filename: filename of cif
        :return: no return, write cif file
        """
        if not atoms:
            atoms = self.atoms
        if not lattice:
            lattice = self.lattice
        if not fcoords:
            fcoords = self.fcoords
        filename = filename

        f = open(filename, "w")
        f.write("data_global\n")
        f.write("_cell_length_a "+str(lattice[0])+"\n")
        f.write("_cell_length_b "+str(lattice[1])+"\n")
        f.write("_cell_length_c "+str(lattice[2])+"\n")
        f.write("_cell_angle_alpha "+str(lattice[3])+"\n")
        f.write("_cell_angle_beta "+str(lattice[4])+"\n")
        f.write("_cell_angle_gamma "+str(lattice[5])+"\n")
        f.write("_symmetry_space_group_name_H-M 'P -1'\n"+
                "loop_\n"+
                "_symmetry_equiv_pos_as_xyz\n"+
                "  'x,y,z'\n"+
                "loop_\n"+
                "_atom_site_label\n"+
                "_atom_site_fract_x\n"+
                "_atom_site_fract_y\n"+
                "_atom_site_fract_z\n")
        for i in range(len(atoms)):
            f.write(str(atoms[i])+"    "+str(fcoords[i][0]).rjust(12)+str(fcoords[i][1]).rjust(12)+str(fcoords[i][2]).rjust(12)+"\n")

        f.close()



            
def latticeGen(vector_a, vector_b, vector_c):
    ab = np.dot(vector_a, vector_b)
    bc = np.dot(vector_b, vector_c)
    ac = np.dot(vector_a, vector_c)

    aa = np.linalg.norm(vector_a)
    bb = np.linalg.norm(vector_b)
    cc = np.linalg.norm(vector_c)

    alpha = degrees(acos(bc/(bb*cc)))
    beta  = degrees(acos(ac/(aa*cc)))
    gamma = degrees(acos(ab/(aa*bb)))

    lattice = {'length':[aa, bb, cc],
               'angle':[alpha, beta, gamma]}

    return lattice
        
