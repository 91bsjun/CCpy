#!/usr/local/bin/python2.7
# This script is to add MAGMOM and LDA+U parameters in each INCAR file.
import os, sys

# -- MAGMOM, LDA+U parameters
# holee
# mag = {"Ni":4.5, "Mn":4.5, "Co":4.5, "Li":0.0, "O":0.0}
# LDAUL = {"Ni":2, "Mn":2, "Co":2, "Li":2, "O":2}
# LDAUU = {"Ni":7.37, "Mn":5.84, "Co":6.14, "Li":0, "O":0}
# LDAUJ = {"Ni":1, "Mn":1, "Co":1, "Li":0, "O":0}

# pymatgen
mag = {'Mn3+': 4, 'Ni4+': 0.6, 'Cr': 5, 'Mn4+': 3, 'Ta': 5, 'Ni3+': 1, 'Mo': 5,
       'Ni': 2, 'V': 5, 'Mn2+': 5, 'Co': 5, 'Co4+': 1, 'W': 5, 'Fe3+': 5, 'Fe2+': 4,
       'Mn': 5, 'Fe4+': 4, 'Fe': 5, 'Co3+': 0.6,
       'Li': 0, 'O': 0}
LDAUL = {'Mo': 2, 'V': 2, 'Cu': 2, 'W': 2, 'Ag': 2, 'Cr': 2, 'Ta': 2,
         'Nb': 2, 'Mn': 2, 'Re': 2, 'Co': 2, 'Ni': 2, 'Fe': 2,
         'Li': 0, 'O': 0}
LDAUU = {'Mo': 4.38, 'V': 3.1, 'Cu': 4, 'W': 4.0, 'Ag': 1.5, 'Cr': 3.5, 'Ta': 2,
         'Nb': 1.5, 'Mn': 3.9, 'Re': 2, 'Co': 3.4, 'Ni': 6, 'Fe': 4.0,
         'Li': 0, 'O': 0}
LDAUJ = {'Mo': 0, 'V': 0, 'Cu': 0, 'W': 0, 'Ag': 0, 'Cr': 0, 'Ta': 0,
         'Nb': 0, 'Mn': 0, 'Re': 0, 'Co': 0, 'Ni': 0, 'Fe': 0,
         'Li': 0, 'O': 0}


try:
    dirname = sys.argv[1]
except:
    dirname = raw_input("Directory name?")

os.chdir(dirname)
# -- supcells = [1-1-1, 1-2-1, ...]
supcells = [d for d in os.listdir("./") if os.path.isdir(d)]
for supcell in supcells:
    os.chdir(supcell)
    # -- dirs = [Co3Mn3Ni3O18Vac9, ...]
    dirs = os.listdir("./")
    dirs.sort()
    for d in dirs:
        os.chdir(d)
        sub_ds = os.listdir("./")
        # -- sub_ds = [c0001, ..]
        for sd in sub_ds:
            os.chdir(sd)

            f = open("POSCAR", "r")
            lines = f.readlines()
            f.close()

            for i in range(len(lines)):
                if i == 5:
                    elts = lines[i].split()
                elif i == 6:
                    n_of_atoms = lines[i].split()

            # elts       = [Co, Mn, Ni, O]
            # n_of_atoms = [3, 3, 3, 18]

            mag_string = ""
            for i in range(len(n_of_atoms)):
                mag_string += str(mag[elts[i]]) + "*" + str(n_of_atoms[i]) + " "

            LDAUL_string = ""
            for i in range(len(elts)):
                LDAUL_string += str(LDAUL[elts[i]]) + " "

            LDAUU_string = ""
            for i in range(len(elts)):
                LDAUU_string += str(LDAUU[elts[i]]) + " "

            LDAUJ_string = ""
            for i in range(len(elts)):
                LDAUJ_string += str(LDAUJ[elts[i]]) + " "

            LDAU = """MAGMOM = %s

    # LDA+U parameters
    LDAU = .TRUE.
    LDAUTYPE = 2
    LDAUL = %s
    LDAUU = %s
    LDAUJ = %s"""%(mag_string, LDAUL_string, LDAUU_string, LDAUJ_string)

            f = open("INCAR", "a")
            f.write(LDAU)
            f.close()

            os.chdir("../")
        os.chdir("../")
    os.chdir("../")
