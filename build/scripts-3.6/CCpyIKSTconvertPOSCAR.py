#!/home/shared/anaconda3/envs/CCpy_tmp/bin/python
# This module is to convert VASP POSCAR file to IKST's DBmaker POSCAR templates

def POStoPOS():
    f = open("POSCAR", "r")
    lines = f.readlines()
    f.close()

    vectors = ""
    wyckoff = ""
    coords = ""
    for i in range(len(lines)):
        if 2 <= i <= 4:
            vectors += lines[i]
        elif 5 <= i <= 6:
            wyckoff += lines[i]
        elif i >= 8 :
            coords += lines[i]

    full_string = """# Lattice vectors
%s
# List of Wyckoff notations and multiplicity
%s

# Coordinates - last flag denotes id to group sites (could be due to symmetry or otherwise)
%s"""%(vectors, wyckoff, coords)
    f = open("POSCAR", "w")
    f.write(full_string)
    f.close()
    print("POSCAR")

if __name__ == '__main__':
  POStoPOS()
