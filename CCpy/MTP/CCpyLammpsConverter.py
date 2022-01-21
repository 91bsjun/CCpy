#!/bin/env python
from ovito.io import import_file, export_file
from pymatgen.core.structure import Structure
from pymatgen.core.trajectory import Trajectory
import warnings
warnings.filterwarnings('ignore', message='.*OVITO.*PyPI')

import os, sys

if len(sys.argv) == 1:
    print("""
[1] cif files to dat
[2] dat files to cif
[3] lammps trajectory to XDATCAR   
    """)
    quit()
type = int(sys.argv[1])
files = sys.argv[2:]

def lmp_poscar_to_cif(file_lines):
    for i, line in enumerate(file_lines):
        if i == 2:
            line = line.replace('\n', '')
            spl = line.split()
            v_x = [float(spl[0]), float(spl[1]), float(spl[2])]
            lattice.append(v_x)
        if i == 3:
            line = line.replace('\n', '')
            spl = line.split()
            v_y = [float(spl[0]), float(spl[1]), float(spl[2])]
            lattice.append(v_y)
        if i == 4:
            line = line.replace('\n', '')
            spl = line.split()
            v_z = [float(spl[0]), float(spl[1]), float(spl[2])]
            lattice.append(v_z)
        if i == 5:
            line = line.replace('\n', '')
            spl = line.split()
            check_type = []
            for s in spl:
                if 'Type_' not in s:
                    species.append(s)
                elif s not in check_type:
                    check_type.append(s)
                    element = input('put in atom %s :' % s)
                    species.append(element)
        if i == 6:
            line = line.replace('\n', '')
            spl = line.split()
            count = 0
            atom_num = 0
            for s in spl:
                num = int(s)
                atom_num += num
                for j in range(num):
                    all_species.append(species[count])
                count += 1
        if i >= 8 and i < (8 + atom_num):
            line = line.replace('\n', '')
            spl = line.split()
            x, y, z = float(spl[0]), float(spl[1]), float(spl[2])
            coords.append([x, y, z])
    return lattice, all_species, coords


if type == 1:
    for file in files:
        dat_file_name = file.replace('.cif', '.dat')
        pipeline = import_file(file)
        if '-atom_style=' in file:
            export_file(pipeline, '%s' % dat_file_name, 'lammps/data', atom_style="charge") ## or atomic
        else:
            export_file(pipeline, '%s' % dat_file_name, 'lammps/data') ## or atomic
        print('%s generate!' % dat_file_name)

elif type == 2:
    lattice = []
    species = []
    all_species = []
    coords = []
    for file in files:
        tmp_file_name = file.replace('.dat', '.tmp')
        pipeline = import_file(file)
        poscar = export_file(pipeline, '%s' % tmp_file_name, 'vasp', reduced=True)
        f = open(tmp_file_name, 'r')
        file_lines = f.readlines()
        f.close()
        lmp_poscar_to_cif(file_lines)
        strs = Structure(lattice, all_species, coords)
        cif_file_name = tmp_file_name.replace('.tmp', '.cif')
        converter = strs.to('.cif', '%s' % cif_file_name)
        print('%s generate!' % cif_file_name)
        os.system('rm -f *.tmp*')
elif type == 3 :
    species = []
    check_type = []
    all_strs = []
    for file in files:
        trj_split = file.split('.')
        trj_split[1] = '.tmp'
        file_names = ""
        for t in trj_split:
            file_names += t
        pipeline = import_file(file)
        for frame_index in range(pipeline.source.num_frames):
            lattice = []
#            species = []
            all_species = []
            coords = []
            poscar = export_file(pipeline, '%s_%s' % (file_names, frame_index), 'vasp', frame=frame_index)
            f = open('%s_%s' %(file_names, frame_index), 'r')
            file_lines = f.readlines()
            f.close()
            for i, line in enumerate(file_lines):
                if i == 2:
                    line = line.replace('\n', '')
                    spl = line.split()
                    v_x = [float(spl[0]), float(spl[1]), float(spl[2])]
                    lattice.append(v_x)
                if i == 3:
                    line = line.replace('\n', '')
                    spl = line.split()
                    v_y = [float(spl[0]), float(spl[1]), float(spl[2])]
                    lattice.append(v_y)
                if i == 4:
                    line = line.replace('\n', '')
                    spl = line.split()
                    v_z = [float(spl[0]), float(spl[1]), float(spl[2])]
                    lattice.append(v_z)
                if i == 5:
                    line = line.replace('\n', '')
                    spl = line.split()
                    for s in spl:
                        if 'Type_' not in s:
                            species.append(s)
                        elif s not in check_type:
                            check_type.append(s)
                            element = input('put in atom %s :' % s)
                            species.append(element)                        
                if i == 6:
                    line = line.replace('\n', '')
                    spl = line.split()
                    count = 0
                    atom_num = 0
                    for s in spl:
                        num = int(s)
                        atom_num += num
                        for j in range(num):
                            all_species.append(species[count])
                        count += 1
                if i >= 8 and i < (8 + atom_num):
                    line = line.replace('\n', '')
                    spl = line.split()
                    x, y, z = float(spl[0]), float(spl[1]), float(spl[2])
                    coords.append([x, y, z])
            strs = Structure(lattice, all_species, coords, coords_are_cartesian=True)
            all_strs.append(strs)
        trj = Trajectory
        trj_read = trj.from_structures(all_strs)
        trj_read.write_Xdatcar(filename='XDATCAR')
        print('Done')
    os.system('rm -f *.tmp*')
            

#            print('%s_%s generate!' %(file_names, frame_index))

