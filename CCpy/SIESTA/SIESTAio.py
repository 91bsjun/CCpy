import os, sys
import re
import numpy as np
from collections import OrderedDict

from pymatgen.core.structure import Structure
from pymatgen.core.lattice import Lattice
from pymatgen.core.periodic_table import Element


class SIESTAInput():
    def __init__(self, structure, sys_name, sys_label=None):
        self.input_structure = structure
        self.name = sys_name
        if not sys_label:
            sys_label = sys_name
        self.sys_label = sys_label

    def system_part(self):
        return OrderedDict({'SystemName': self.name, 'SystemLabel': self.sys_label})

    def structure_part(self, kpt_criteria=20, constrain_sites=None, in_kpt=None):
        """

        """
        st = self.input_structure
        options = OrderedDict({})
        blk_options = OrderedDict({})

        sites = st.sites
        types_of_specie = st.types_of_specie

        # -- NumberOfAtoms, NumberOfSpecies
        options['NumberOfAtoms'] = len(sites)
        options['NumberOfSpecies'] = len(types_of_specie)

        # -- Chemical_Species_Label
        specie_label = {}
        blk_options['Chemical_Species_Label'] = []
        for i, elt in enumerate(types_of_specie):
            elt_str = str(elt)
            elt_num = elt.number
            specie_label[elt_str] = [i + 1, elt_num]  # ex) specie_label['O'] = [1, 8]
            blk_options['Chemical_Species_Label'].append(
                '%s%s%s' % (rj(str(i + 1), 2), rj(str(elt_num), 4), rj(elt_str, 4)))

        # -- LatticeConstant
        options['LatticeConstant'] = '1.00 Ang'

        # -- LatticeParameters, LatticeVectors
        lat = st.lattice
        blk_options['LatticeParameters'] = ['%f ' * 6 % (lat.a, lat.b, lat.c, lat.alpha, lat.beta, lat.gamma)]
        '''
        blk_options['LatticeVectors'] = []
        for v in lat.matrix:
            blk_options['LatticeVectors'].append('%.8f %.8f %.8f' % (v[0], v[1], v[2]))
        '''

        # -- AtomicCoordinatesFormat, AtomicCoorFormatOut
        options['AtomicCoordinatesFormat'] = 'Fractional'
        options['AtomicCoorFormatOut'] = 'Fractional'

        # -- AtomicCoordinatesAndAtomicSpecies
        blk_options['AtomicCoordinatesAndAtomicSpecies'] = []
        for s in sites:
            val = '%.10f %.10f %.10f %s%s' % (
            s.frac_coords[0], s.frac_coords[1], s.frac_coords[2], rj(str(specie_label[str(s.specie)][0]), 2),
            rj(str(s.specie), 4))
            blk_options['AtomicCoordinatesAndAtomicSpecies'].append(val)

        """
        To do
        if constrain_sites:
            blk_options['GeometryConstraints'] = []
        """

        # -- kgrid_Monkhorst_Pack
        if in_kpt:
            kpts = in_kpt
        else:
            kpts = []
            for v in lat.abc:
                if v >= kpt_criteria:
                    kpts.append(1)
                else:
                    kpts.append(int(round(kpt_criteria / v, 0)))
        kpt_a = '%s%s%s  0.0' % (rj(str(kpts[0]), 2), rj('0', 4), rj('0', 4))
        kpt_b = '%s%s%s  0.0' % (rj('0', 2), rj(str(kpts[1]), 4), rj('0', 4))
        kpt_c = '%s%s%s  0.0' % (rj('0', 2), rj('0', 4), rj(str(kpts[1]), 4))
        blk_options['kgrid_Monkhorst_Pack'] = [kpt_a, kpt_b, kpt_c]

        self.structure_options = options
        self.structure_blk_options = blk_options

        return options, blk_options

    def calc_option_part(self):
        options = {'SolutionMethod': 'diagon', 'ElectronicTemperature': '300 K',
                   'XC.functional': 'GGA', 'XC.authors': 'PBE', 'PAO.BasisSize': 'SZ',
                   'SpinPolarized': '.false.', 'MeshCutoff': '300. Ry',
                   'MinSCFIterations': 10, 'MaxSCFIterations': 100,
                   'DM.Tolerance': '2.d-3', 'DM.NumberPulay': 8, 'DM.MixingWeight': 0.10}
        options = OrderedDict(options)
        if spin:
            options['SpinPolarized'] = '.true.'
        self.calc_options = options

        return options

    def relax_option_part(self):
        """
        force_tol (eV/Ang)
        """
        options = {'MD.TypeOfRun': 'CG', 'MD.MaxForceTol': '0.05 eV/Ang', 'MD.NumCGsteps': 500,
                   'MD.VariableCell': '.true.'}
        options = OrderedDict(options)
        self.relax_options = options

        return options

    def md_option_part(self, run_type, init_T, final_T, max_time_step, timestep=2):
        """
        timestep (fs)
        """
        init_T = '%s K' % str(init_T)
        final_T = '%s K' % str(final_T)
        timestep = '%s fs' % str(timestep)
        options = {'MD.UseSaveXV': '.true.', 'MD.TypeOfRun': run_type,
                   'MD.InitialTemperature': init_T, 'MD.TargetTemperature': final_T,
                   'MD.Initial.Time.Step': 1, 'MD.Final.Time.Step': max_time_step, 'MD.Length.Time.Step': timestep}
        options = OrderedDict(options)
        self.md_options = options

        return options

    def output_option_part(self):
        options = {'WriteCoorInitial': '.true.', 'WriteCoorStep': '.true.', 'WriteCoorXmol': '.true.',
                   'WriteForces': '.true.', 'WriteKpoints': '.false.', 'WriteEigenvalues': '.false.',
                   'WriteKbands': '.false.', 'WriteBands': '.true.', 'WriteMullikenPop': '1',
                   'WriteMDCoorXmol': '.true.', 'WriteMDXmol': '.true.', 'WriteMDhistory': '.true.',
                   'WriteXML': '.false.', 'WriteWaveFunctions': '.true.'}
        options = OrderedDict(options)
        self.output_options = options

        return options

    def saving_reading_option_part(self):
        options = {'DM.UseSaveDM': '.true.', 'MD.UseSaveCG': '.true.', 'SaveRho': '.false.',
                   'SaveDeltaRho': '.false.', 'SaveElectrostaticPotential': '.false.',
                   'WriteSiestaDim': '.false.', 'WriteDenchar': '.true.'}
        options = OrderedDict(options)
        self.saving_reading_options = options

        return options

    def potential_maker(self, potential_dirpath):
        types_of_specie = self.types_of_specie
        for elt in types_of_specie:
            psf = str(elt) + ".psf"
            full_path = potential_dirpath + "/" + psf
            if os.path.isfile(full_path):
                os.system("cp %s ./" % full_path)
            else:
                print("Cannot find", full_path)

class SIESTARelaxset():
    def __init__(self, structure, sys_name, sys_label=None, user_calc_option=None, in_kpt=None):
        self.input_structure = structure
        self.name = sys_name
        if not sys_label:
            sys_label = sys_name
        self.sys_label = sys_label

        si = SIESTAInput(structure, sys_name, sys_label)

        system_option = si.system_part()
        structure_option, blk_structure_option = si.structure_part(in_kpt=in_kpt)
        system_option.update(structure_option)

        calc_option = si.calc_option_part()
        relax_option = si.relax_option_part()
        calc_option.update(relax_option)

        output_option = si.output_option_part()
        calc_option.update(output_option)

        sr_option = si.saving_reading_option_part()
        calc_option.update(sr_option)

        if user_calc_option:
            calc_option.update(user_calc_option)

        self.si = si
        self.system_option = system_option
        self.blk_structure_option = blk_structure_option
        self.calc_option = calc_option

    def update_option(self, option_dict):
        self.calc_option.update(OrderedDict(option_dict))

    def write_input(self, filename, potential_dirpath):
        f = open(filename, 'w')
        f.close()
        input_writer(filename, self.system_option)
        blk_input_writer(filename, self.blk_structure_option)
        input_writer(filename, self.calc_option)
        self.si.potential_maker(potential_dirpath)


class SIESTAMDset():
    def __init__(self, structure, sys_name, run_type, init_T, final_T,
                 max_time_step, timestep=2, sys_label=None, user_calc_option=None, in_kpt=None):
        self.name = sys_name
        if not sys_label:
            sys_label = sys_name
        self.sys_label = sys_label

        si = SIESTAInput(structure, sys_name, sys_label)

        system_option = si.system_part()
        structure_option, blk_structure_option = si.structure_part(in_kpt=in_kpt)
        system_option.update(structure_option)

        calc_option = si.calc_option_part()
        md_option = si.md_option_part(run_type, init_T, final_T, max_time_step, timestep=timestep)
        calc_option.update(md_option)

        output_option = si.output_option_part()
        calc_option.update(output_option)

        sr_option = si.saving_reading_option_part()
        calc_option.update(sr_option)

        if user_calc_option:
            calc_option.update(user_calc_option)

        self.si = si
        self.system_option = system_option
        self.blk_structure_option = blk_structure_option
        self.calc_option = calc_option

    def update_option(self, option_dict):
        self.calc_option.update(OrderedDict(option_dict))

    def write_input(self, filename, potential_dirpath):
        f = open(filename, 'w')
        f.close()
        input_writer(filename, self.system_option)
        blk_input_writer(filename, self.blk_structure_option)
        input_writer(filename, self.calc_option)
        self.si.potential_maker(potential_dirpath)


class SIESTAOutput():
    def __init__(self, output_filename="siesta.out"):
        out = open(output_filename, "r").read()
        self.out = out
        self.out_lines = out.split("\n")

        # find number of atoms
        natoms_patt = re.compile("NumberOfAtoms\s+(\d+)")
        natoms = natoms_patt.findall(out)
        natoms = int(natoms[0])
        self.natoms = natoms

        # find coordinates type: 'fractional' or 'Ang'
        coord_type_patt = re.compile("outcoor: Atomic coordinates \((\D+)\)", re.M)
        coord_type = coord_type_patt.findall(out)
        coord_type = coord_type[0]

        coords_are_cartesian = False
        if coord_type == 'Ang':
            coords_are_cartesian = True
        self.coords_are_cartesian = coords_are_cartesian

        # find iteration length
        iteration_patt = re.compile("Begin CG move")
        iteration_len = len(iteration_patt.findall(out))
        self.iteration_len = iteration_len

    def structure_parser(self):
        lines = self.out_lines
        iteration_len = self.iteration_len

        all_lat_matrice = []
        all_species = []
        all_coords = []
        for i, l in enumerate(lines):
            if 'outcell: Unit cell vectors (Ang):' in l:
                j = i + 1
                lat_matrix = [np.array(lines[k].split(), dtype='float') for k in range(j, j + 3)]
                all_lat_matrice.append(Lattice(lat_matrix))
            if 'outcoor: Atomic coordinates' in l:
                j = i + 1
                coords_lines = lines[j:j + self.natoms]
                species = []
                coords = []
                for cl in coords_lines:
                    cl = cl.split()
                    species.append(cl[-1])
                    coords.append(np.array(cl[:3], dtype='float'))
                all_species.append(species)
                all_coords.append(coords)

        structures = []
        for i in range(iteration_len):
            structure = Structure(all_lat_matrice[i], all_species[i], all_coords[i],
                                  coords_are_cartesian=self.coords_are_cartesian)
            structures.append(structure)

        self.structures = structures
        self.initial_structure = structures[0]
        self.final_structure = structures[-1]

        return self.structures

    def energy_parser(self):
        out = self.out
        e_ks_patt = re.compile("siesta: E_KS\(eV\) =\s+([-]?\d+[.]\d+)")
        ks_energies = e_ks_patt.findall(out)

        self.energies = ks_energies
        self.initial_energy = ks_energies[0]
        self.final_energy = ks_energies[-1]

        return self.energies

    def save_final_structure(self, filename):
        final_structure = self.final_structure
        final_structure.to(filename=filename)


def input_writer(filename, options, header=None):
    f = open(filename, 'a')
    if header:
        f.write(header_printer(header) + "\n")
    for key in options.keys():
        line = "%s %s\n" % (lj(key, 30), options[key])
        f.write(line)
    f.close()

def blk_input_writer(filename, options, header=None):
    f = open(filename, 'a')
    if header:
        f.write(header_printer(header) + "\n")
    for key in options.keys():
        f.write("\n%%block %s\n" % key)
        for v in options[key]:
            f.write(v + "\n")
        f.write("%%endblock %s\n" % key)
    f.close()

def header_printer(msg, total_len=30):
    dec = total_len - len(msg)
    dec = int(dec / 2)
    dec1 = "\n# " + "-" * (dec) + " "
    if len(msg) % 2 == 1:
        dec += 1
    dec2 = " " + "-" * (dec) + " #"

    return dec1 + msg + dec2

def rj(msg, n):
    return msg.rjust(n)

def lj(msg, n):
    return msg.ljust(n)

