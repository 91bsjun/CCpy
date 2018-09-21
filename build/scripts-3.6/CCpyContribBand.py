#!/home/shared/anaconda3/envs/CCpy_tmp/bin/python

import re, sys
import numpy as np
import matplotlib.pyplot as plt

from pymatgen.core.lattice import Lattice
from pymatgen.core import IStructure
from pymatgen.electronic_structure.core import Spin
from pymatgen.electronic_structure.bandstructure import BandStructureSymmLine

from CCpy.VASP.pymatgen_plotter import BSPlotter

"""
For static calculations, the file PROCAR contains the spd- and site projected character of each band.
"""

version = sys.version
if version[0] == '3':
    raw_input = input


class SiteProjectedBand():
    def __init__(self):
        # some process of get site indice
        pass

    def parsing_procar(self, filename="PROCAR", sites=None, orbital_index=None):
        """
        PROCAR parsing process to get eigenvalues and site contribution

        :return:
             kpoints(dict), eigenvalues(dict), contribution(dict)
        """
        PROCAR = open(filename, "r").read()
        lines = PROCAR.split("\n")

        # -- pre info
        info = lines[1].split()
        n_of_kpoints = int(info[3])
        n_of_bands = int(info[7])
        n_of_ions = int(info[11])
        # print(n_of_ions,n_of_kpoints,n_of_bands)

        # -- find k-point start index
        k_point_start_index = []
        kpoint_patt = re.compile(" k-point\s+\d{1,3}")
        for i in range(len(lines)):
            m = kpoint_patt.search(lines[i])
            if m:
                k_point_start_index.append(i)

        all_kpts = []  # All k-points
        all_eigenvals = []  # Eigenvalues of each bands
        all_eigenvals_conts = []  # Concentration of each eigenvalues
        all_eigenvals_colors = []  # Color of major site group

        all_bands = {}  # tmp dict
        all_conts = {}  # tmp dict
        all_colors = {}  # tmp dict

        # choose orbital index
        orbital_index = self.orbital_choose_tool()

        # -- each k-point
        for ki in range(len(k_point_start_index)):
            try:
                kpt_block_lines = lines[k_point_start_index[ki]:k_point_start_index[ki + 1]]
            except:
                # -- final part
                kpt_block_lines = lines[k_point_start_index[ki]:]

            # -- Add to kpts_dict
            kpts = lines[k_point_start_index[ki]].split()
            kpts = [float(kpts[3]), float(kpts[4]), float(kpts[5])]
            all_kpts.append(np.array(kpts))

            band_start_index = []
            band_patt = re.compile("band\s+\d{1,3}")
            for i in range(len(kpt_block_lines)):
                m = band_patt.search(kpt_block_lines[i])
                if m:
                    band_start_index.append(i)

            band_num = 1  # key of all_bands dict
            for bi in band_start_index:
                # -- find occupancy
                A_occu = 0.0
                B_occu = 0.0
                for i in sites["A"]:
                    for j in orbital_index:
                        ion_line = kpt_block_lines[bi + 2 + i]
                        A_occu += float(ion_line.split()[j])
                for i in sites["B"]:
                    for j in orbital_index:
                        ion_line = kpt_block_lines[bi + 2 + i]
                        B_occu += float(ion_line.split()[j])
                # -- color of major sites group
                if A_occu > B_occu:
                    color = '#DB0000'
                elif A_occu < B_occu:
                    color = '#0042ED'
                else:
                    color = 'black'

                # Use occu / total_occur   or   just occu ?
                # tot_occu = float(kpt_block_lines[bi+2+n_of_ions+1].split()[-1])
                # if tot_occu == 0:
                #     cont = 0
                # elif occu > tot_occu:
                #     cont = 1
                # else:
                #     cont = occu/tot_occu
                cont = 1  # this will be removed

                # -- find energy
                energy = float(kpt_block_lines[bi].split()[4])
                try:
                    all_bands[band_num].append(energy)
                    all_conts[band_num].append(cont)
                    all_colors[band_num].append(color)
                except:
                    all_bands[band_num] = [energy]
                    all_conts[band_num] = [cont]
                    all_colors[band_num] = [color]
                band_num += 1

        # -- rearrange eigenvalues as each band
        for key in all_bands.keys():
            all_eigenvals.append(np.array(all_bands[key]))
            all_eigenvals_conts.append(np.array(all_conts[key]))
            all_eigenvals_colors.append(np.array(all_colors[key]))
        all_eigenvals = np.array(all_eigenvals)
        all_eigenvals_conts = np.array(all_eigenvals_conts)
        all_eigenvals_colors = np.array(all_eigenvals_colors)

        # -- to pymatgen eigenval type and same type of occupancy
        pmg_eigenval = {Spin(1): all_eigenvals}
        pmg_conts = {Spin(1): all_eigenvals_conts}
        pmg_colors = {Spin(1): all_eigenvals_colors}

        return all_kpts, pmg_eigenval, pmg_conts, pmg_colors

    def get_kpoints_label(self, filename="KPOINTS"):
        """
        Find symmetry line labels in KPOINTS file

        :return: label dictionary
        """
        KPOINTS = open(filename, "r").read()
        label_pattern = re.compile("-?\d+.\d+\s+-?\d+.\d+\s+-?\d+.\d+\s+!?.*", re.M)
        m = label_pattern.findall(KPOINTS)
        tmp = [t.replace("!", "") for t in m]
        kpoints_label = {}
        for t in tmp:
            splt = t.split()
            kpoints_label[splt[-1]] = np.array([float(splt[0]), float(splt[1]), float(splt[2])])

        return kpoints_label

    def get_reciprocal_vector(self, filename="POSCAR"):
        """
        Find reciprocal vectors from POSCAR

        :return: reciprocal vector object (pymatgen)
        """

        POSCAR = open(filename, "r").read()
        POSCAR = POSCAR.split("\n")
        a = np.array([float(POSCAR[2].split()[0]),
                      float(POSCAR[2].split()[1]),
                      float(POSCAR[2].split()[2])])
        b = np.array([float(POSCAR[3].split()[0]),
                      float(POSCAR[3].split()[1]),
                      float(POSCAR[3].split()[2])])
        c = np.array([float(POSCAR[4].split()[0]),
                      float(POSCAR[4].split()[1]),
                      float(POSCAR[4].split()[2])])
        lattice = np.array([a, b, c])
        reci_lattice = Lattice(lattice).reciprocal_lattice

        # -- Just to show sites number of ions..
        structure = IStructure.from_file(filename)
        print(structure)
        sites = self.site_choose_tool()

        return reci_lattice, sites

    def get_efermi(self, filename="OUTCAR"):
        """
        Find fermi energy from OUTCAR

        :return: e-fermi
        """
        OUTCAR = open(filename, "r").read()
        fermi_pattern = re.compile("E-fermi\s*:\s*(\S+)", re.M)
        m = fermi_pattern.search(OUTCAR)
        result = m.group()
        efermi = float(result.split()[2])

        return efermi

    def get_band_structure(self, kpoints_filename="KPOINTS", procar_filename="PROCAR",
                           poscar_filename="POSCAR", outcar_filename="OUTCAR"):
        """

        :return: pymatgen bands structure object
        """

        lattice, sites = self.get_reciprocal_vector(filename=poscar_filename)
        kpoints, eigenvals, occupancy, colors = self.parsing_procar(filename=procar_filename, sites=sites)
        efermi = self.get_efermi(filename=outcar_filename)
        labels_dict = self.get_kpoints_label(filename=kpoints_filename)

        bands = BandStructureSymmLine(kpoints, eigenvals, lattice, efermi, labels_dict)

        return bands, occupancy, sites, colors

    def site_choose_tool(self):
        # -- Get A sites
        get_num = raw_input("""Choose sites of Group A. ex) 1-3,5-10,11,12,13
: """
                            )
        try:
            A_sites = []
            get_num = get_num.split(",")  # 1-4,6-10,11,12
            for i in get_num:
                if "-" in i:
                    r = i.split("-")
                    for j in range(int(r[0]), int(r[1]) + 1):
                        A_sites.append(j + 1)
                else:
                    i = int(i)
                    A_sites.append(i + 1)
        except:
            print("Unvalid input type.")
            print("ex : 1-3,5-10,11,12,13")
            quit()

        # -- Get B sites
        get_num = raw_input("""Choose sites of Group B. ex) 1-3,5-10,11,12,13
: """
                            )
        try:
            B_sites = []
            get_num = get_num.split(",")  # 1-4,6-10,11,12
            for i in get_num:
                if "-" in i:
                    r = i.split("-")
                    for j in range(int(r[0]), int(r[1]) + 1):
                        B_sites.append(j + 1)
                else:
                    i = int(i)
                    B_sites.append(i + 1)
        except:
            print("Unvalid input type.")
            print("ex : 1-3,5-10,11,12,13")
            quit()

        sites = {"A": A_sites, "B": B_sites}

        return sites

    def orbital_choose_tool(self):
        print("""
--------------
Orbital index
--------------
1  : s
2  : py
3  : pz
4  : px
5  : dxy
6  : dyz
7  : dz2
8  : dxz
9  : dx2
10 : total""")
        get_num = raw_input("""Choose orbitals ex) 1,2,5-9
: """
                            )
        try:
            orbital_index = []
            get_num = get_num.split(",")  # 1-4,6-10,11,12
            for i in get_num:
                if "-" in i:
                    r = i.split("-")
                    for j in range(int(r[0]), int(r[1]) + 1):
                        orbital_index.append(j)
                else:
                    i = int(i)
                    orbital_index.append(i)
        except:
            print("Unvalid input type.")
            print("ex : 1-3,5-10,11,12,13")
            quit()
        if 10 in orbital_index and len(orbital_index) != 1:
            print("Can not select total and other orbitals.")
            quit()

        return orbital_index


if __name__ == "__main__":
    cls_site_band = SiteProjectedBand()
    bands, occupancy, sites, colors = cls_site_band.get_band_structure(kpoints_filename="./Band-DOS/KPOINTS",
                                                                       procar_filename="./Band-DOS/PROCAR",
                                                                       poscar_filename="./Band-DOS/POSCAR",
                                                                       outcar_filename="./Band-DOS/OUTCAR")
    try:
        lims = [float(sys.argv[1]), float(sys.argv[2])]
        miny = min(lims)
        maxy = max(lims)
    except:
        get_lim = raw_input("** Set y-limitaion (ex: -5,5) : ")
        lims = get_lim.split(",")
        miny = float(min(lims))
        maxy = float(max(lims))

    fig = plt.figure(figsize=(6, 10))
    plotter = BSPlotter(bands)
    plotter.get_contrib_projected_plot(zero_to_efermi=True, line_width=3, occupancy=occupancy, colors=colors)
    plt.axhline(y=0, lw=1, ls=':', color='gray')
    plt.tick_params(labelsize=15)
    plt.ylim(miny, maxy)
    plt.tight_layout()
    figname = raw_input("Saving figure.. File name ? (ex: TiO2_site.png) \n: ")
    plt.savefig(figname, dpi=200)
    plt.show()
