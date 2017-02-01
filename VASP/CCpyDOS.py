#!/usr/local/bin/python2.7

import os,sys
import subprocess
import matplotlib.pyplot as plt

from pymatgen.io.vasp.outputs import Vasprun
# from pymatgen.io.vasp.outputs import BSVasprun
from pymatgen_plotter import DosPlotter
# from pymatgen_plotter import BSPlotterProjected


class CMSDOS():
    def __init__(self):
        print("Parsing vasprun.xml... please wait...")
        run = Vasprun("./Band-DOS/vasprun.xml", parse_dos=True)
        plotter = DosPlotter(zero_at_efermi=False, sigma=0.02)
        
        cdos = run.complete_dos
        
        self.cdos = cdos
        self.plotter = plotter

    def spd_dos(self, minx=None, maxx=None, miny=None, maxy=None, with_band=False):
        from pymatgen.electronic_structure.core import OrbitalType
        """
        class OrbitalType(integer)
        s = <OrbitalType.s: 0>
        p = <OrbitalType.p: 1>
        d = <OrbitalType.d: 2>
        f = <OrbitalType.f: 3>
        """ 
        cdos = self.cdos
        plotter = self.plotter

        spd_dos = cdos.get_spd_dos()
        s = OrbitalType(0)
        p = OrbitalType(1)
        d = OrbitalType(2)

        plotter.add_dos_dict({d:spd_dos[d]})
        plotter.add_dos_dict({p:spd_dos[p]})
        plotter.add_dos_dict({s:spd_dos[s]})
        plt = plotter.get_plot()

        plt.tick_params(labelsize=15)
        plt.legend(fancybox=True,shadow=True,prop={'size':18})
        plt.xlim(minx,maxx)
        plt.ylim(miny,maxy)

        return plt


    def elt_dos(self, minx=None, maxx=None, miny=None, maxy=None, with_band=False):
        from pymatgen.electronic_structure.core import OrbitalType
        
        cdos = self.cdos
        plotter = self.plotter

        elt_dos = cdos.get_element_dos()

        plotter.add_dos_dict(elt_dos)

        plt = plotter.get_plot()
        plt.legend(fancybox=True,shadow=True,prop={'size':18})
        plt.xlim(minx,maxx)
        plt.ylim(miny,maxy)

        return plt
    

    def elt_spd_dos(self, minx=None, maxx=None, miny=None, maxy=None, with_band=False):
        from pymatgen.electronic_structure.core import OrbitalType
        
        cdos = self.cdos
        plotter = self.plotter
        structure = cdos.structure
        elements = []
        for el in structure.species:
            if el not in elements:
                elements.append(el)
        for i in range(len(elements)):
            print(str(i).ljust(2)+": "+str(elements[i]))
        index = input("Select single element (as integer) : ")

        elt_spd_dos = cdos.get_element_spd_dos(elements[index])
        s = OrbitalType(0)
        p = OrbitalType(1)
        d = OrbitalType(2)

        plotter.add_dos("D",elt_spd_dos[d])
        plotter.add_dos("P",elt_spd_dos[p])
        plotter.add_dos("S",elt_spd_dos[s])

        plt = plotter.get_plot()
        plt.legend(fancybox=True,shadow=True,prop={'size':18})
        plt.xlim(minx,maxx)
        plt.ylim(miny,maxy)

        return plt

    def site_dos(self, minx=None, maxx=None, miny=None, maxy=None, with_band=False):
        cdos = self.cdos
        plotter = self.plotter
        structure = cdos.structure
        print(structure.species)
        index = self.site_choose_tool()

        init = True
        for i in index:
            if init:
                total_dos = cdos.get_site_dos(structure[i])
                init = False
            else:
                site_dos = cdos.get_site_dos(structure[i])
                total_dos = total_dos + site_dos

        plotter.add_dos("Choosed sites",total_dos)
        plt = plotter.get_plot()
        plt.tick_params(labelsize=15)
        plt.legend(fancybox=True,shadow=True,prop={'size':18})
        plt.xlim(minx,maxx)
        plt.ylim(miny,maxy)

        return plt

    def site_spd_dos(self, minx=None, maxx=None, miny=None, maxy=None, with_band=False):
        from pymatgen.electronic_structure.core import OrbitalType
        s = OrbitalType(0)
        p = OrbitalType(1)
        d = OrbitalType(2)
        
        cdos = self.cdos
        plotter = self.plotter
        structure = cdos.structure
        print(structure)
        index = self.site_choose_tool()

        init = True
        for i in index:
            if init:
                total_dos = cdos.get_site_spd_dos(structure[i])
                total_s = total_dos[s]
                total_p = total_dos[p]
                total_d = total_dos[d]
                init = False
            else:
                site_spd_dos = cdos.get_site_spd_dos(structure[i])
                site_s = site_spd_dos[s]
                site_p = site_spd_dos[p]
                site_d = site_spd_dos[d]
                
                total_s = total_s + site_s
                total_p = total_p + site_p
                total_d = total_d + site_d
                
        #total_dos = total_s + total_p + total_d
        
        plotter.add_dos("D",total_d)
        plotter.add_dos("P",total_p)
        plotter.add_dos("S",total_s)
        
        plt = plotter.get_plot()
        plt.tick_params(labelsize=15)
        plt.legend(fancybox=True,shadow=True,prop={'size':18})
        plt.xlim(minx,maxx)
        plt.ylim(miny,maxy)

        return plt

    def site_t2g_eg_dos(self, minx=None, maxx=None, miny=None, maxy=None, with_band=False):
        """
        t2g : dxy, dxz, dyz
        eg  : dx2, dz2
        """
        cdos = self.cdos
        plotter = self.plotter
        structure = cdos.structure
        print(structure)
        index = self.site_choose_tool()

        init = True
        for i in index:
            if init:
                total_dos = cdos.get_site_t2g_eg_resolved_dos(structure[i])
                total_t2g = total_dos['t2g']
                total_eg = total_dos['e_g']
                init = False
            else:
                site_dos = cdos.get_site_t2g_eg_resolved_dos(structure[i])
                site_t2g = site_dos['t2g']
                site_eg = site_dos['e_g']
                
                total_t2g = total_t2g + site_t2g
                total_eg = total_eg + site_eg
        
        plotter.add_dos("e_g",total_eg)
        plotter.add_dos("t2g",total_t2g)
        
        plt = plotter.get_plot()
        plt.tick_params(labelsize=15)
        plt.legend(fancybox=True,shadow=True,prop={'size':18})
        plt.xlim(minx,maxx)
        plt.ylim(miny,maxy)

        return plt


    def partial_p_dos(self, minx=None, maxx=None, miny=None, maxy=None, with_band=False):
        """
        Make 'def get_site_partial_p_dos' in dos.py
        Origin from 'def get_site_t2g_eg_resolved_dos'
        """

        cdos = self.cdos
        plotter = self.plotter
        structure = cdos.structure
        print(structure)
        index = self.site_choose_tool()
        
        init = True
        for i in index:
            if init:
                total_dos = cdos.get_site_partial_p_dos(structure[i])
                total_px = total_dos['px']
                total_py = total_dos['py']
                total_pz = total_dos['pz']
                init = False
            else:
                site_dos = cdos.get_site_partial_p_dos(structure[i])
                site_px = site_dos['px']
                site_py = site_dos['py']
                site_pz = site_dos['pz']
                
                total_px = total_px + site_px
                total_py = total_py + site_py
                total_pz = total_pz + site_pz
        
        plotter.add_dos("px",total_px)
        plotter.add_dos("py",total_py)
        plotter.add_dos("pz",total_pz)

        plt = plotter.get_plot()        
        plt.tick_params(labelsize=15)
        plt.legend(fancybox=True,shadow=True,prop={'size':18})
        plt.xlim(minx,maxx)
        plt.ylim(miny,maxy)

        return plt
    

    def partial_d_dos(self, minx=None, maxx=None, miny=None, maxy=None, with_band=False):
        """
        Make 'def get_site_partial_d_dos' in dos.py
        Origin from 'def get_site_t2g_eg_resolved_dos'
        """

        cdos = self.cdos
        plotter = self.plotter
        structure = cdos.structure
        print(structure)
        index = self.site_choose_tool()
        
        init = True
        for i in index:
            if init:
                total_dos = cdos.get_site_partial_d_dos(structure[i])
                total_dxy = total_dos['dxy']
                total_dyz = total_dos['dyz']
                total_dxz = total_dos['dxz']
                total_dx2 = total_dos['dx2']
                total_dz2 = total_dos['dz2']
                init = False
            else:
                site_dos = cdos.get_site_partial_d_dos(structure[i])
                site_dxy = site_dos['dxy']
                site_dyz = site_dos['dyz']
                site_dxz = site_dos['dxz']
                site_dx2 = site_dos['dx2']
                site_dz2 = site_dos['dz2']
                
                total_dxy = total_dxy + site_dxy
                total_dyz = total_dyz + site_dyz
                total_dxz = total_dxz + site_dxz
                total_dx2 = total_dx2 + site_dx2
                total_dz2 = total_dz2 + site_dz2
        
        plotter.add_dos("dxy",total_dxy)
        plotter.add_dos("dyz",total_dyz)
        plotter.add_dos("dxz",total_dxz)
        plotter.add_dos("dx2",total_dx2)
        plotter.add_dos("dz2",total_dz2)

        plt = plotter.get_plot()
        plt.tick_params(labelsize=15)
        plt.legend(fancybox=True,shadow=True,prop={'size':18})
        plt.xlim(minx,maxx)
        plt.ylim(miny,maxy)

        return plt
                

    def site_choose_tool(self):
        get_num = raw_input("""Choose sites ex) 1-3,5-10,11,12,13
: """
                            )
        try:
            index = []
            get_num = get_num.split(",")  # 1-4,6-10,11,12                
            for i in get_num:
                if "-" in i:
                    r = i.split("-")
                    for j in range(int(r[0]),int(r[1])+1):
                        index.append(j)
                else:                    
                    i = int(i)
                    index.append(i)
        except:
            print("Unvalid input type.")
            print("ex : 1-3,5-10,11,12,13")
            quit()

        return index
        

if __name__=="__main__":
    try:
        sys.argv[1]
    except:
        print("""-------------------------------------
Usage : cms_dos [option] [miny] [maxy] [sub_option1] [sub_option2]...
[option]
1  : spd DOS
2  : element DOS
3  : spd DOS of element
4  : sites DOS
5  : spd DOS of sites
6  : t2g & eg DOS of sites
7  : partial p DOS (px, py, pz)
8  : partial d DOS (dxy, dyz, dxz, dz2, dx2)
[sub_options]
n  : Do not show figure, just save images (ex : cms_dos 1 -3 3 n)
-d : Set y-axis limitation (ex : cms_dos 3 -3 3 -d0,5)
-------------------------------------"""
              )
        quit()
    try:
        lims = [float(sys.argv[2]), float(sys.argv[3])]        
        minx = min(lims)
        maxx = max(lims)
    except:        
        get_lim = raw_input("** Set x-limitaion (ex: -5,5) : ")
        lims = get_lim.split(",")
        minx = float(min(lims))
        maxx = float(max(lims))
    ylim = [argv for argv in sys.argv if "-d" in argv]
    if len(ylim) > 0:
        ylim = ylim[0]
        ylim = ylim.replace("-d","")
        ylim = ylim.split(",")
        ylim = [float(ylim[0]), float(ylim[1])]
        miny = min(ylim)
        maxy = max(ylim)
    else:
        miny = None
        maxy = None

    dirname = os.getcwd()
    dirname = dirname.split("/")[-1]
    
    if sys.argv[1] == "1":
        fig = plt.figure(figsize=(10,6))        
        cms_dos = CMSDOS()
        plt = cms_dos.spd_dos(minx=minx,maxx=maxx,miny=miny,maxy=maxy)
        plt.tight_layout()
        plt.savefig(dirname+"_spd_DOS.png")
      
        if "n" not in sys.argv:
            plt.show()

    elif sys.argv[1] == "2":
        fig = plt.figure(figsize=(10,6))
        cms_dos = CMSDOS()
        plt = cms_dos.elt_dos(minx=minx,maxx=maxx,miny=miny,maxy=maxy)
        plt.tight_layout()
        plt.savefig(dirname+"_element_DOS.png")

        if "n" not in sys.argv:
            plt.show()

    elif sys.argv[1] == "3":
        fig = plt.figure(figsize=(10,6))
        cms_dos = CMSDOS()
        plt = cms_dos.elt_spd_dos(minx=minx,maxx=maxx,miny=miny,maxy=maxy)
        plt.tight_layout()
        plt.savefig(dirname+"_spd_element_DOS.png")

        if "n" not in sys.argv:
            plt.show()

    elif sys.argv[1] == "4":
        fig = plt.figure(figsize=(10,6))
        cms_dos = CMSDOS()
        plt = cms_dos.site_dos(minx=minx,maxx=maxx,miny=miny,maxy=maxy)
        plt.tight_layout()
        plt.savefig(dirname+"_site_DOS.png")
      
        if "n" not in sys.argv:
            plt.show()

    elif sys.argv[1] == "5":
        fig = plt.figure(figsize=(10,6))
        cms_dos = CMSDOS()
        plt = cms_dos.site_spd_dos(minx=minx,maxx=maxx,miny=miny,maxy=maxy)
        plt.tight_layout()
        plt.savefig(dirname+"_site_spd_DOS.png")
      
        if "n" not in sys.argv:
            plt.show()

    elif sys.argv[1] == "6":
        fig = plt.figure(figsize=(10,6))
        cms_dos = CMSDOS()
        plt = cms_dos.site_t2g_eg_dos(minx=minx,maxx=maxx,miny=miny,maxy=maxy)
        plt.tight_layout()
        plt.savefig(dirname+"_site_t2geg_DOS.png")
      
        if "n" not in sys.argv:
            plt.show()

    elif sys.argv[1] == "7":
        fig = plt.figure(figsize=(10,6))
        cms_dos = CMSDOS()
        plt = cms_dos.partial_p_dos(minx=minx,maxx=maxx,miny=miny,maxy=maxy)
        plt.tight_layout()
        plt.savefig(dirname+"_site_partial_p_DOS.png")
      
        if "n" not in sys.argv:
            plt.show()

    elif sys.argv[1] == "8":
        fig = plt.figure(figsize=(10,6))
        cms_dos = CMSDOS()
        plt = cms_dos.partial_d_dos(minx=minx,maxx=maxx,miny=miny,maxy=maxy)
        plt.tight_layout()
        plt.savefig(dirname+"_site_partial_d_DOS.png")
      
        if "n" not in sys.argv:
            plt.show()


