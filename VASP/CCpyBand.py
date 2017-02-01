#!/usr/local/bin/python2.7

import os, sys
import subprocess

import matplotlib.pyplot as plt

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.io.vasp.outputs import BSVasprun
# from pymatgen.electronic_structure.plotter import BSPlotter
# from pymatgen.electronic_structure.plotter import BSPlotterProjected
# from pymatgen.electronic_structure.plotter import DosPlotter
from pymatgen_plotter import BSPlotter
from pymatgen_plotter import BSPlotterProjected
from pymatgen_plotter import DosPlotter

import pickle


class CMSBand():
    def __init__(self, elt_projected=False, dos=False):
        # -- plot with DOS : Full VASP run
        if dos:
            if elt_projected:
                try:
                    run = load_pickle_data(name="vasprun.dos.elt.pkl")
                    bands = load_pickle_data(name="bands.elt.pkl")
                except:
                    print("Failed")
                    print("Parsing vasprun.xml... please wait...")
                    run = Vasprun("./Band-DOS/vasprun.xml", parse_dos=True, parse_projected_eigen=True)
                    bands = run.get_band_structure("./Band-DOS/KPOINTS")
                    print("Done!")
                    save_pickle_data(name="vasprun.dos.elt.pkl",obj=run)
                    save_pickle_data(name="bands.elt.pkl",obj=bands)
                cdos = run.complete_dos
            else:
                try:
                    run = load_pickle_data(name="vasprun.dos.pkl")
                    bands = load_pickle_data(name="bands.pkl")
                except:
                    print("Failed")
                    print("Parsing vasprun.xml... please wait...")
                    run = Vasprun("./Band-DOS/vasprun.xml", parse_dos=True)
                    bands = run.get_band_structure("./Band-DOS/KPOINTS")
                    print("Done!")
                    save_pickle_data(name="vasprun.dos.pkl", obj=run)
                    save_pickle_data(name="bands.pkl", obj=bands)
                cdos = run.complete_dos
            self.cdos = cdos
        # -- plot only Band : BS VASP run
        else:
            if elt_projected:
                try:
                    run = load_pickle_data(name="vasprun.elt.pkl")
                    bands = load_pickle_data(name="bands.elt.pkl")
                except:
                    print("Pickled data not exsit.")
                    print("Parsing vasprun.xml... please wait...")
                    run = BSVasprun("./Band-DOS/vasprun.xml", parse_projected_eigen=True)
                    bands = run.get_band_structure("./Band-DOS/KPOINTS")
                    print("Done!")
                    save_pickle_data(name="vasprun.elt.pkl", obj=run)
                    save_pickle_data(name="bands.elt.pkl", obj=bands)
            else:
                try:
                    run = load_pickle_data(name="vasprun.pkl")
                    bands = load_pickle_data(name="bands.pkl")
                except:
                    print("Pickled data not exist")
                    print("Parsing vasprun.xml... please wait...")
                    run = BSVasprun("./Band-DOS/vasprun.xml")
                    bands = run.get_band_structure("./Band-DOS/KPOINTS")
                    print("Done!")
                    save_pickle_data(name="vasprun.pkl", obj=run)
                    save_pickle_data(name="bands.pkl", obj=bands)

        self.bands = bands
        

    def blueBand(self, miny=None,maxy=None):
        bands = self.bands        
        plotter = BSPlotter(bands)
        self.plotter = plotter

        plotter.get_plot(zero_to_efermi=True)        
        plt.axhline(y=0, lw=1, ls=':', color='gray')
        plt.tick_params(labelsize=15)
        plt.ylim(miny, maxy)
               
        return plt

    def colorBand(self, miny=None,maxy=None,elt_ordered=None):
        bands = self.bands
        plotter = BSPlotterProjected(bands)
        self.plotter = plotter

        if elt_ordered:
            pass
        else:
            elts = plotter._bs.structure.composition.elements
            while len(elts) > 3:
                print("Maximum 3 elements available.")
                print(elts)
                print(str(elts[-1])+" will be removed.")                
            else:
                elt_ordered = [str(e) for e in elts]

        self.elt_ordered = elt_ordered
        
        plotter.get_elt_projected_plots_color(zero_to_efermi=True, elt_ordered=elt_ordered)        
        plt.axhline(y=0, lw=1, ls=':', color='gray')
        plt.tick_params(labelsize=15)

        if len(elt_ordered) == 3:            
            colors = ["b","r","g"]
        elif len(elt_ordered) == 2:
            colors = ["b","r"]
        for i in range(len(elt_ordered)):            
            plt.plot(0,0,color=colors[i],label=elt_ordered[i],linewidth=2)
        plt.legend(fancybox=True,shadow=True,prop={'size':18})
        plt.ylim(miny,maxy)

        return plt


    def element_DOS(self, miny=None,maxy=None, minx=None, maxx=None, elt_ordered=None,with_band=False):
        from pymatgen.core.periodic_table import Element
        
        cdos = self.cdos
        elt_dos = cdos.get_element_dos()

        plotter = DosPlotter(zero_at_efermi=True, sigma=0.02)
        if with_band:
            for i in range(len(elt_ordered)):
                i=i+1
                plotter.add_dos_dict({Element(elt_ordered[-i]) : elt_dos[Element(elt_ordered[-i])]})
        else:
            plotter.add_dos_dict(elt_dos)

        plt = plotter.get_rotate_plot()
        
        plt.axhline(y=0, lw=1, ls=':', color='gray')
        plt.tick_params(labelsize=15)
        plt.legend(fancybox=True,shadow=True,prop={'size':18})
        plt.ylim(miny,maxy)
        plt.xlim(minx,maxx)

        return plt


    def save_band_data(self, color=False, savefig=True):
        plotter = self.plotter
        metal = plotter._bs.is_metal()
        if metal:
            vbm = 0
            cbm = 0
            gap = 0
            efermi = plotter._bs.efermi
            print("* efermi : "+str(efermi))
            print("* band gap : "+str(gap))
        else:            
            band_data = plotter.bs_plot_data(zero_to_efermi=True)
            if band_data['vbm'][0][1] != band_data['vbm'][1][1]:
            print("???? 2 vbm ?? Notice to bsjun please")
            vbm = band_data['vbm'][0][1]
            cbm = band_data['cbm'][0][1]
            gap = band_data['band_gap']
            efermi = plotter._bs.efermi
            print("* efermi : "+str(efermi))
            print("* "+str(gap))
            gap = gap.split()[-1]
        print("* cbm : "+str(cbm))
        print("* vbm : "+str(vbm))
        datafile = open("band.dat","w")
        datafile.write("bandgap "+str(gap)+"\ncbm "+str(cbm)+"\nvbm "+str(vbm)+"\n")    
        datafile.close()
        print("* Save band data : band.dat")        
        pwd = os.getcwd()
        dirname = pwd.split("/")[-1]

        if savefig:
            if color:
                figname=str(dirname)+"_colorBand.png"
                fig.savefig(figname)
                makejpg="convert "+figname+" "+figname.replace(".png",".jpg")
                subprocess.call(makejpg, shell=True)        
                print("* Save figure : "+figname+", "+figname.replace(".png",".jpg"))            
            else:
                figname=str(dirname)+"_blueBand.png"
                fig.savefig(figname)
                makejpg="convert "+figname+" "+figname.replace(".png",".jpg")
                subprocess.call(makejpg, shell=True)        
                print("* Save figure : "+figname+", "+figname.replace(".png",".jpg"))


def save_pickle_data(name=None, obj=None):
    print("Saving pickled data.. "+name)
    with open(name, 'wb') as data:
        pickle.dump(obj, data)
    print("Done!")

def load_pickle_data(name=None):
    print("Reading pickled data.. "+name)
    with open(name, 'rb') as data:
        loaded = pickle.load(data)
    print("Done!")
    return loaded


if __name__=="__main__":
    try:
        sys.argv[1]
    except:
        print("""-------------------------------------
Usage : cms_band [option] [miny] [maxy] [sub_option1] [sub_option2]...
[option]
1  : blue band
2  : color band
3  : blue band & element DOS
4  : color band & element DOS
[sub_options]
n  : Do not show figure, just save images (ex : cms_band 1 -3 3 n)
a  : Make element order as default (ex : cms_band 2 -3 3 a)
-e : Make element order (ex : cms_band 2 -3 3 -eCo,Ni)
-d : Make DOS x-axis limitation (ex : cms_band 3 -3 3 -d0,5)
                                (ex : cms_band 3 -3 3 -eC,N -d0,5)
-------------------------------------"""
              )
        quit()
    try:
        lims = [float(sys.argv[2]), float(sys.argv[3])]        
        miny = min(lims)
        maxy = max(lims)
    except:        
        get_lim = raw_input("** Set y-limitaion (ex: -5,5) : ")
        lims = get_lim.split(",")
        miny = float(min(lims))
        maxy = float(max(lims))

    if sys.argv[1] == "1":
        fig = plt.figure(figsize=(6,10))
        
        cms_band = CMSBand()
        plt = cms_band.blueBand(miny=miny,maxy=maxy)
        plt.tight_layout()

        cms_band.save_band_data(color=False)        
        if "n" not in sys.argv:
            plt.show()

    elif sys.argv[1] == "2":
        fig = plt.figure(figsize=(6,10))

        cms_band = CMSBand(elt_projected=True)

        elt_argv = [argv for argv in sys.argv if "-e" in argv]
        if len(elt_argv) > 0:
            elt=True
            elt_argv = elt_argv[0]
            elt_argv = elt_argv.replace("-e","")
        else:
            elt=False                
        
        if "a" in sys.argv:
            elt_ordered = None
        elif elt:
            elt_ordered = elt_argv.split(",")
            elt_ordered = elt_ordered
        else:
            get_atoms = raw_input("* Input the elements order to plot band (ex : C,N) : ")
            get_atoms = get_atoms.replace(" ","")
            elt_ordered = get_atoms.split(',')
            if len(elt_ordered) > 3:
                get_atoms = raw_input("* Maximum 3 elements available : ")
                elt_ordered = get_atoms.split(',')
            elt_ordered = elt_ordered
        
        plt = cms_band.colorBand(miny=miny,maxy=maxy,elt_ordered=elt_ordered)
        plt.tight_layout()

        cms_band.save_band_data(color=True)        
        if "n" not in sys.argv:
            plt.show()

    elif sys.argv[1] == "3":
        fig = plt.figure(figsize=(12,10))

        ## setting element order
        elt_argv = [argv for argv in sys.argv if "-e" in argv]
        if len(elt_argv) > 0:
            elt=True
            elt_argv = elt_argv[0]
            elt_argv = elt_argv.replace("-e","")
        else:
            elt=False
        
        if "a" in sys.argv:
            elt_ordered = None
        elif elt:
            elt_ordered = elt_argv.split(",")
            elt_ordered = elt_ordered
        else:
            get_atoms = raw_input("* Input the elements order to plot band (ex : C,N) : ")
            get_atoms = get_atoms.replace(" ","")
            elt_ordered = get_atoms.split(',')
            if len(elt_ordered) > 3:
                get_atoms = raw_input("* Maximum 3 elements available : ")
                elt_ordered = get_atoms.split(',')
            elt_ordered = elt_ordered

        ## setting DOS x-lim
        doslim = [argv for argv in sys.argv if "-d" in argv]
        if len(doslim) > 0:
            doslim = doslim[0]
            doslim = doslim.replace("-d","")
            doslim = doslim.split(",")
            doslim = [float(doslim[0]), float(doslim[1])]
            dosmin = min(doslim)
            dosmax = max(doslim)
        else:
            dosmin = None
            dosmax = None
            

        plt.subplot(121)
        cms_band = CMSBand(elt_projected=True, dos=True)        
        plt = cms_band.blueBand(miny=miny,maxy=maxy)
        
        cms_band.save_band_data(color=False, savefig=False)
        
        plt.subplot(122)
        plt = cms_band.element_DOS(miny=miny,maxy=maxy,minx=dosmin,maxx=dosmax,elt_ordered=elt_ordered,with_band=True)
        plt.tight_layout()

        pwd = os.getcwd()
        dirname = pwd.split("/")[-1]
        figname = dirname+"_blueBand_DOS.png"
        plt.savefig(figname)
        makejpg="convert "+figname+" "+figname.replace(".png",".jpg")
        subprocess.call(makejpg, shell=True)
        print("* Save figure : "+figname+", "+figname.replace(".png",".jpg"))
        
        if "n" not in sys.argv:
            plt.show()

    elif sys.argv[1] == "4":
        fig = plt.figure(figsize=(12,10))

        cms_band = CMSBand(elt_projected=True, dos=True)
        

        ## setting element order
        elt_argv = [argv for argv in sys.argv if "-e" in argv]
        if len(elt_argv) > 0:
            elt=True
            elt_argv = elt_argv[0]
            elt_argv = elt_argv.replace("-e","")
        else:
            elt=False
        
        if "a" in sys.argv:
            elt_ordered = None
        elif elt:
            elt_ordered = elt_argv.split(",")
            elt_ordered = elt_ordered
        else:
            get_atoms = raw_input("* Input the elements order to plot band (ex : C,N) : ")
            get_atoms = get_atoms.replace(" ","")
            elt_ordered = get_atoms.split(',')
            if len(elt_ordered) > 3:
                get_atoms = raw_input("* Maximum 3 elements available : ")
                elt_ordered = get_atoms.split(',')
            elt_ordered = elt_ordered

        ## setting DOS x-lim
        doslim = [argv for argv in sys.argv if "-d" in argv]
        if len(doslim) > 0:
            doslim = doslim[0]
            doslim = doslim.replace("-d","")
            doslim = doslim.split(",")
            doslim = [float(doslim[0]), float(doslim[1])]
            dosmin = min(doslim)
            dosmax = max(doslim)
        else:
            dosmin = None
            dosmax = None

        plt.subplot(121)
        plt = cms_band.colorBand(miny=miny,maxy=maxy,elt_ordered=elt_ordered)
        cms_band.save_band_data(color=True, savefig=False)

        plt.subplot(122)
        plt = cms_band.element_DOS(miny=miny,maxy=maxy,minx=dosmin,maxx=dosmax,elt_ordered=cms_band.elt_ordered,with_band=True)
        plt.tight_layout()

        pwd = os.getcwd()
        dirname = pwd.split("/")[-1]
        figname = dirname+"_colorBand_DOS.png"
        plt.savefig(figname)
        makejpg="convert "+figname+" "+figname.replace(".png",".jpg")
        subprocess.call(makejpg, shell=True)        
        print("* Save figure : "+figname+", "+figname.replace(".png",".jpg"))

        if "n" not in sys.argv:
            plt.show()
