#!/usr/bin/env python

import os, sys
import subprocess

import matplotlib.pyplot as plt

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.io.vasp.outputs import BSVasprun
from pymatgen.electronic_structure.core import Spin
# from pymatgen.electronic_structure.plotter import BSPlotter
# from pymatgen.electronic_structure.plotter import BSPlotterProjected
# from pymatgen.electronic_structure.plotter import DosPlotter
from CCpy.VASP.pymatgen_plotter import BSPlotter, BSPlotterProjected, DosPlotter
from CCpy.Tools.CCpyTools import selectVASPBandOutputs

import pickle

version = sys.version
if version[0] == '3':
    raw_input = input

class CMSBand():
    def __init__(self, elt_projected=False, dos=False, fig=None):
        # -- plot with DOS : Full VASP run
        if dos:
            if elt_projected:
                try:
                    run = load_pickle_data(name="vasprun.dos.elt.pkl")
                    bands = load_pickle_data(name="bands.elt.pkl")
                except:
                    sys.stdout.write("... Failed\n")
                    sys.stdout.write("Parsing vasprun.xml... please wait... ")
                    sys.stdout.flush()
                    run = Vasprun("./Band-DOS/vasprun.xml", parse_dos=True, parse_projected_eigen=True)
                    bands = run.get_band_structure("./Band-DOS/KPOINTS")
                    sys.stdout.write("Done!\n")
                    save_pickle_data(name="vasprun.dos.elt.pkl",obj=run)
                    save_pickle_data(name="bands.elt.pkl",obj=bands)
                cdos = run.complete_dos
            else:
                try:
                    run = load_pickle_data(name="vasprun.dos.pkl")
                    bands = load_pickle_data(name="bands.pkl")
                except:
                    sys.stdout.write("... Failed\n")
                    sys.stdout.write("Parsing vasprun.xml... please wait... ")
                    sys.stdout.flush()
                    run = Vasprun("./Band-DOS/vasprun.xml", parse_dos=True)
                    bands = run.get_band_structure("./Band-DOS/KPOINTS")
                    sys.stdout.write("Done!\n")
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
                    sys.stdout.write("Cannot find pickled data.\n")
                    sys.stdout.write("Parsing vasprun.xml... please wait... ")
                    sys.stdout.flush()
                    run = BSVasprun("./Band-DOS/vasprun.xml", parse_projected_eigen=True)
                    bands = run.get_band_structure("./Band-DOS/KPOINTS")
                    sys.stdout.write("Done!\n")
                    save_pickle_data(name="vasprun.elt.pkl", obj=run)
                    save_pickle_data(name="bands.elt.pkl", obj=bands)
            else:
                try:
                    run = load_pickle_data(name="vasprun.pkl")
                    bands = load_pickle_data(name="bands.pkl")
                except:
                    sys.stdout.write("Cannot find pickled data.\n")
                    sys.stdout.write("Parsing vasprun.xml... please wait... ")
                    sys.stdout.flush()
                    run = BSVasprun("./Band-DOS/vasprun.xml")
                    bands = run.get_band_structure("./Band-DOS/KPOINTS")
                    sys.stdout.write("Done!\n")
                    save_pickle_data(name="vasprun.pkl", obj=run)
                    save_pickle_data(name="bands.pkl", obj=bands)

        self.bands = bands
        self.fig = fig
        

    def blueBand(self, band_indices, miny=None, maxy=None, line_width=3):
        bands = self.bands        
        plotter = BSPlotter(bands)
        self.plotter = plotter

        plotter.get_plot(zero_to_efermi=True, line_width=line_width)
        plt.axhline(y=0, lw=1, ls=':', color='gray')
        plt.tick_params(labelsize=15)
        plt.ylim(miny, maxy)
               
        return plt

    def colorBand(self, miny=None, maxy=None, elt_ordered=None, color_order=['g','b','r'], line_width=3):
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

        # -- if single atom : return to blue band
        if len(elt_ordered) == 1:
            plt = self.blueBand(miny=miny,maxy=maxy,line_width=line_width)
            return plt
        else:
            self.elt_ordered = elt_ordered
            import matplotlib.pyplot as plt
            plotter.get_elt_projected_plots_color(zero_to_efermi=True, elt_ordered=elt_ordered, line_width=line_width, color_order=color_order)
            plt.axhline(y=0, lw=1, ls=':', color='gray')
            plt.tick_params(labelsize=15)

            colors = color_order
            # change vivid green to normal green
            for i, val in enumerate(color_order):
                if val == 'g':
                    color_order[i] = '#1DDB16'
                    
            for i in range(len(elt_ordered)):
                plt.plot(0, 0, color=colors[i], label=elt_ordered[i], linewidth=2)
            plt.legend(fancybox=True, shadow=True, prop={'size':18}, loc='upper right')
            plt.ylim(miny, maxy)

            return plt

    def colorBand_spin(self, miny=None, maxy=None, elt_ordered=None, color_order=['g','b','r'], line_width=3, spin="up"):
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

        # -- if single atom : return to blue band
        if len(elt_ordered) == 1:
            plt = self.blueBand(miny=miny,maxy=maxy,line_width=line_width)
            return plt
        else:
            self.elt_ordered = elt_ordered
            import matplotlib.pyplot as plt
            plotter.get_elt_projected_plots_color_spin(zero_to_efermi=True, elt_ordered=elt_ordered, color_order=color_order, line_width=line_width, spin=spin)
            plt.axhline(y=0, lw=1, ls=':', color='gray')
            plt.tick_params(labelsize=15)

            colors = color_order
            # change vivid green to normal green
            for i, val in enumerate(color_order):
                if val == 'g':
                    color_order[i] = '#1DDB16'
            for i in range(len(elt_ordered)):
                plt.plot(0,0,color=colors[i],label=elt_ordered[i],linewidth=2)
            plt.legend(fancybox=True,shadow=True,prop={'size':18}, loc='upper right')
            plt.ylim(miny,maxy)

            return plt


    def element_DOS(self, miny=None,maxy=None, minx=None, maxx=None, elt_ordered=None, color_order=['g', 'b', 'r'], with_band=False):
        from pymatgen.core.periodic_table import Element
        
        cdos = self.cdos
        elt_dos = cdos.get_element_dos()

        plotter = DosPlotter(zero_at_efermi=True, sigma=0.02)
        if with_band:
            if elt_ordered == None:
                plotter.add_dos_dict(elt_dos)
            else:
                for i in range(len(elt_ordered)):
                    i=i+1
                    plotter.add_dos_dict({Element(elt_ordered[-i]) : elt_dos[Element(elt_ordered[-i])]})
        else:
            plotter.add_dos_dict(elt_dos)

        plt = plotter.get_rotate_plot(color_order=color_order)
        
        plt.axhline(y=0, lw=1, ls=':', color='gray')
        plt.tick_params(labelsize=15)
        plt.legend(fancybox=True, shadow=True, prop={'size':18}, loc='upper right')
        plt.ylim(miny,maxy)
        plt.xlim(minx,maxx)

        return plt


    def save_band_data(self, color=False, savefig=True):
        fig = self.fig
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
            if len(band_data['vbm']) > 1:
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

    def get_minimal_band_data(self):
        bands = self.bands
        if bands.is_metal():
            bgap, cbm, vbm = 0.0, 0.0, 0.0
        else:
            bgap = bands.get_band_gap()
            efermi = bands.efermi
            cbm = bands.get_cbm()
            vbm = bands.get_vbm()

            bgap, cbm, vbm = bgap['energy'], cbm['energy'], vbm['energy']

            cbm = cbm-efermi
            vbm = vbm-efermi

        datafile = open("band.dat", "w")
        datafile.write("bandgap " + str(bgap) + "\ncbm " + str(cbm) + "\nvbm " + str(vbm) + "\n")
        datafile.close()
        print("* Save band data : band.dat")

    def save_band_structure(self):
        plotter = self.plotter
        data = plotter.bs_plot_data(zero_to_efermi=True)
        nb_bands = plotter._nb_bands

        band_data = {}
        for i in range(nb_bands):
            band_data['band' + str(i)] = []

        distances = []
        for di in range(len(data['distances'])):
            distances += data['distances'][di]
            energy_data = data['energy'][di][str(Spin.up)]
            for bi in range(len(energy_data)):
                band_data['band' + str(bi)] += energy_data[bi]
        band_data['distances'] = distances
        import pandas as pd
        df = pd.DataFrame(band_data).set_index('distances')
        df.to_csv('band_structure.csv')






def save_pickle_data(name=None, obj=None):
    sys.stdout.write("Saving pickled data.. " + name + "... ")
    sys.stdout.flush()
    with open(name, 'wb') as data:
        pickle.dump(obj, data)
    sys.stdout.write("Done!\n")

def load_pickle_data(name=None):
    sys.stdout.write("Reading pickled data.. " + name + "... ")
    with open(name, 'rb') as data:
        loaded = pickle.load(data)
    sys.stdout.write("Done!\n")
    return loaded

def main_run():
    # -- parsing options
    ylim = [-5, 5]
    miny = min(ylim)
    maxy = max(ylim)
    elt_ordered = None
    color_order = ['g', 'b', 'r']
    line_width = 3
    doslim = [0, 20]
    dosmin = min(doslim)
    dosmax = max(doslim)
    for argv in sys.argv:
        if '-ylim=' in argv:
            ylim = argv.replace("-ylim=", "")
            ylim = ylim.split(",")
            ylim = [float(ylim[0]), float(ylim[1])]
            miny = min(ylim)
            maxy = max(ylim)
        elif '-e=' in argv:
            elt = argv.replace("-e=", "")
            elt_ordered = elt.split(",")
        elif '-c=' in argv:
            rgb = argv.replace("-c=", "")
            if len(rgb) == 2:
                full_rgb = ['r', 'g', 'b']
                full_rgb.remove(rgb[0])
                full_rgb.remove(rgb[1])
                rgb += full_rgb[0]
            color_order = [rgb[0], rgb[1], rgb[2]]
        elif '-lw=' in argv:
            line_width = float(argv.replace("-lw=", ""))
        elif '-dlim=' in argv:
            doslim = argv.replace("-dlim=", "")
            doslim = doslim.split(",")
            doslim = [float(doslim[0]), float(doslim[1])]
            dosmin = min(doslim)
            dosmax = max(doslim)


        """
        old version method to get elt order
        else:
            get_atoms = raw_input("* Input the elements order to plot band (ex : C,N) : ")
            get_atoms = get_atoms.replace(" ", "")
            elt_ordered = get_atoms.split(',')
            if len(elt_ordered) > 3:
                get_atoms = raw_input("* Maximum 3 elements available : ")
                elt_ordered = get_atoms.split(',')
            elt_ordered = elt_ordered
        """
    if sys.argv[1] != "0":
        import matplotlib.pyplot as plt

    if sys.argv[1] == "0":
        cms_band = CMSBand()
        cms_band.get_minimal_band_data()

    # -- blue band
    elif sys.argv[1] == "1":
        fig = plt.figure(figsize=(6, 10))

        band_indices = False
        cms_band = CMSBand(fig=fig)
        for argv in sys.argv:
            if '-nb=' in argv:
                from CCpy.CCpyTools import input_num_parser
                nb_bands = cms_band.bands.nb_bands
                band_indices = input_num_parser(nb_bands)

        plt = cms_band.blueBand(miny=miny, maxy=maxy, line_width=line_width, band_indices=band_indices)
        plt.tight_layout()

        cms_band.save_band_data(color=False)
        #cms_band.save_band_structure()
        
        if "n" not in sys.argv:
            plt.show()

    # -- color band
    elif sys.argv[1] == "2":
        fig = plt.figure(figsize=(6, 10))

        cms_band = CMSBand(elt_projected=True, fig=fig)
        plt = cms_band.colorBand(miny=miny, maxy=maxy, elt_ordered=elt_ordered, line_width=line_width, color_order=color_order)
        plt.tight_layout()

        cms_band.save_band_data(color=True)
        
        if "n" not in sys.argv:
            plt.show()

    # -- blue band & elt DOS
    elif sys.argv[1] == "3":
        fig = plt.figure(figsize=(12, 10))


        plt.subplot(121)
        cms_band = CMSBand(elt_projected=True, dos=True, fig=fig)
        plt = cms_band.blueBand(miny=miny, maxy=maxy, line_width=line_width)

        cms_band.save_band_data(color=False, savefig=False)

        plt.subplot(122)
        plt = cms_band.element_DOS(miny=miny, maxy=maxy, minx=dosmin, maxx=dosmax, elt_ordered=elt_ordered, color_order=color_order,
                                   with_band=True)
        plt.tight_layout()

        pwd = os.getcwd()
        dirname = pwd.split("/")[-1]
        figname = dirname + "_blueBand_DOS.png"
        plt.savefig(figname)
        makejpg = "convert " + figname + " " + figname.replace(".png", ".jpg")
        subprocess.call(makejpg, shell=True)
        print("* Save figure : " + figname + ", " + figname.replace(".png", ".jpg"))

        if "n" not in sys.argv:
            plt.show()

    # -- color band & elt band
    elif sys.argv[1] == "4":
        fig = plt.figure(figsize=(12, 10))

        cms_band = CMSBand(elt_projected=True, dos=True, fig=fig)

        plt.subplot(121)
        plt = cms_band.colorBand(miny=miny, maxy=maxy, elt_ordered=elt_ordered, color_order=color_order, line_width=line_width)
        cms_band.save_band_data(color=True, savefig=False)

        plt.subplot(122)
        plt = cms_band.element_DOS(miny=miny, maxy=maxy, minx=dosmin, maxx=dosmax, elt_ordered=cms_band.elt_ordered, color_order=color_order,
                                   with_band=True)
        plt.tight_layout()

        pwd = os.getcwd()
        dirname = pwd.split("/")[-1]
        figname = dirname + "_colorBand_DOS.png"
        plt.savefig(figname)
        makejpg = "convert " + figname + " " + figname.replace(".png", ".jpg")
        subprocess.call(makejpg, shell=True)
        print("* Save figure : " + figname + ", " + figname.replace(".png", ".jpg"))

        if "n" not in sys.argv:
            plt.show()


    # -- color band with spin
    elif sys.argv[1] == "5":
        fig = plt.figure(figsize=(6, 10))

        cms_band = CMSBand(elt_projected=True, fig=fig)

        if "up" in sys.argv:
            spin = "up"
        elif "down" in sys.argv:
            spin = "down"

        plt = cms_band.colorBand_spin(miny=miny, maxy=maxy, elt_ordered=elt_ordered, color_order=color_order, line_width=line_width, spin=spin)
        plt.tight_layout()

        pwd = os.getcwd()
        dirname = pwd.split("/")[-1]
        figname = dirname + "_colorBand_spin_"+spin+".png"
        plt.savefig(figname)
        makejpg = "convert " + figname + " " + figname.replace(".png", ".jpg")
        subprocess.call(makejpg, shell=True)
        print("* Save figure : " + figname + ", " + figname.replace(".png", ".jpg"))

        cms_band.save_band_data(color=True, savefig=False)
        if "n" not in sys.argv:
            plt.show()


if __name__=="__main__":
    try:
        sys.argv[1]
    except:
        print("""-------------------------------------
Usage : cms_band [option] [sub_option1] [sub_option2]...
-------------------------------------
[option]
0   : save band data only (band gap, cbm, vbm)
1   : blue band
2   : color band
3   : blue band & element DOS
4   : color band & element DOS
5   : color band spin UP or DOWN

[sub_options]
-ylim=       : set y-axis limitation (default: -5,5)
              (ex: CCpyBand.py 1 -ylim=-3,3)

-e=          : assign element order when plot color band (maximum 3 elements available, default: in vasprun.xml)
              (ex: CCpyBand.py 2 -e=Co,Ni)

-c=          : assign color order when plot color band (default: gbr)
              (ex: CCpyBand.py 2 -c=brg)
              (ex: CCpyBand.py 2 -c=gb -e=C,N)
      
-dlim=       : Set DOS x-axis limitation (default: 0,20)
              (ex: CCpyBand.py 3 -dlim=0,5)
              (ex: CCpyBand.py 4 -e=Co,Ni -c=br -dlim=0,15)
      
-lw=         : Set line width of figure (default: 3)
              (ex: CCpyBand.py 2 -lw=5)
              
up/down      : if spin polarized calculation / option 5
              (ex: CCpyBand.py 5 -ylim=4,4 down)

a            : Select all possible directories
              (ex: CCpyBand.py 1 a)
              
n            : Do not show figure, just save figure and band.dat
              (ex: CCpyBand.py 1 n)
-------------------------------------"""
              )
        quit()

    ask = True
    if "a" in sys.argv:
        ask = False

    inputs = selectVASPBandOutputs("./", ask=ask)
    for each_input in inputs:
        os.chdir(each_input)
        print("* Current directory : " + each_input)
        main_run()
        os.chdir("../")
