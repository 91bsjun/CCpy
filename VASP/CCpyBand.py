#!/usr/bin/env python

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
                    sys.stdout.write("Pickled data not exsit.\n")
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
                    sys.stdout.write("Pickled data not exsit.\n")
                    sys.stdout.write("Parsing vasprun.xml... please wait... ")
                    sys.stdout.flush()
                    run = BSVasprun("./Band-DOS/vasprun.xml")
                    bands = run.get_band_structure("./Band-DOS/KPOINTS")
                    sys.stdout.write("Done!\n")
                    save_pickle_data(name="vasprun.pkl", obj=run)
                    save_pickle_data(name="bands.pkl", obj=bands)

        self.bands = bands
        self.fig = fig
        

    def blueBand(self, miny=None,maxy=None,line_width=3):
        bands = self.bands        
        plotter = BSPlotter(bands)
        self.plotter = plotter

        plotter.get_plot(zero_to_efermi=True, line_width=line_width)
        plt.axhline(y=0, lw=1, ls=':', color='gray')
        plt.tick_params(labelsize=15)
        plt.ylim(miny, maxy)
               
        return plt

    def colorBand(self, miny=None,maxy=None,elt_ordered=None,line_width=3):
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
            plotter.get_elt_projected_plots_color(zero_to_efermi=True, elt_ordered=elt_ordered, line_width=line_width)
            plt.axhline(y=0, lw=1, ls=':', color='gray')
            plt.tick_params(labelsize=15)

            if len(elt_ordered) == 3:
                colors = ["r","#1DDB16","b"]
            elif len(elt_ordered) == 2:
                colors = ["b","r"]
            for i in range(len(elt_ordered)):
                plt.plot(0,0,color=colors[i],label=elt_ordered[i],linewidth=2)
            plt.legend(fancybox=True,shadow=True,prop={'size':18}, loc='upper right')
            plt.ylim(miny,maxy)

            return plt

    def colorBand_spin(self, miny=None,maxy=None,elt_ordered=None,line_width=3,spin="up"):
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
            plotter.get_elt_projected_plots_color_spin(zero_to_efermi=True, elt_ordered=elt_ordered, line_width=line_width, spin=spin)
            plt.axhline(y=0, lw=1, ls=':', color='gray')
            plt.tick_params(labelsize=15)

            if len(elt_ordered) == 3:
                colors = ["r","#1DDB16","b"]
            elif len(elt_ordered) == 2:
                colors = ["b","r"]
            for i in range(len(elt_ordered)):
                plt.plot(0,0,color=colors[i],label=elt_ordered[i],linewidth=2)
            plt.legend(fancybox=True,shadow=True,prop={'size':18}, loc='upper right')
            plt.ylim(miny,maxy)

            return plt


    def element_DOS(self, miny=None,maxy=None, minx=None, maxx=None, elt_ordered=None,with_band=False):
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

        plt = plotter.get_rotate_plot()
        
        plt.axhline(y=0, lw=1, ls=':', color='gray')
        plt.tick_params(labelsize=15)
        plt.legend(fancybox=True,shadow=True,prop={'size':18})
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
    if sys.argv[1] != "0":
        import matplotlib.pyplot as plt
        try:
            lims = [float(sys.argv[2]), float(sys.argv[3])]
            miny = min(lims)
            maxy = max(lims)
        except:
            get_lim = raw_input("** Set y-limitaion (ex: -5,5) : ")
            lims = get_lim.split(",")
            miny = float(min(lims))
            maxy = float(max(lims))

        line_width = [argv for argv in sys.argv if "-lw" in argv]
        if len(line_width) > 0:
            line_width = line_width[0]
            line_width = line_width.replace("-lw", "")
            line_width = float(line_width)
        else:
            line_width = 3

    if sys.argv[1] == "0":
        cms_band = CMSBand()
        cms_band.get_minimal_band_data()

    # -- blue band
    elif sys.argv[1] == "1":
        fig = plt.figure(figsize=(6, 10))

        cms_band = CMSBand(fig=fig)
        plt = cms_band.blueBand(miny=miny, maxy=maxy, line_width=line_width)
        plt.tight_layout()

        cms_band.save_band_data(color=False)
        if "n" not in sys.argv:
            plt.show()

    # -- color band
    elif sys.argv[1] == "2":
        fig = plt.figure(figsize=(6, 10))

        cms_band = CMSBand(elt_projected=True, fig=fig)

        elt_argv = [argv for argv in sys.argv if "-e" in argv]
        if len(elt_argv) > 0:
            elt = True
            elt_argv = elt_argv[0]
            elt_argv = elt_argv.replace("-e", "")
        else:
            elt = False

        if "ed" in sys.argv:
            elt_ordered = None
        elif elt:
            elt_ordered = elt_argv.split(",")
            elt_ordered = elt_ordered
        else:
            get_atoms = raw_input("* Input the elements order to plot band (ex : C,N) : ")
            get_atoms = get_atoms.replace(" ", "")
            elt_ordered = get_atoms.split(',')
            if len(elt_ordered) > 3:
                get_atoms = raw_input("* Maximum 3 elements available : ")
                elt_ordered = get_atoms.split(',')
            elt_ordered = elt_ordered

        plt = cms_band.colorBand(miny=miny, maxy=maxy, elt_ordered=elt_ordered, line_width=line_width)
        plt.tight_layout()

        cms_band.save_band_data(color=True)
        if "n" not in sys.argv:
            plt.show()

    # -- blue band & elt DOS
    elif sys.argv[1] == "3":
        fig = plt.figure(figsize=(12, 10))

        ## setting element order
        elt_argv = [argv for argv in sys.argv if "-e" in argv]
        if len(elt_argv) > 0:
            elt = True
            elt_argv = elt_argv[0]
            elt_argv = elt_argv.replace("-e", "")
        else:
            elt = False

        if "ed" in sys.argv:
            elt_ordered = None
        elif elt:
            elt_ordered = elt_argv.split(",")
            elt_ordered = elt_ordered
        else:
            get_atoms = raw_input("* Input the elements order to plot band (ex : C,N) : ")
            get_atoms = get_atoms.replace(" ", "")
            elt_ordered = get_atoms.split(',')
            if len(elt_ordered) > 3:
                get_atoms = raw_input("* Maximum 3 elements available : ")
                elt_ordered = get_atoms.split(',')
            elt_ordered = elt_ordered

        ## setting DOS x-lim
        doslim = [argv for argv in sys.argv if "-d" in argv]
        if len(doslim) > 0:
            doslim = doslim[0]
            doslim = doslim.replace("-d", "")
            doslim = doslim.split(",")
            doslim = [float(doslim[0]), float(doslim[1])]
            dosmin = min(doslim)
            dosmax = max(doslim)
        else:
            dosmin = None
            dosmax = None

        plt.subplot(121)
        cms_band = CMSBand(elt_projected=True, dos=True, fig=fig)
        plt = cms_band.blueBand(miny=miny, maxy=maxy, line_width=line_width)

        cms_band.save_band_data(color=False, savefig=False)

        plt.subplot(122)
        plt = cms_band.element_DOS(miny=miny, maxy=maxy, minx=dosmin, maxx=dosmax, elt_ordered=elt_ordered,
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

        ## setting element order
        elt_argv = [argv for argv in sys.argv if "-e" in argv]
        if len(elt_argv) > 0:
            elt = True
            elt_argv = elt_argv[0]
            elt_argv = elt_argv.replace("-e", "")
        else:
            elt = False

        if "ed" in sys.argv:
            elt_ordered = None
        elif elt:
            elt_ordered = elt_argv.split(",")
            elt_ordered = elt_ordered
        else:
            get_atoms = raw_input("* Input the elements order to plot band (ex : C,N) : ")
            get_atoms = get_atoms.replace(" ", "")
            elt_ordered = get_atoms.split(',')
            if len(elt_ordered) > 3:
                get_atoms = raw_input("* Maximum 3 elements available : ")
                elt_ordered = get_atoms.split(',')
            elt_ordered = elt_ordered

        ## setting DOS x-lim
        doslim = [argv for argv in sys.argv if "-d" in argv]
        if len(doslim) > 0:
            doslim = doslim[0]
            doslim = doslim.replace("-d", "")
            doslim = doslim.split(",")
            doslim = [float(doslim[0]), float(doslim[1])]
            dosmin = min(doslim)
            dosmax = max(doslim)
        else:
            dosmin = None
            dosmax = None

        plt.subplot(121)
        plt = cms_band.colorBand(miny=miny, maxy=maxy, elt_ordered=elt_ordered, line_width=line_width)
        cms_band.save_band_data(color=True, savefig=False)

        plt.subplot(122)
        plt = cms_band.element_DOS(miny=miny, maxy=maxy, minx=dosmin, maxx=dosmax, elt_ordered=cms_band.elt_ordered,
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

        elt_argv = [argv for argv in sys.argv if "-e" in argv]
        if len(elt_argv) > 0:
            elt = True
            elt_argv = elt_argv[0]
            elt_argv = elt_argv.replace("-e", "")
        else:
            elt = False

        if "ed" in sys.argv:
            elt_ordered = None
        elif elt:
            elt_ordered = elt_argv.split(",")
            elt_ordered = elt_ordered
        else:
            get_atoms = raw_input("* Input the elements order to plot band (ex : C,N) : ")
            get_atoms = get_atoms.replace(" ", "")
            elt_ordered = get_atoms.split(',')
            if len(elt_ordered) > 3:
                get_atoms = raw_input("* Maximum 3 elements available : ")
                elt_ordered = get_atoms.split(',')
            elt_ordered = elt_ordered

        if "up" in sys.argv:
            spin = "up"
        elif "down" in sys.argv:
            spin = "down"

        plt = cms_band.colorBand_spin(miny=miny, maxy=maxy, elt_ordered=elt_ordered, line_width=line_width, spin=spin)
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
Usage : cms_band [option] [miny] [maxy] [sub_option1] [sub_option2]...
-------------------------------------
[option]
0   : save band data only (band gap, cbm, vbm)
1   : blue band
2   : color band
3   : blue band & element DOS
4   : color band & element DOS
5   : color band spin UP or DOWN
[sub_options]
a   : Select all possible directories
      (ex : CCpyBand.py 1 -3 3 a)
n   : Do not show figure, just save figure and band.dat
      (ex : CCpyBand.py 1 -3 3 n)
ed  : Make element order as default
      (ex : CCpyBand.py 2 -3 3 ed)
-e  : Make element order
      (ex : CCpyBand.py 2 -3 3 -eCo,Ni)
-d  : Set DOS x-axis limitation
      (ex : CCpyBand.py 3 -3 3 -d0,5)
      (ex : CCpyBand.py 3 -3 3 -eC,N -d0,5)
-lw : Set line width of figure
      (ex : CCpyBand.py 3 -3 3 -lw5) // default = 3
up/down  : if spin polarized calculation / option 5
      (ex : CCpyBand.py 5 -3 3 -eC,N down)
-------------------------------------"""
              )
        quit()

    ask = True
    if "a" in sys.argv:
        ask = False

    inputs = selectVASPBandOutputs("./", ask=ask)
    for each_input in inputs:
        os.chdir(each_input)
        print(each_input)
        main_run()
        os.chdir("../")
