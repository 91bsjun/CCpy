from pymatgen.analysis.diffusion_analyzer import DiffusionAnalyzer as DA
from pymatgen_diffusion.aimd.van_hove import VanHoveAnalysis
from pymatgen_diffusion.aimd.pathway import ProbabilityDensityAnalysis

__author__ = "Hoijung Jung"


def vanhove_use(DaDa,spe,where_a):
    van = VanHoveAnalysis(DaDa, avg_nsteps=50, ngrid=201, rmax=5.0, step_skip=10, sigma=0.1, cellrange=1, species=spe,indices=None)
    plt = van.get_1d_plot(type='distinct', times=[0.0, 1.0, 2.0, 3.0, 4.0, 5.0], colors=None)
    plt.savefig("v01"+where_a+"distinct.png")
    plt_3 = van.get_3d_plot(figsize=(12, 8), type='distinct')
    plt_3.savefig("v02"+where_a+"distinct.png")
    print("finished v01distinct.png and v02distinct.png ")

def pdpd_use(DaDa,spe,where_a):
    proden = ProbabilityDensityAnalysis.from_diffusion_analyzer(DaDa, interval=0.5, species=spe)
    proden.to_chgcar(filename=where_a+'pd_CHGCAR.vasp')




if len(sys.argv) != 4 : # 5== 4option ok~
    print("must typing 3 option " )
    print("1 option : species" )
    print("2 option : msd plot type , if typing 1 = overall,a,b,c or typing 2 = species msd or typing 3= 1+2 type" )
    print("3 option : want vanhove and probability density  ex:) v =vanhove , p=probability density vp=two option nn=non using vanhove and probability density  " )
    print("ex:)  Li 2 vp == Li species and want species MSD and vanhove cal , pd cal")
    sys.exit()

spe=sys.argv[1]
type=sys.argv[2]
vpvp=sys.argv[3]

where_="_"+os.path.split(os.getcwd())[-1]+"_"



matplotlib.use('Agg')

a = DA.from_files(filepaths=["vasprun.xml"], specie=spe, step_skip=10, min_obs=25)


# ----- data
sd = a.get_summary_dict(include_msd_t=True)

#make diffusion dict
f = open("01Diffusion.dat" ,"w")
f.write("Diffusivity : " + str(sd['D']) + " cm^2/s\n")
f.write("Conductivity : " + str(sd['S']) + " mS/cm\n")
f.write("Diffusivity_s : " + str(sd['D_sigma']) + " cm^2/s\n")
f.write("Conductivity_s : " + str(sd['S_sigma']) + " mS/cm\n")
f.write("Diffusivity_charge : " + str(sd['D_charge']) + " cm^2/s\n")
f.write("Diffusivity_charge_s : " + str(sd['D_charge_sigma']) + " cm^2/s\n")

f.write("Specie    : " + str(sd['specie']) + " \n")
f.write("Time Step : " + str(sd['time_step']) + " fs\n")
f.write("Step Skip : " + str(sd['step_skip']) + " step\n")
f.write("Temperature : " + str(sd['temperature']) + " K\n")
f.write("Haven_ratio : " + str(sd['Haven_ratio']) + " \n")
#f.write("msd : " + str(sd['msd']) + " \n")
#f.write("dt : " + str(sd['dt']) + " \n")
f.close()

if type== "2":
    plt = a.get_msd_plot(mode="species")
    plt.savefig("02"+where_+"msd_spe.png")
    print("finished 02msd_spe.png")
elif type=="1":
    plt = a.get_msd_plot(mode="direction")
    plt.savefig("02"+where_+"msd_dir.png")
    print("finished 02msd_dir.png")
elif type == "3":
    plt_1 = a.get_msd_plot(mode="species")
    plt_1.savefig("02"+where_+"msd_spe.png")
    plt_2 = a.get_msd_plot(mode="direction")
    plt_2.savefig("02"+where_+"msd_dir.png")
    print("finished 02msd_spe.png and 02msd_dir.png")

if vpvp=="nn":
    print("non using vanhove and probability density")
elif vpvp=="v":
    vanhove_use(a,spe,where_)
elif vpvp=="p":
    pdpd_use(a,spe,where_)
elif vpvp=="vp":
    vanhove_use(a,spe,where_)
    pdpd_use(a,spe,where_)