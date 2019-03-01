import os, sys
from collections import OrderedDict

from pymatgen.core.structure import IStructure

from CCpy.SIESTA.SIESTAio import SIESTARelaxset, SIESTAMDset
from CCpy.Tools.CCpyTools import selectInputs
from CCpy.Tools.CCpyTools import header_printer as header


version = sys.version
if version[0] == '3':
    raw_input = input

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [sub_option1] [sub_option2..]")
    print('''--------------------------------------
[options]
1 : Relaxation calculation  (from initial structure files)
2 : MD calculation    (after previous calculation)
    -type=[Verlet / Nose / ParrinelloRahman / NoseParrinelloRahman / Anneal]   : ensemble
          [NVE    / NVT  / NPE              / NPT                  /       ]          
    -iT=[integer]       : initial temperature
    -fT=[integer]       : final temperature
    -ts=[integer]       : timestep (fs) / default: 2
    -ms=[integer]       : max timestep
    
    ex) CCpySIESTAInputGen.py 2 -type=Nose -iT=1000 -fT=1000 -ts=1 -ms=5000

[common options]
    -spin               : spin polarized / default: non polarized
    -kp=[#,#,#]         : assign k-points / default: lattice_param / 20
    
    ex) CCpySIESTAInputGen.py 2 -spin -kp=4,4,1 -type=Nose -iT=1000 -fT=1000 -ts=1 -ms=5000
''')
    quit()

potential_dirpath = '/home/shared/SIESTA_POT/'

single_point = False
spin = False
kpoints = None
init_T = None
final_T = None
timestep = 2
max_timestep = None
run_type = 'CG'

user_option = OrderedDict({})
for arg in sys.argv:
    if "-sp" == arg:
        single_point=True
    elif "-spin" in arg:
        user_opiton['SpinPolarized'] = '.true.'
    elif "-kp" in arg:
        kpoints = arg.split("=")[1].split(",")
    elif "-type" in arg:
        run_type = arg.split("=")[1]
        user_option['MD.TypeOfRun'] = run_type
    elif "-iT" in arg:
        init_T = int(arg.split("=")[1])
    elif "-fT" in arg:
        final_T = int(arg.split("=")[1])
    elif "-ts" in arg:
        timestep = int(arg.split("=")[1])
    elif "-ms" in arg:
        max_timestep = int(arg.split("=")[1])

def mkdir(dirname):
    try:
        os.mkdir(dirname)
    except:
        pass

if sys.argv[1] == "1":
    input_marker = [".cif", "POSCAR", "CONTCAR"]
    inputs = selectInputs(input_marker, "./")
    in_option = raw_input("Add or modifty option (ex: XC.functional=LDA, MaxSCFIterations=200) / else Enter\n: ")
    if len(in_option) != 0:
        in_option = in_option.replace(" ", "").split(",")
        for o in in_option:
            key = o.split("=")[0]
            val = o.split("=")[1]
            user_opiton[key] = val

    for each_input in inputs:
        if '.cif' in each_input:
            name = each_input.replace(".cif", "")
        else:
            name = each_input + '_relax'
        structure = IStructure.from_file(each_input)
        mkdir(name)
        os.chdir(name)
        input_set = SIESTARelaxset(structure, name, user_calc_option=user_opiton, in_kpt=kpoints)
        input_set.write_input(filename=name+".fdf", potential_dirpath=potential_dirpath)
        os.chdir("../")
elif sys.argv[1] == "2":
    input_marker = [".cif", "POSCAR", "CONTCAR"]
    inputs = selectInputs(input_marker, "./")
    in_option = raw_input("Add or modifty option (ex: XC.functional=LDA, MaxSCFIterations=200) / else Enter\n: ")
    if len(in_option) != 0:
        in_option = in_option.replace(" ", "").split(",")
        for o in in_option:
            key = o.split("=")[0]
            val = o.split("=")[1]
            user_opiton[key] = val

    for each_input in inputs:
        if '.cif' in each_input:
            name = each_input.replace(".cif", "")
        else:
            name = each_input + '_siesta'
        structure = IStructure.from_file(each_input)
        mkdir(name)
        os.chdir(name)
        input_set = SIESTAMDset(structure, name, run_type, init_T, final_T, max_timestep, user_calc_option=user_option, in_kpt=kpoints)
        input_set.write_input(filename=name+".fdf", potential_dirpath=potential_dirpath)
        os.chdir("../")
