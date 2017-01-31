#!/usr/local/bin/python2.7

import os, sys
from CCpy.VASP.VASPio import VASPInput
from CCpy.Tools.CCpyTools import selectInputs, selectVASPInputs, selectVASPOutputs, linux_command

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [sub_option1] [sub_option2..]")
    print('''--------------------------------------
[options]
1 : Relaxation calculation
2 : Band-DOS calculation (after previous calcualtion)
3 : Relaxation and Band-DOS (not supported yet)
4 : Static calculation (after previous calculation)
5 : MIT relaxation calculation

[sub_options]
INCAR OPTION
sp : Single point calculation (DEFAULT : non-single)
-isif=# : ISIF value (3: full relax, 2: cell fixed,... DEFAULT:3)
vdw : DFT-D2 grimme's function
spin : Spin polarized calculation (DEFAULT : unpolarized)

KPOINTS
-kp=#,#,# (DEFAULT : reciprocal parameter as devided by 20)'''
          )
    quit()


single_point=False
cell_fixed=False
vdw=False
spin=False
isif=False
kpoints=False

for arg in sys.argv:
    if "sp" == arg:
        single_point=True
    elif "-isif=" in arg:
        isif = int(arg.split("=")[-1])
    elif "vdw" in arg:
        vdw=True
    elif "spin" in arg:
        spin=True
    elif "-kp" in arg:
        kpoints = arg.split("=")[1].split(",")


if sys.argv[1] == "1":
    input_marker = [".xsd", ".cif", "POSCAR", "CONTCAR"]
    inputs = selectInputs(input_marker, "./")
    for each_input in inputs:
        VI = VASPInput(each_input)
        VI.cms_vasp_set(single_point=single_point,isif=isif,vdw=vdw,kpoints=kpoints,spin=spin)        
elif sys.argv[1] == "2":
    inputs = selectVASPOutputs("./")
    for each_input in inputs:
        os.chdir(each_input)
        VI = VASPInput("CONTCAR")
        VI.cms_band_set()
        os.chdir("../")

elif sys.argv[1] == "4":
    inputs = selectVASPOutputs("./")
    for each_input in inputs:
        os.chdir(each_input)

        static_VI = VASPInput("CONTCAR", dirname="STATIC")
        static_VI.MP_static_set()
        
        os.chdir("../")

elif sys.argv[1] == "5":
    input_marker = [".xsd", ".cif", "POSCAR", "CONTCAR"]
    inputs = selectInputs(input_marker, "./")
    for each_input in inputs:
        os.chdir(each_input)

        MIT_relax_VI = VASPInput(each_input)
        MIT_relax_VI.MIT_relax_set()

        os.chdir("../")








