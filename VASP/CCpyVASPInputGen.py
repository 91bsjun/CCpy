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
1 : Relaxation calculation  (from initial structure files)
2 : Band-DOS calculation    (after previous calculation)
3 : Band-DOS calculation    (from initial structure files)
4 : Static calculation      (after previous calculation)
5 : MIT relaxation calculation
6 : MP HSE band calculation

[sub_options]
ex) CCpyVASPInputGen.py 1 -isif=2 -spin -mag -kp=4,4,2 ...
    < INCAR OPTION >
    -sp      : Single point calculation      (DEFAULT : NSW = 200)
    -isif=# : ISIF value                     (DEFAULT : 3)
    -vdw     : DFT-D2 grimme's function      (DEFAULT : not used)
    -spin    : Spin polarized calculation    (DEFAULT : unpolarized)
    -mag     : Add magnetic monet parameters (values from Pymatgen)
    -ldau    : Add LDA+U parameters          (values from Pymatgen)

    < KPOINTS OPTION >
    -kp=#,#,#                                (DEFAULT : reciprocal parameter as devided by 20)'''
          )
    quit()


single_point=False
cell_fixed=False
vdw=False
spin=False
mag=False
ldau=False
isif=False
kpoints=False

for arg in sys.argv:
    if "-sp" == arg:
        single_point=True
    elif "-isif=" in arg:
        isif = int(arg.split("=")[-1])
    elif "-vdw" in arg:
        vdw=True
    elif "-spin" in arg:
        spin=True
    elif "-kp" in arg:
        kpoints = arg.split("=")[1].split(",")
    elif "-ldau" in arg:
        ldau = True
    elif "-mag" in arg:
        mag = True


if sys.argv[1] == "1":
    input_marker = [".xsd", ".cif", "POSCAR", "CONTCAR"]
    inputs = selectInputs(input_marker, "./")
    chk = True
    for each_input in inputs:
        VI = VASPInput(each_input)
        if chk:
            VI.cms_vasp_set(single_point=single_point, isif=isif, vdw=vdw, kpoints=kpoints, spin=spin, mag=mag,
                            ldau=ldau, input_incar=None, input_kpts=None)
            incar = open(VI.dirname + "/INCAR").read()
            kpts = open(VI.dirname + "/KPOINTS").read()
            same_inputs = raw_input("* Do you want create other inputs as same as these INCAR and KPOINTS ? (y/n)")
            if same_inputs == "y":
                chk = False
            else:
                chk = True
        else:
            VI.cms_vasp_set(single_point=single_point,isif=isif,vdw=vdw,kpoints=kpoints,spin=spin,mag=mag,ldau=ldau,
                            input_incar=incar, input_kpts=kpts)

elif sys.argv[1] == "2":
    inputs = selectVASPOutputs("./")
    for each_input in inputs:
        os.chdir(each_input)
        VI = VASPInput("CONTCAR")
        VI.cms_band_set(spin=spin,mag=mag,ldau=ldau)
        os.chdir("../")

elif sys.argv[1] == "3":
    input_marker = [".xsd", ".cif", "POSCAR", "CONTCAR"]
    inputs = selectInputs(input_marker, "./")
    for each_input in inputs:
        VI = VASPInput(each_input)
        dirname = each_input.replace(".cif","").replace(".xsd","")
        os.mkdir(dirname)
        os.chdir(dirname)
        VI.cms_band_set(kpoints=kpoints,spin=spin,mag=mag,ldau=ldau)
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
        MIT_relax_VI = VASPInput(each_input)
        MIT_relax_VI.MIT_relax_set()

elif sys.argv[1] == "6":
    input_marker = [".xsd", ".cif", "POSCAR", "CONTCAR"]
    inputs = selectInputs(input_marker, "./")
    for each_input in inputs:
        MP_HSE_VI = VASPInput(each_input)
        MP_HSE_VI.MP_HSE_band_set()






