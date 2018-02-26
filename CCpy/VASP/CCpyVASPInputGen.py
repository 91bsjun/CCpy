#!/usr/bin/env python

import os, sys
import json
from collections import OrderedDict

from CCpy.VASP.VASPio import VASPInput
from CCpy.Tools.CCpyTools import selectInputs, selectVASPInputs, selectVASPOutputs, linux_command

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
2 : Band-DOS calculation    (after previous calculation)
3 : Band-DOS calculation    (from initial structure files)
4 : Static calculation      (after previous calculation)

[sub_options]
ex) CCpyVASPInputGen.py 1 -isif=2 -spin -mag -kp=4,4,2 -vdw=D3damp, -pseudo=Nb_sv, -pot=LDA_54...

    < INCAR OPTION >
    -sp      : Single point calculation      (DEFAULT : NSW = 200)
    -isif=#  : ISIF value                    (DEFAULT : 3)
    -spin    : Spin polarized calculation    (DEFAULT : unpolarized)
    -mag     : Add magnetic monet parameters (values from Pymatgen)
    -ldau    : Add LDA+U parameters          (values from Pymatgen)

    van der Waals corrections                (DEFAULT : do not use)
    -vdw=D2     : DFT-D2 method of Grimme                   (VASP.5.2.11)
    -vdw=D3     : zero damping DFT-D3 method of Grimme      (VASP.5.3.4)
    -vdw=D3damp : DFT-D3 method with Becke-Jonson damping   (VASP.5.3.4)
    -vdw=dDsC   : dDsC dispersion correction method         (VASP.5.4.1)

    < KPOINTS OPTION >
    -kp=#,#,#                                (DEFAULT : reciprocal parameter as devided by 20)

    < POTCAR OPTION>
    -pot=PBE_54 : VASP potential setting     (DEFAULT : PBE_54)
                  Possible potentials = PBE, PBE_52, PBE_54, LDA, LDA_52, LDA_54, PW91, LDA_US, PW91_US
    -pseudo=    : Select pseudo potential    (DEFAULT : normal)
                  ex) -pseudo=Nb_sv,Ti_sv    --> will use 'Nb_sv, Ti_sv' pseudo potential to 'Nb, Ti'

[preset options]
~/.CCpy/*.json
    '''
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
pseudo=False
functional="PBE_54"

for arg in sys.argv:
    if "-sp" == arg:
        single_point=True
    elif "-isif=" in arg:
        isif = int(arg.split("=")[-1])
    elif "-vdw" in arg:
        vdw=arg.split("=")[1]
    elif "-spin" in arg:
        spin=True
    elif "-kp" in arg:
        kpoints = arg.split("=")[1].split(",")
    elif "-ldau" in arg:
        ldau = True
    elif "-mag" in arg:
        mag = True
    elif "-pot" in arg:
        functional = arg.split("=")[1]
    elif "-pseudo" in arg:
        pseudo = arg.split("=")[1].split(",")

if sys.argv[1] == "1":
    input_marker = [".xsd", ".cif", "POSCAR", "CONTCAR"]
    inputs = selectInputs(input_marker, "./")
    chk = True
    for each_input in inputs:
        VI = VASPInput(each_input)

        # -- First process of edit INCAR, KPOINTS while create Inputs
        # -- If user want to use same options go to else
        if chk:
            VI.cms_vasp_set(single_point=single_point, isif=isif, vdw=vdw, kpoints=kpoints, spin=spin, mag=mag, ldau=ldau,
                            functional=functional, pseudo=pseudo,
                            get_pre_options=None)
            same_inputs = raw_input("\n* Do you want create the rest of inputs as same as these INCAR ? (y/n)")
            if same_inputs == "y":
                chk = False
                jstring = open("current_options.json", "r").read()
                current_options = json.loads(jstring, object_pairs_hook=OrderedDict)
            else:
                chk = True
        else:
            VI.cms_vasp_set(single_point=single_point,isif=isif,vdw=vdw,kpoints=kpoints,spin=spin,mag=mag,ldau=ldau,
                            pseudo=pseudo, functional=functional,
                            get_pre_options=current_options)
    linux_command("rm current_options.json")

elif sys.argv[1] == "2":
    inputs = selectVASPOutputs("./")
    for each_input in inputs:
        if "KPOINTSP" in os.listdir("./"):
            print("\n* Line-mode KPOINTS file (KPOINTSP) has been detected.")
            input_line_kpts = open("KPOINTSP", "r").read()
        else:
            use_kpts = raw_input("\n* Line-mode KPOINTS file (KPOINSTP) is not detected.\n\n* Give line-mode KPOINTS file name, else Enter (make new)\n: ")
            if len(use_kpts) >= 1:
                input_line_kpts = use_kpts
            else:
                input_line_kpts = False
        sys.stdout.write(each_input + "... ")
        sys.stdout.flush()
        os.chdir(each_input)
        VI = VASPInput(additional=True, dirname=each_input)
        VI.cms_band_set(input_line_kpts=input_line_kpts)
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






