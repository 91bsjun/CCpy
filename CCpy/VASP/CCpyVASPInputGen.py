#!/usr/bin/env python
import os, sys
import shutil
import json
from collections import OrderedDict

from CCpy.VASP.VASPio import VASPInput
from CCpy.Tools.CCpyTools import selectInputs, selectVASPInputs, selectVASPOutputs, linux_command, bcolors
from pathlib import Path

version = sys.version
if version[0] == '3':
    raw_input = input


if len(sys.argv) <= 1:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [sub_option1] [sub_option2..]")
    print('''--------------------------------------
[options]
1   : Relaxation calculation  (from initial structure files)
2   : Band-DOS calculation    (after previous calculation)
3   : Band-DOS calculation    (from initial structure files)
add : User defined additional calculation from previous calculation


[sub_options]
ex) CCpyVASPInputGen.py 1 -isif=2 -spin -mag -kp=4,4,2 -vdw=D3damp, -pseudo=Nb_sv, -pot=LDA_54...

    < USE PRESET >
    -preset=[NAME] : [NAME].yaml in ~/.CCpy/vasp/

    < STRUCTURE OPTION >
    -refine_poscar : Use refined structure with space group (sym prec 0.1)

    < INCAR OPTION >
    -sp      : Single point calculation      (DEFAULT : NSW = 200)
    -isif=#  : ISIF value                    (DEFAULT : 3)
    -spin    : Spin polarized calculation    (DEFAULT : unpolarized)
    -mag     : Use magnetic monet parameters (values from config file)
    -ldau    : Use LDA+U parameters          (values from config file)

    van der Waals corrections                (DEFAULT : do not use)
    -vdw=D2     : DFT-D2 method of Grimme                   (VASP.5.2.11)
    -vdw=D3     : zero damping DFT-D3 method of Grimme      (VASP.5.3.4)
    -vdw=D3damp : DFT-D3 method with Becke-Jonson damping   (VASP.5.3.4)
    -vdw=dDsC   : dDsC dispersion correction method         (VASP.5.4.1)
    -vdw=optb88 : optb88 method
    -vdw=optb86b: optb86b method

    < KPOINTS OPTION >
    -kp=#,#,#                                (DEFAULT : reciprocal parameter as devided by 20)

    < POTCAR OPTION >
    -pot=PBE_54 : VASP potential setting     (DEFAULT : PBE_54)
                  Possible potentials = PBE, PBE_52, PBE_54, LDA, LDA_52, LDA_54, PW91, LDA_US, PW91_US
    -pseudo=    : Select pseudo potential    (DEFAULT : normal)
                  ex) -pseudo=Nb_sv,Ti_sv    --> will use 'Nb_sv, Ti_sv' pseudo potential to 'Nb, Ti'

    < ADDITIONAL CALCULATION >
    when use option 'add', 
    -dir=[DIRNAME]     : Additional calculation dir under previous run
    -pre_dir=[DIRNAME] : Previous directory name to copy CONTCAR, ... (default ./)
    -preset=[NAME]     : [NAME].yaml in ~/.CCpy/vasp/

    < SEQUENTIAL JOB >
    This method can be used with CCpyJobSubmit.py without vasp input generation.
    -sequence=[FILENAME] : sequence calculation based on presets and dirname in [FILENAME]
    [file example]
    default  ./
    static   ./static
    band     ./Band-DOS
 

[preset options]
~/.CCpy/vasp/___.yaml
    '''
          )
    VI = VASPInput()
    quit()


single_point = False
cell_fixe = False
vdw = False
spin = False
mag = False
ldau = False
isif = False
kpoints = False
pseudo = False
incar_preset = False
functional = "PBE_54"
ismear = 0
additional_dir = None
pre_dir = "./"
batch = False
refine_poscar = False

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
    elif "-ismear" in arg:
        ismear = arg.split("=")[1]
    elif "-preset" in arg:
        incar_preset = arg.split("=")[1]
        incar_preset = incar_preset + ".yaml"
    elif "-dir" in arg:
        additional_dir = arg.split("=")[1]
    elif "-pre_dir" in arg:
        pre_dir = arg.split("=")[1]
    elif "-batch" in arg:
        batch = True
    elif "-refine_poscar" in arg:
        refine_poscar = True
    

if sys.argv[1] == "1":
    input_marker = [".xsd", ".cif", "POSCAR", "CONTCAR"]
    if batch:
        inputs = selectInputs(input_marker, "./", ask=False)
    else:
        inputs = selectInputs(input_marker, "./")
    chk = True
    for each_input in inputs:
        VI = VASPInput(each_input, preset_yaml=incar_preset, refine_poscar=refine_poscar)

        # -- First process of edit INCAR, KPOINTS while create Inputs
        # -- If user want to use same options go to else
        if chk:
            VI.cms_vasp_set(single_point=single_point, isif=isif, vdw=vdw, kpoints=kpoints, spin=spin, mag=mag, ldau=ldau,
                            functional=functional, pseudo=pseudo,
                            get_pre_incar=None, batch=batch)
            if len(inputs) >= 2 and not batch:
                same_inputs = raw_input(bcolors.OKGREEN + "\n* Use this INCAR to others? (y/n)" + bcolors.ENDC)
                if same_inputs == "y":
                    chk = False
                else:
                    chk = True
        else:
            VI.cms_vasp_set(single_point=single_point,isif=isif,vdw=vdw,kpoints=kpoints,spin=spin,mag=mag,ldau=ldau,
                            pseudo=pseudo, functional=functional,
                            get_pre_incar=".prev_incar.yaml", batch=True)

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
        VI = VASPInput(additional_dir=True, dirname=each_input)
        VI.cms_band_set(input_line_kpts=input_line_kpts)
        os.chdir("../")


elif sys.argv[1] == "add":
    if not additional_dir:
        print("Additional input dirname must be assigned: -dir=")
        quit()
    elif not incar_preset:
        print("WARNING: preset option is not assigned: -preset=")

    inputs = selectVASPOutputs("./")
    for each_input in inputs:
        #print(each_input + "/" + additional_dir)
        VI = VASPInput(additional_dir=additional_dir, dirname=each_input, preset_yaml=incar_preset, refine_poscar=refine_poscar)
        VI.cms_vasp_set(single_point=single_point, isif=isif, vdw=vdw, kpoints=kpoints, spin=spin, mag=mag, ldau=ldau,
                        functional=functional, pseudo=pseudo,
                        get_pre_incar=None, batch=True, pre_dir=pre_dir)



elif sys.argv[1] == "MPRelax":
    input_marker = [".xsd", ".cif", "POSCAR", "CONTCAR"]
    inputs = selectInputs(input_marker, "./")
    if spin:
        ispin = 2
    else:
        ispin = 1
    user_incar = {"ISPIN": ispin, "LCHARG": "False", "ISMEAR": ismear}
    for each_input in inputs:
        MP_relax_VI = VASPInput(each_input)
        MP_relax_VI.MP_relax_set(user_incar=user_incar)

elif sys.argv[1] == "MITRelax":
    input_marker = [".xsd", ".cif", "POSCAR", "CONTCAR"]
    inputs = selectInputs(input_marker, "./")
    for each_input in inputs:
        MIT_relax_VI = VASPInput(each_input)
        MIT_relax_VI.MIT_relax_set()

elif sys.argv[1] == "MPHSEBand":
    input_marker = [".xsd", ".cif", "POSCAR", "CONTCAR"]
    inputs = selectInputs(input_marker, "./")
    for each_input in inputs:
        MP_HSE_VI = VASPInput(each_input)
        MP_HSE_VI.MP_HSE_band_set()






