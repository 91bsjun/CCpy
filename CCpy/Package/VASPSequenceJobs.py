import os, sys
from collections import OrderedDict

from CCpy.VASP.VASPio import VASPInput
from CCpy.Tools.CCpyTools import selectInputs, selectVASPInputs, selectVASPOutputs, linux_command, bcolors
from pathlib import Path

input_filename = sys.argv[1]
sequence_file = sys.argv[2]
python_path = sys.argv[3]
script_path = sys.argv[4]
refine_poscar = sys.argv[5]

presets = []
dirnames = []
for l in open(sequence_file, 'r').readlines():
    if len(l.split()) >= 1:
        presets.append(l.split()[0] + '.yaml')
        dirnames.append(l.split()[1])

run = f"{python_path} {script_path}"
pwd = os.getcwd()
for i in range(len(presets)):
    if i == 0:
        VI = VASPInput(filename=input_filename, preset_yaml=presets[i], refine_poscar=refine_poscar)
        VI.cms_vasp_set(batch=True)
        dirname = VI.dirname
        os.chdir(dirname)
        os.system(run)
        os.system('touch vasp.done')
        os.chdir(pwd)
        
    else:
        pre_dir = dirnames[i-1]
        VI = VASPInput(additional_dir=dirnames[i], dirname=dirname, preset_yaml=presets[i], refine_poscar=refine_poscar)
        VI.cms_vasp_set(batch=True, pre_dir=pre_dir)
        os.chdir(dirname + '/' + dirnames[i])
        os.system(run)
        os.system('touch vasp.done')
        os.chdir(pwd)
