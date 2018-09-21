#!/home/shared/anaconda3/envs/CCpy_tmp/bin/python

import os, sys
from subprocess import call as shl

from CCpy.CASM.CASMio import CASMInput
from CCpy.Tools.CCpyTools import selectInputs

###
# File collecting
###
try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option]")
    print('''--------------------------------------
[1] : prim.json Gen'''
          )
    quit()

input_marker = [".xsd", ".cif", "POSCAR", "CONTCAR"]
inputs = selectInputs(input_marker, "./")

for each_input in inputs:
    if sys.argv[1] == "1":
        CI = CASMInput(each_input)
        CI.primGen()
