#!/usr/local/bin/python2.7

import os, sys
from subprocess import call as shl
from CCpy.Qchem.Qchemio import QchemInput as QI
from CCpy.Tools.CCpyTools import selectInputs

###
# File collecting
###
try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option]")
    print('''--------------------------------------
[1] : Pair model
[2] : Cluster model
      [Option2] : Single molecule length, [Option3] : Starting number of center molecule'''
          )
    quit()

ask = True
if "a" in sys.argv:
    ask = False
input_marker = [".xsd", ".car", ".xyz"]
inputs = selectInputs(input_marker, "./", ask=ask)

if sys.argv[1] == "1":
    for each_input in inputs:
        myqi = QI(each_input)
        myqi.parsingStructure()
        myqi.pairInputGen()

elif sys.argv[1] == "2":
    try:
        single_molecule_len = int(sys.argv[2])
        center_number = int(sys.argv[3])
    except:
        single_molecule_len = input("Length of single molecule : ")
        center_number = input("Starting number of center molecule : ")
    for each_input in inputs:
        myqi = QI(each_input, single_molecule_len, center_number)
        myqi.parsingStructure()
        myqi.clusterInputGen()

else:
    print("Unvaild option.")
    quit()









        
