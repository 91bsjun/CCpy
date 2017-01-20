#!/usr/local/bin/python2.7

import os, sys
from CCpy.VASP.VASPio import VASPOutput
from CCpy.Tools.CCpyTools import selectVASPOutputs

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [sub_option1] [sub_option2..]")
    print('''--------------------------------------
[options]
1 : Get final structure (or initial strcture)
2 : Energy & Cell volume convergence plot'''
          )
    quit()

if sys.argv[1] == "1":
    inputs = selectVASPOutputs("./")
    for each_input in inputs:
        os.chdir(each_input)
        VO = VASPOutput()
        VO.getFinalStructure()
        os.chdir("../")

elif sys.argv[1] == "2":
    inputs = selectVASPOutputs("./")
    for each_input in inputs:
        os.chdir(each_input)
        VO = VASPOutput()
        VO.getConvergence()
        os.chdir("../")
