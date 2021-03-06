#!/usr/local/bin/python2.7
import sys,os
from CCpy.Tools.CCpyTools import lattice_strain
from CCpy.Tools.CCpyTools import selectInputs

def main_run(filename=None):
    sa, sb, sc, saa, sbb, scc, cleaning = False, False, False, False, False, False, False
    for arg in sys.argv:
        if "a=" in arg and "gamma" not in arg:
            parse = arg.replace("a=","").replace(" ","")
            parse = parse.split(",")
            sa = [float(parse[0]),float(parse[1]),float(parse[2])]
        if "b=" in arg:
            parse = arg.replace("b=","").replace(" ","")
            parse = parse.split(",")
            sb = [float(parse[0]),float(parse[1]),float(parse[2])]
        if "c=" in arg:
            parse = arg.replace("c=","").replace(" ","")
            parse = parse.split(",")
            sc = [float(parse[0]),float(parse[1]),float(parse[2])]
        if "alpha=" in arg:
            parse = arg.replace("alpha=","").replace(" ","")
            parse = parse.split(",")
            saa = [float(parse[0]),float(parse[1]),float(parse[2])]
        if "beta=" in arg:
            parse = arg.replace("beta=","").replace(" ","")
            parse = parse.split(",")
            sbb = [float(parse[0]),float(parse[1]),float(parse[2])]
        if "gamma=" in arg:
            parse = arg.replace("gamma=","").replace(" ","")
            parse = parse.split(",")
            scc = [float(parse[0]),float(parse[1]),float(parse[2])]
        if "NN" in arg:
            print("you select NN type")
            cleaning = True
    lattice_strain(filename, sa=sa, sb=sb, sc=sc, saa=saa, sbb=sbb, scc=scc)
    if cleaning:
        os.system("NNDB.py")

if __name__=="__main__":
    try:
        sys.argv[1]
    except:
        print("""-------------------------------------
Usage : CCpyLatticeStrain.py [option1] [option2] ...
-------------------------------------
[option]
a=-15,15,5
  ==> lattice parameter `a` to -15%, -10%, -5%, ... , 10%, 15%
b=-4,4,2
  ==> lattice parameter `b` to -4%, -2%, 0%, 2%, 4%
gamma=-9,9,3
  ==> lattice parameter `gamma` to -9%, -6%, ... , 6%, 9%

[example]
CCpyLatticeStrain.py a=-15,15,5 b=-15,15,5 gamma=-9,9,3
-------------------------------------"""
              )
        quit()

    input_marker = [".cif", "POSCAR", "CONTCAR"]
    inputs = selectInputs(input_marker, "./")
    for i in inputs:
        main_run(i)
