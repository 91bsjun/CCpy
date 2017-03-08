#!/usr/local/bin/python2.7

import os, sys
from subprocess import call as shl

from CCpy.Gaussian.Gaussianio import GaussianInput as GI
from CCpy.Gaussian.Gaussianio import GaussianOutput as GO
from CCpy.Tools.CCpyStructure import NonPeriodicCoordinates as npc
from CCpy.Tools.CCpyTools import selectInputs

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [suboption]")
    print('''--------------------------------------
[1] : From structure files (*.xyz, *.car, *.xsd)
[2] : From log files (*.log)
[3] : From checkpoint files (in ./check directory) (*.chk)
[4] : Edit com files (*.com)
[suboption]
a : no check files, calculate all inputs'''
          )
    quit()




ask = True
if "a" in sys.argv:
    ask = False

if sys.argv[1] == "1":
    input_marker = [".xsd", ".xyz", "car"]
    inputs = selectInputs(input_marker, "./", ask=ask)
elif sys.argv[1] == "2":
    input_marker = [".log"]
    inputs = selectInputs(input_marker, "./")
elif sys.argv[1] == "3":
    input_marker = [".chk"]
    inputs = selectInputs(input_marker, "./check")
elif sys.argv[1] == "4":
    input_marker = [".com"]
    inputs = selectInputs(input_marker, "./")

# -- option preset
options = {"nproc":24, "mem":64, "functional":"B3LYP", "basis":"6-31G",
           "chg":0, "multi":1,
           "options":["gfinput","gfprint","SCF(maxcycle=512,conver=6)","pop=full","iop(3/33=1,3/36=-1)","nosym","opt=gediis"],
           "options_under_coordinates":""}

# ------ basic option edit ------ #
print("\n\n------------- Preset of basic options -------------")
for key in options.keys():
    if "options" not in str(key):
        print(str(key) + "="+str(options[key]))

get_sets = raw_input("* Anything want to modify ? if not, enter \"n\". \nif you have (ex: basis=gen,functional=Cam-B3LYP) \n: ")
if get_sets != "n":
    vals = get_sets.replace(" ","")
    vals = vals.split(",")
    for val in vals:
        key = val.split("=")[0]
        if key not in options.keys():
            print("You seem to have misspelled.")
            print(key +" is not in our options.")

        value = val.split("=")[1]
        options[key] = value

# ------ calc option edit ------ #
print("\n\n-------------- Calculation options ---------------")
ex = "Example items : "
for o in options["options"]:
    ex += o +" "
ex+="\n"
print(ex)
get_options = raw_input("Enter options (ex: gfinput gfprint nosym opt=gediis)\n:")
get_options = get_options.replace(" ","")
get_options = get_options.split(",")
options["options"] = get_options

# ------ bottom option edit ------ #
print("\n\n--------- Options under coordinates area ---------")
print("Enter \"n\" when you finished.")
print("""(example)
line:I 0
line:SDD
line:****
line:C H N 0
line:6-31G*
line:****
line:
line:I 0
line:SDD
""")

get_options = ""
line_option = ""
while line_option != "n":
    line_option = raw_input("line: ")
    if line_option != "n":
        get_options += line_option+"\n"
options["options_under_coordinates"] = get_options

myGI = GI(nproc=options['nproc'], mem=options['mem'],
          functional=options['functional'], basis=options['basis'],
          chg=options['chg'], multi=options['multi'],
          options=options['options'], options2=options['options_under_coordinates'])

try:
    os.mkdir("check")
except:
    pass
for each_input in inputs:
    if sys.argv[1] == "1":
        myGI.newCalc(each_input)
    elif sys.argv[1] == "2":
        mygo = GO(each_input)
        n_atoms, atoms, coords = mygo.getFinalStructure()
        name = each_input.replace(".log","")
        if name+".xyz" in os.listdir("./"):
            name = name+"[2]"

        mynpc = npc("tmp")
        mynpc.name, mynpc.n_atoms, mynpc.atoms, mynpc.coords = name, n_atoms, atoms, coords
        mynpc.to_xyzFile()

        myGI.newCalc(name+".xyz")
        shl("rm -rf "+name+".xyz", shell=True)
        
    elif sys.argv[1] == "3":
        newname = each_input.replace(".chk","_a")
        myGI.additionalCalc(each_input, comname=newname)

    elif sys.argv[1] == "4":
        f = open(each_input, "r")
        lines = f.readlines()
        f. close()

        f.write(each_input, "w")
        for line in lines:
            if "%nproc=" in line:
                f.write("%nproc="+str(nproc)+"\n")
            elif "%mem=" in line:
                f.write("%mem="+str(mem)+"Gb\n")
            elif "#p" in line:
                f.write("#p "+functional+"/"+basis+" gfinput gfprint SCF(maxcycle=512,conver=6) "+options+"\n")
            else:
                f.write(line)
        f.close()
        print(each_input)
            

