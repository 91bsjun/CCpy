#!/usr/local/bin/python2.7

import os, sys
import json
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

version = sys.version
if version[0] == '3':
    raw_input = input

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


home = os.getenv("HOME")
if ".CCpy" not in os.listdir(home):
    os.mkdir(home+"/.CCpy")
configs = os.listdir(home+"/.CCpy")
if "g09_input.json" in configs:
    jstring = open(home + "/.CCpy/g09_input.json", "r").read()
    options = json.loads(jstring)
else:
    # -- option preset
    options = {"nproc":24, "mem":64, "functional":"B3LYP", "basis":"6-31G*",
               "chg":0, "multi":1,
               "options":"gfinput gfprint SCF(maxcycle=512,conver=6) opt=gediis freq=noraman",
               "options_under_coordinates":""}

# ------ basic option edit ------ #
print("\n\n------------- Preset of basic options -------------")
for key in options.keys():
    if "options" not in str(key):
        print(str(key) + "="+str(options[key]))

get_sets = raw_input("\n* Anything want to modify ? if not, enter \"n\". \nif you have (ex: basis=gen,functional=Cam-B3LYP) \n: ")
if get_sets != "n":
    vals = get_sets.replace(" ","")
    vals = vals.split(",")
    for val in vals:
        key = val.split("=")[0]
        if key not in options.keys():
            print("You seem to have misspelled.")
            print(key +" is not in our options.")
            quit()

        value = val.split("=")[1]
        options[key] = value

# ------ calc option edit ------ #
print("\n\n-------------- Calculation options ---------------")
print("Current option : " + options["options"])
print("Option blocks : gfinput gfprint SCF(maxcycle=512,conver=6) opt=gediis freq=noraman pop=full iop(3/33=1,3/36=-1) pseudo=read EmpiricalDispersion=GD3 nosym sp")
get_options = raw_input("Enter options to modify (ex: gfinput gfprint nosym opt=gediis) if not, enter \"p\" \n :")
if get_options != "p":
    options["options"] = get_options

# ------ bottom option edit ------ #
print("\n\n--------- Options under coordinates area ---------")
if len(options["options_under_coordinates"]) < 2:
    print("Current option : Empty")
else:
    print("Current option : \n" + options["options_under_coordinates"])
print("If you want to use current option, enter \"p\". \n Or fill line.")
print("Enter \"n\" when you finished.")
print("""(example)
line: I 0
line: SDD
line: ****
line: C H N 0
line: 6-31G*
line: ****
line:
line: I 0
line: SDD
""")

get_options = ""
line_option = ""
while line_option != "n" and line_option != "p":
    line_option = raw_input("line: ")
    if line_option == "p":
        get_options = options["options_under_coordinates"]
        break
    if line_option != "n":
        get_options += line_option+"\n"
options["options_under_coordinates"] = get_options
chk = raw_input("Do you want to update preset option? (y/n)")
if chk == "y":
    jstring = json.dumps(options, indent=4)
    f = open(home + "/.CCpy/g09_input.json", "w")
    f.write(jstring)
    f.close()
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
            

