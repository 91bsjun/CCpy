#!/home/shared/anaconda3/envs/CCpy_tmp/bin/python

import os, sys
from CCpy.ATAT.ATATio import ATATInput
from CCpy.Tools.CCpyTools import selectInputs, linux_command

import time

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [sub_option1] [sub_option2..]")
    print('''--------------------------------------
[options]
0 : Make vasp.wrap (VASP INCAR template)
1 : Generate ATAT input file (lat.in)
2 : Generate configurations
3 : Generate VASP inputfiles (after [2])
4 : Add LDA+U, MAGMOM parameters (after [3])'''
          )
    quit()

if sys.argv[1] == "0":
    AI = ATATInput()
    AI.vasp_wrap_template()

if sys.argv[1] == "1":
    input_marker = [".xsd", ".cif", "POSCAR", "CONTCAR"]
    inputs = selectInputs(input_marker, "./")
    for each_input in inputs:
        AI = ATATInput(each_input)
        AI.inputGen()

elif sys.argv[1] == "2":
    try:
        number_of_set = int(sys.argv[2])
    except:
        number_of_set = input("How many structures to make? ")

    
    for i in range(number_of_set):
        dirs = [d for d in os.listdir("./") if os.path.isdir(d)]
        linux_command("touch ready")
        new_dir = []
        while len(new_dir) != 1:
            new_dir = [d for d in os.listdir("./") if os.path.isdir(d) if d not in dirs]
        print("Generated : "+new_dir[0])

elif sys.argv[1] == "3":
    all_inputs = [int(d) for d in os.listdir("./") if os.path.isdir(d) if
                  "str.out" in os.listdir(d) and "wait" in os.listdir(d)]
    all_inputs.sort()

    print("0 : All files")
    for i in range(len(all_inputs)):
        print(str(i + 1) + " : " + str(all_inputs[i]))
    get_num = raw_input("Choose file : ")
    all_inputs = [str(d) for d in all_inputs]
    try:
        if get_num == "0":
            inputs = all_inputs
        else:
            inputs = []
            get_num = get_num.split(",")  # 1-4,6-10,11,12
            for i in get_num:
                if "-" in i:
                    r = i.split("-")
                    for j in range(int(r[0]), int(r[1]) + 1):
                        inputs.append(all_inputs[j - 1])
                else:
                    i = int(i)
                    inputs.append(all_inputs[i - 1])
    except:
        print("Unvalid input type.")
        print("ex : 1-3,5-10,11,12,13")
        quit()

    for d in inputs:
        os.chdir(d)
        print(d)
        linux_command("runstruct_vasp -nr")
        os.chdir("../")
        
elif sys.argv[1] == "4":
    all_inputs = [int(d) for d in os.listdir("./") if os.path.isdir(d) if "atomlabel.tmp" in os.listdir(d)]
    if len(all_inputs) == 0:
        print("No available directory detected.. ")
        quit()
    all_inputs.sort()

    print("0 : All files")
    for i in range(len(all_inputs)):
        print(str(i + 1) + " : " + str(all_inputs[i]))
    get_num = raw_input("Choose file : ")
    all_inputs = [str(d) for d in all_inputs]
    try:
        if get_num == "0":
            inputs = all_inputs
        else:
            inputs = []
            get_num = get_num.split(",")  # 1-4,6-10,11,12
            for i in get_num:
                if "-" in i:
                    r = i.split("-")
                    for j in range(int(r[0]), int(r[1]) + 1):
                        inputs.append(all_inputs[j - 1])
                else:
                    i = int(i)
                    inputs.append(all_inputs[i - 1])
    except:
        print("Unvalid input type.")
        print("ex : 1-3,5-10,11,12,13")
        quit()

    for d in inputs:
        os.chdir(d)
        AI = ATATInput()
        AI.add_ldau()
        os.chdir("../")