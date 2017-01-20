#!/usr/local/bin/python2.7

import os, sys
from subprocess import call as shl

from CCpy.Queue.CCpyJobControl import JobSubmit as JS
from CCpy.Tools.CCpyTools import selectInputs, selectVASPInputs
from CCpy.Tools.CCpyTools import linux_command as lc
from CCpy.Tools.CCpyTools import get_ip

try:
    chk = sys.argv[1]
    chk = sys.argv[2]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [queue name] [divide]")
    print('''--------------------------------------
[option]
1 : Gaussian09
2 : VASP
3 : ATK
4 : Q-chem
5 : ATAT [f:fitsvl]

[queue name]
xeon1, xeon2, ...

[divide] (optional)
be integer value, which divide CPU numbers and memories

[suboption]
a : no check files, calculate all inputs'''
          )
    quit()

ip = get_ip()
if ip != "166.104.249.249":
    print("DO AT NODE00 !!")
    quit()

ask = True
if "a" in sys.argv:
    ask = False
## ------ GAUSSIAN
if sys.argv[1] == "1":

    # --- PROPER QUEUE NAME CHECK
    queues = ["xeon1", "xeon2", "xeon3", "xeon4", "xeon5", "I5"]
    try:
        queue = sys.argv[2]
    except:
        queue = raw_input("Queue (xeon1, xeon2, ...) : ")
    if queue not in queues:
        print("Unvalid queue name")
        quit()

        
    # --- DEVIDE CPU CHECK 
    try:
        divided = int(sys.argv[3])
    except:
        divided = 1


    # --- COLLECT INPUT FILES     
    input_marker = [".com"]
    inputs = selectInputs(input_marker, "./", ask=ask)
    

    # --- SUBMIT QUEUE
    for each_input in inputs:
        myJS = JS(each_input, queue, divided)
        myJS.gaussian()

## ------ VASP
if sys.argv[1] == "2":
    
    # --- Collect VASP inputs
    static=False
    band=False
    if "static" in sys.argv:
        static=True
        inputs = selectVASPInputs("./", ask=ask, static=True)
    elif "band" in sys.argv:
        band=True
        inputs = selectVASPInputs("./", ask=ask, band=True)
    else:        
        inputs = selectVASPInputs("./", ask=ask)    


    # --- PROPER QUEUE NAME CHECK
    queues = ["xeon1", "xeon2", "xeon3", "xeon4", "xeon5", "I5"]
    try:
        queue = sys.argv[2]
    except:
        queue = raw_input("Queue (xeon1, xeon2, ...) : ")
    if queue not in queues:
        print("Unvalid queue name")
        quit()


        
    # --- DEVIDE CPU CHECK
    try:
        divided = int(sys.argv[3])
    except:
        divided = 1


    # --- SUBMIT QUEUE
    pwd = os.getcwd()
    for each_input in inputs:        
        os.chdir(pwd)
        myJS = JS(each_input, queue, divided)
        if static:
            myJS.vasp(static=True)
        elif band:
            myJS.vasp(band=True)
        else:
            myJS.vasp()        

### QCHEM ########################################################
elif sys.argv[1] == "4":

    #### COLLECT INPUT FILES #####################################    
    input_marker = [".in"]
    inputs = selectInputs(input_marker, "./", ask=ask)


    #### PROPER QUEUE NAME CHECK #################################
    queues = ["xeon1", "xeon2", "xeon3", "xeon4", "xeon5", "I5"]
    try:
        queue = sys.argv[2]
    except:
        queue = raw_input("Queue (xeon1, xeon2, ...) ? ")
    if queue not in queues:
        print("Unvalid queue name")
        quit()

        
    ### DIVIDE CPU CHECK #########################################
    try:
        divided = int(sys.argv[3])
    except:
        divided = 1
    

    ### SUBMIT QUEUE #############################################
    for each_input in inputs:
        myJS = JS(each_input, queue, divided)
        myJS.qchem()

### ATAT ########################################################
if sys.argv[1] == "5":
    #### COLLECT INPUT DIRECTORIES ##################################### 
    all_inputs = [int(d) for d in os.listdir("./") if os.path.isdir(d) if "str.out" in os.listdir(d)]
    all_inputs.sort()

    # fitsvl calc
    if "f" in sys.argv:
        inputs = []
        f = open("strname.in", "r")
        dirs = f.read()
        f.close()
        dirs = dirs.split()

        f = open("fitsvl_list", "w")
        pwd = os.getcwd()
        # strname.in dirs
        for d in dirs:
            os.chdir(d)
            sub_d = [a for a in os.listdir("./") if os.path.isdir(a) if "vol_" in a]
            # vol_0, vol_1.5 ...
            for sd in sub_d:
                os.chdir(sd)
                ssub_d = [a for a in os.listdir("./") if os.path.isdir(a) if "p+" in a]
                # p+0.2_8.7_0 ...
                for ssd in ssub_d:
                    os.chdir(ssd)
                    f.write(os.getcwd())
                    f.write("\n")
                    inputs.append(os.getcwd())
                    os.chdir("../")
                os.chdir("../")
            os.chdir("../")
        f.close()

    # maps calc
    else:
        print("0 : All files")
        for i in range(len(all_inputs)):
            if len(os.listdir(str(all_inputs[i])))==2:
                print(str(i+1) + " : " + str(all_inputs[i]))
            else:
                print(str(i+1) + " : " + str(all_inputs[i])+" , running or calculated.")
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
                        for j in range(int(r[0]),int(r[1])+1):
                            inputs.append(all_inputs[j-1])
                    else:                    
                        i = int(i)
                        inputs.append(all_inputs[i-1])
        except:
            print("Unvalid input type.")
            print("ex : 1-3,5-10,11,12,13")
            quit()    


    #### PROPER QUEUE NAME CHECK #################################
    queues = ["xeon1", "xeon2", "xeon3", "xeon4", "xeon5", "I5"]
    try:
        queue = sys.argv[2]
    except:
        queue = raw_input("Queue (xeon1, xeon2, ...) : ")
    if queue not in queues:
        print("Unvalid queue name")
        quit()


        
    ### DIVIDE CPU CHECK #########################################
    try:
        divided = int(sys.argv[3])
    except:
        divided = 1

    print(inputs)
    ### SUBMIT QUEUE #############################################
    pwd = os.getcwd()
    for each_input in inputs:
        os.chdir(pwd)
        myJS = JS(each_input, queue, divided)
        myJS.atat()        
