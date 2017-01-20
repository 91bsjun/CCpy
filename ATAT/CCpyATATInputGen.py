#!/usr/local/bin/python2.7

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
1 : Generate ATAT input file (lat.in)
2 : Generate configurations
3 : Background qdel finished job'''
          )
    quit()

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
        number_of_set = input("How many sets to make? ")

    
    for i in range(number_of_set):
        dirs = [d for d in os.listdir("./") if os.path.isdir(d)]
        linux_command("touch ready")
        new_dir = []
        while len(new_dir) != 1:
            new_dir = [d for d in os.listdir("./") if os.path.isdir(d) if d not in dirs]
        print("Generated : "+new_dir[0])

elif sys.argv[1] == "3":
    while True:
        linux_command("bsjunJobDelete.py 3")
        linux_command("bsjunJobDelete.py 3 f")
        time.sleep(60)
        
elif sys.argv[1] == "4":
    dirs = [d for d in os.listdir("./") if os.path.isdir(d) if "str.out" in os.listdir(d)]
    try:
        os.mkdir("NOT_1.1.1")
    except:
        pass
    dirs.sort()
    for d in dirs:
        f = open(d+"/str.out","r")
        lines = f.readlines()
        f.close()
        n=0
        m=0
        c=0
        for l in lines:
            if "Ni" in l:
                n+=1
            elif "Mn" in l:
                m+=1
            elif "Co" in l:
                c+=1
        if not n == m == c == 3:
            print("mv "+d+" NOT_1.1.1")

elif sys.argv[1] == "3":
    while True:
        linux_command("bsjunJobDelete.py 3")
        time.sleep(30)
        
elif sys.argv[1] == "5":
    elements = ["Ni","Mn","Co"]
    sets = []
    
    










    
