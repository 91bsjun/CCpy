import os, sys

import numpy as np
from CCpy.Tools.CCpyStructure import NonPeriodicCoordinates

def coord_shift(filename, atom_range, axis, delta_range, negative=False):
    """
    Function for shift coordinate in non-periodic structure
    
    Args
        filename : structure filename (*.xyz, *.car, *.xsd available)
        atom_range : atom number list of what you want to shift range(11,21)
        axis : shift axis ("x" or "y" or "z")
        delta_range : list of shift length values [0.1, 0.2, 0.3] (better : np.linspace(0, 5, 5))
        negative : if you want to negative shift also.
        
    Info about file naming
        if you want to file indexing by shifted value   -> remove annotation of "# index with length"
                                        indexing number -> remove annotation of "# indexing with integer"

    Usage example:
        import os, sys
        import numpy as np
        from subprocess import call as shl

        from bsjunCODE.bsjunStructure import NonPeriodicCoordinates as npc
        from bsjunCODE.bsjunTools import coord_shift

        shift = np.linspace(0, 7.1, 50)

        files = [car for car in os.listdir("./") if ".xyz" in car]
        files.sort()
        cnt=0
        for filename in files:
            tmp = "%2d"%cnt
            tmp = tmp.replace(" ","0")
            os.mkdir(tmp)
            os.chdir(tmp)
            shl("mv ../"+filename+" ./", shell=True)
            
            coord_shift(filename, range(0,40), "x", shift, negative=False)
            shl("mv "+filename+" ../structures", shell=True)
            
            os.chdir("../")
            cnt+=1
    """

    # -- Parsing axis to coordinate's index
    if axis == "x":
        axis = 0
        name = "tx"
    elif axis == "y":
        axis = 1
        name = "ty"
    elif axis == "z":
        axis = 2
        name = "tz"
    else:
        print("Wrong axis name.")
        quit()

    """
    naming
    uncomment # index with length : file names are generated as shifted length
    uncomment # index with integer : file names are generated as index order
    """
    shift = delta_range
    cnt = 0   # indexing with integer
    for i in shift:
        mynpc = NonPeriodicCoordinates(filename)
        if ".car" in filename:            
            mynpc.carFile()
        elif ".xsd" in filename:
            mynpc.xsdFile()
        elif ".xyz" in filename:
            mynpc.xyzFile()
        coordinates = np.array(mynpc.coords)

        # molecule range (atom number - 1 = coordinate index)
        for j in atom_range:
            coordinates[j][axis] = coordinates[j][axis]+i    

        mynpc.coords = coordinates
        pre_name = mynpc.name
        #mynpc.name = pre_name+name+"%.2f"%i  # index with length
        tmp = pre_name+"_%2d"%cnt  # indexing with integer
        tmp = tmp.replace(" ","0")  # indexing with integer
        mynpc.name = tmp  # indexing with integer
        mynpc.to_xyzFile()
        cnt+=1

    cnt = 0    # indexing with integer
    if negative:
        for i in shift:
            mynpc = NonPeriodicCoordinates(filename)
            if ".car" in filename:            
                mynpc.carFile()
            elif ".xsd" in filename:
                mynpc.xsdFile()
            elif ".xyz" in filename:
                mynpc.xyzFile()
            coordinates = np.array(mynpc.coords)
            
            for j in atom_range:
                coordinates[j][axis] = coordinates[j][axis]-i    

            mynpc.coords = coordinates
            pre_name = mynpc.name
            #mynpc.name = pre_name+name+"n%.2f"%i  # index with length
            tmp = pre_name+"_n%2d"%cnt  # indexing with integer
            tmp = tmp.replace(" ","0")  # indexing with integer
            mynpc.name = tmp  # indexing with integer
            mynpc.to_xyzFile()
            cnt+=1 # indexing with integer

def selectInputs(marker, directory_path, ask=True):
    """
    Args
        marker : what the string of included in filename [".xyz",".xsd","POSCAR"]
    Usage example
        from bsjunTools import selectInputs
        
        input_marker = [".xsd", ".car", ".xyz"]
        inputs = selectInputs(input_marker, "./")
    """
    all_files = os.listdir(directory_path)
    all_files.sort()
    all_inputs = []
    for each_file in all_files:
        for m in marker:
            if m in each_file:
                all_inputs.append(each_file)

    # -- if ask == False : select all inputs
    if ask == True:
        print("0 : All files")
        for i in range(len(all_inputs)):
            print(str(i+1) + " : " + all_inputs[i])
        get_num = raw_input("Choose file : ")
    else:
        get_num = "0"
        
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

    if len(inputs) == 0:
        print("No available file detected.")
        quit()
    return inputs

    
def selectVASPInputs(directory_path, ask=True, static=False, band=False):
    """
    Needs edition -> pick <INCAR POSCAR KPOINTS POTCAR> included directories
    """
    all_dirs = [each_dir for each_dir in os.listdir(directory_path) if os.path.isdir(each_dir)]
    all_inputs = []
    for each_dir in all_dirs:
        files = os.listdir(each_dir)
        if static:
            if "STATIC" in files:
                all_inputs.append(each_dir)
        elif band:
            if "Band-DOS" in files and "PreCalc" in files:
                all_inputs.append(each_dir)
        else:
            if "POSCAR" in files and "POTCAR" in files and "KPOINTS" in files and "INCAR" in files:
                all_inputs.append(each_dir)
    
    if ask == True:
        print("0 : All files")
        for i in range(len(all_inputs)):
            print(str(i+1) + " : " + all_inputs[i])
        get_num = raw_input("Choose file : ")
    else:
        get_num = "0"
        
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

    if len(inputs) == 0:
        print("No available file detected.")
        quit()

    return inputs

def selectVASPOutputs(directory_path, ask=True):
    """
    Needs edition -> pick <INCAR POSCAR KPOINTS POTCAR> included directories
    """
    all_dirs = [each_dir for each_dir in os.listdir(directory_path) if os.path.isdir(each_dir)]
    all_inputs = []
    for each_dir in all_dirs:
        files = os.listdir(each_dir)
        if "CONTCAR" in files:
            all_inputs.append(each_dir)
    
    if ask == True:
        print("0 : All files")
        for i in range(len(all_inputs)):
            print(str(i+1) + " : " + all_inputs[i])
        get_num = raw_input("Choose file : ")
    else:
        get_num = "0"
        
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

    if len(inputs) == 0:
        print("No available file detected.")
        quit()

    return inputs

def file_writer(filename, string):
    f = open(filename, "w")
    f.write(string)
    f.close()
 
def linux_command(string):
    from subprocess import call
    call(string, shell=True)

def get_ip():
    ifconfig = os.popen("ifconfig")
    for l in ifconfig.readlines():
        if "inet addr" in l:
            ip = l.split()[1]
            ip = ip.split(":")[1]
            return ip
                
            

def ssh_command(servername,portnum,msg):
    command = "sshpass ssh -p %d %s \"%s\""%(portnum,servername,msg)
    linux_command(command)

def ssh_send_file(servername,portnum,username,password,obj,dst):
    command = 'sshpass -p'+password+' scp -P '+portnum+' -o StrictHostKeyChecking=no '+obj+' '+username+'@'+servername+':'+dst
    linux_command(command)

def ssh_send_directory(servername,portnum,username,password,obj,dst):
    command = 'sshpass -p'+password+' scp -P '+portnum+' -r -o StrictHostKeyChecking=no '+obj+' '+username+'@'+servername+':'+dst
    linux_command(command)
