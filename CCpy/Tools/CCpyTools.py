import os, sys
import re
import glob
import json
import numpy as np
import pandas as pd
from collections import OrderedDict

from CCpy.Tools.CCpyStructure import NonPeriodicCoordinates
from CCpy.Tools.CCpyStructure import PeriodicStructure

version = sys.version
if version[0] == '3':
    raw_input = input

def lattice_strain(filename, sa=False, sb=False, sc=False, saa=False, sbb=False, scc=False):
    # --------- Parsing structure file ----------- #
    ps = PeriodicStructure(filename)
    if ".cif" in filename:
        ps.cifFile()
        name = filename.replace(".cif","")
    elif "POSCAR" in filename or "CONTCAR" in filename:
        ps.vaspFile()
        name = filename
    else:
        print("Invalid file format. Only *.cif and VASP POSCAR type available.")
        quit()
    ori = ps.latticeGen()        # [ori_a, ori_b, ori_c, ori_aa, ori_bb, ori_cc]

    # --------- Check strain parameters ---------- #
    items = [sa, sb, sc, saa, sbb, scc]
    for i in items:
        if i:
            if float(i[0]) % float(i[2]) != 0:
                print("Max and min strain value should be divided by interval.")
                print(str(i[0]) + "/" + str(i[2]) + " = 0")
                print(str(i[1]) + "/" + str(i[2]) + " = 0")
    key = ["a", "b", "c", "aa", "bb", "cc"]
    chg_index = []
    for i in range(len(items)):
        if items[i]:
            chg_index.append(i)

    # ------ Initialize lattice parameters ------- #
    # -- cell parameters
    param_vars = {"a":[ori[0]], "b":[ori[1]], "c":[ori[2]], "aa":[ori[3]], "bb":[ori[4]], "cc":[ori[5]]}
    # -- index for each parameters (strain %)
    param_vars_index = {"a":[0], "b":[0], "c":[0], "aa":[0], "bb":[0], "cc":[0]}

    # ----- add strained parameters to dict ----- #
    for i in chg_index:
        params = []
        indice = []
        strain = float(items[i][0])
        while strain <= float(items[i][1]):
            param = ori[i] * (100.0 + strain)/100
            params.append(param)
            indice.append(strain)
            strain = strain + float(items[i][2])
        param_vars[key[i]] = params
        param_vars_index[key[i]] = indice

    db = {'filename':[], 'a':[], 'b':[], 'c':[], 'alpha':[], 'beta':[], 'gamma':[],
          'strain_a':[], 'strain_b':[], 'strain_c':[], 'strain_alpha':[], 'strain_beta':[], 'strain_gamma':[]}
    cnt=0
    for i in range(len(param_vars["a"])):
        a = param_vars["a"][i]
        #a_index =  "__a"+str(param_vars_index["a"][i])
        for j in range(len(param_vars["b"])):
            b = param_vars["b"][j]
            #b_index = "__b"+str(param_vars_index["b"][j])
            for k in range(len(param_vars["c"])):
                c = param_vars["c"][k]
                #c_index = "__c" + str(param_vars_index["c"][k])
                for ii in range(len(param_vars["aa"])):
                    aa = param_vars["aa"][ii]
                    #aa_index = "__al" + str(param_vars_index["aa"][ii])
                    for jj in range(len(param_vars["bb"])):
                        bb = param_vars["bb"][jj]
                        #bb_index = "__be" + str(param_vars_index["bb"][jj])
                        for kk in range(len(param_vars["cc"])):
                            cc = param_vars["cc"][kk]
                            #cc_index = "__ga" + str(param_vars_index["cc"][kk])


                            tmp_index = "_%6d" % cnt  # indexing with integer
                            index_name = tmp_index.replace(" ", "0")
                            cnt+=1
                            # -- write cif file
                            lattice = [a,b,c,aa,bb,cc]
                            filename = name+index_name+".cif"
                            ps.cifWrite(lattice=lattice, filename=filename)

                            # -- add to db
                            db['filename'].append(filename)
                            db['a'].append(a)
                            db['b'].append(b)
                            db['c'].append(c)
                            db['alpha'].append(aa)
                            db['beta'].append(bb)
                            db['gamma'].append(cc)
                            db['strain_a'].append(param_vars_index["a"][i])
                            db['strain_b'].append(param_vars_index["b"][j])
                            db['strain_c'].append(param_vars_index["c"][k])
                            db['strain_alpha'].append(param_vars_index["aa"][ii])
                            db['strain_beta'].append(param_vars_index["bb"][jj])
                            db['strain_gamma'].append(param_vars_index["cc"][kk])
    df = pd.DataFrame(db)
    df = df[['filename','strain_a','strain_b','strain_c','strain_alpha','strain_beta','strain_gamma',
             'a', 'b', 'c', 'alpha', 'beta', 'gamma']]
    df.to_csv("00DB_"+name+".csv")



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
            if m in each_file and each_file not in all_inputs:
                all_inputs.append(each_file)

    # -- if ask == False : select all inputs
    if ask == True:
        for i in range(len(all_inputs)):
            print(str(i+1) + " : " + all_inputs[i])
        print("0 : All files")
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
    inputs.sort()
    return inputs

    
def selectVASPInputs(directory_path, dir_list=None, ask=True, static=False, band=False, phonon=False, sub=False):
    """
    Needs edition -> pick <INCAR POSCAR KPOINTS POTCAR> included directories
    """
    if dir_list:
        all_dirs = dir_list
    elif sub:
        def get_valid_paths(path):
            (parent, subdirs, files) = path
            if (not parent.endswith("/Band-DOS")) and (
               len(glob.glob(os.path.join(parent, "vasprun.xml*"))) > 0 or (
               len(glob.glob(os.path.join(parent, "POSCAR*"))) > 0 and
               len(glob.glob(os.path.join(parent, "INCAR*"))) > 0 and
               len(glob.glob(os.path.join(parent, "KPOINTS*"))) > 0 and
               len(glob.glob(os.path.join(parent, "POTCAR*"))) > 0)
           ):
                return [parent]
            return []
        all_dirs = []
        for (parent, subdirs, files) in os.walk(directory_path):
            all_dirs.extend(get_valid_paths((parent, subdirs,files)))

    else:
        all_dirs = [each_dir for each_dir in os.listdir(directory_path) if os.path.isdir(each_dir)]
    all_dirs.sort()
    all_inputs = []
    for each_dir in all_dirs:
        files = os.listdir(each_dir)
        if static:
            if "STATIC" in files:
                all_inputs.append(each_dir)
        elif band:
            if "Band-DOS" in files:
                all_inputs.append(each_dir)
        elif phonon:
            if "Phonon_opt" in files:
                all_inputs.append(each_dir)
        else:
            if "POSCAR" in files and "POTCAR" in files and "KPOINTS" in files and "INCAR" in files:
                all_inputs.append(each_dir)
    
    if ask == True:
        for i in range(len(all_inputs)):
            print(str(i+1) + " : " + all_inputs[i])
        print("0 : All files")
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

def selectVASPOutputs(directory_path, dir_list=None, ask=True, sub=False):
    if dir_list:
        all_dirs = dir_list
    elif sub:
        def get_valid_paths(path):
            (parent, subdirs, files) = path
            if "Band-DOS" in subdirs:
                return [parent]
            if (not parent.endswith("/Band-DOS")) and (
               len(glob.glob(os.path.join(parent, "vasprun.xml*"))) > 0 or (
               len(glob.glob(os.path.join(parent, "POSCAR*"))) > 0 and
               len(glob.glob(os.path.join(parent, "INCAR*"))) > 0 and
               len(glob.glob(os.path.join(parent, "KPOINTS*"))) > 0 and
               len(glob.glob(os.path.join(parent, "POTCAR*"))) > 0)
           ):
                return [parent]
            return []
        all_dirs = []
        for (parent, subdirs, files) in os.walk(directory_path):
            all_dirs.extend(get_valid_paths((parent, subdirs,files)))

    else:
        all_dirs = [each_dir for each_dir in os.listdir(directory_path) if os.path.isdir(each_dir)] 
    all_dirs.sort()
    all_inputs = all_dirs
    all_inputs = []
    for each_dir in all_dirs:
        files = os.listdir(each_dir)
        if "vasprun.xml" in files or "POSCAR" in files or "CONTCAR" in files or "OUTCAR" in files:
            all_inputs.append(each_dir)
    if ask == True:
        for i in range(len(all_inputs)):
            print(str(i+1) + " : " + all_inputs[i])
        print("0 : All files")
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


def selectVASPBandOutputs(directory_path, ask=True):
    all_dirs = [each_dir for each_dir in os.listdir(directory_path) if os.path.isdir(each_dir)]
    all_dirs.sort()
    all_inputs = []
    for each_dir in all_dirs:
        files = os.listdir(each_dir)
        if "Band-DOS" in files:
            all_inputs.append(each_dir)

    if ask == True:
        print("0 : All files")
        for i in range(len(all_inputs)):
            print(str(i + 1) + " : " + all_inputs[i])
        get_num = raw_input("Choose file : ")
    else:
        get_num = "0"

    inputs = []
    try:
        if get_num == "0":
            inputs = all_inputs
        else:
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

    if len(inputs) == 0:
        print("No available file detected.")
        quit()

    return inputs

def selectSIESTAInput(directory_path, dir_list=None, ask=True, sub=False):
    """
    Needs edition -> pick <INCAR POSCAR KPOINTS POTCAR> included directories
    """

    def get_valid_paths(path):
        (parent, subdirs, files) = path
        if len(glob.glob(os.path.join(parent, "*.fdf"))) > 0:
            parent = parent.replace("./", "")
            fdf_file = glob.glob(os.path.join(parent, "*.fdf"))
            return fdf_file
        return []

    all_dirs = []
    if dir_list:
        all_dirs = dir_list
    elif sub:
        for (parent, subdirs, files) in os.walk(directory_path):
            all_dirs.extend(get_valid_paths((parent, subdirs, files)))
    else:
        tmp_dirs = [each_dir for each_dir in os.listdir(directory_path) if os.path.isdir(each_dir)]
        for each_dir in tmp_dirs:
            all_dirs.extend(glob.glob(os.path.join(each_dir, "*.fdf")))

    all_dirs.sort()
    all_inputs = all_dirs

    inputs = input_chooser(all_inputs, ask=ask)
    if len(inputs) == 0:
        print("No available file detected.")
        quit()

    full_path_inputs = [os.getcwd() + "/" + each_input for each_input in inputs]


    return full_path_inputs

def input_chooser(all_inputs, ask=True):
    if ask == True:
        for i in range(len(all_inputs)):
            print(str(i + 1) + " : " + all_inputs[i])
        print("0 : All files")
        get_num = raw_input("Choose file : ")
    else:
        get_num = "0"

    inputs = []
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

    return inputs


def input_num_parser(len_input):
    """

    :param len_input: length of all inputs
    :return: indices of inputs
    """
    get_num = raw_input("Choose number (ex. a: all // 1-3,5-10,11,12)\n: ")
    input_indices = []
    try:
        if get_num == "a":
            input_indices = range(len_input)
        else:
            get_num = get_num.split(",")  # 1-4,6-10,11,12
            for i in get_num:
                if "-" in i:
                    r = i.split("-")
                    for j in range(int(r[0]), int(r[1]) + 1):
                        input_indices.append(j)
                else:
                    i = int(i)
                    input_indices.append(i)
    except:
        print("Unvalid input type.")
        print("ex : 1-3,5-10,11,12,13")
        quit()

    return input_indices


def find_convex_hull(points):
    """
    :param points: numpy array of [[1,2], [2,4], [3,5]]
    :return: dictionary
    """
    from scipy.spatial import ConvexHull
    hull = ConvexHull(points)
    hull_x = points[hull.vertices,0]
    hull_y = points[hull.vertices,1]
    hull_data = {'x':hull_x, 'y':hull_y}

    return hull_data

def file_writer(filename, string):
    f = open(filename, "w")
    f.write(string)
    f.close()

def save_json(dict, filename, indent=4):
    jstring = json.dumps(dict, indent=indent)
    f = open(filename, "w")
    f.write(jstring)
    f.close()

def load_json(filename, ordered=False):
    jstring = open(filename, "r").read()
    if ordered:
        dict = json.loads(jstring, object_pairs_hook=OrderedDict)
    else:
        dict = json.loads(jstring)

    return dict

def change_dict_key(ordered_dict, ori_key, new_key, new_val):
    dict = OrderedDict([(new_key, new_val) if k == ori_key else (k, v) for k, v in ordered_dict.items()])

    return dict

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

def progress_bar(total, crt, len_bar=50, cmt=None):
    proceeding = int(crt / total * len_bar)
    pgb_msg = "[ %s%s ]" % ("=" * proceeding, " " * (len_bar - proceeding))
    pgb_msg += " [ " + str(crt).rjust(len(str(total)) + 1) + " / " + str(total).rjust(len(str(total))) + " ] "      # rjust length --> 숫자 자릿수
    if cmt:
        pgb_msg += cmt
    sys.stdout.write(pgb_msg)
    sys.stdout.flush()
    sys.stdout.write("\b" * len(pgb_msg))
    sys.stdout.flush()
    
def formula_encoder(formula):
    SUB = str.maketrans("0123456789", "₀₁₂₃₄₅₆₇₈₉")
    #atoms = re.compile("[A-Z]{1}[a-z]*[0-9]*[.]*[0-9]*")
    atoms = re.compile("[A-Z]{1}.*")
    atoms = atoms.findall(formula)
    s = ""
    for a in atoms:
        s += a.translate(SUB)

    number_len = len(formula) - len(s)
    if number_len != 0:
        number = formula[:number_len]
    else:
        number = ""
    encoded_formula = number + s 

    return encoded_formula

def formula_encoder_mpl(formula):
    def to_subs(num):
        num = str(num)
        return '$_{%s}$' % num

    atoms = re.compile("[A-Z]{1}[a-z]?\d*[.]?\d*")
    atoms = atoms.findall(formula)

    s = ""
    raw_s = ""
    for a in atoms:     # atoms = ['Li4.5', 'Co2']
        raw_s += a
        specie = re.compile("[A-Z]{1}[a-z]?")
        specie = specie.findall(a)[0]
        num = a[len(specie):]
        sub_num = to_subs(num)

        s += specie + sub_num

    number_len = len(formula) - len(raw_s)
    if number_len != 0:
        number = formula[:number_len]
    else:
        number = ""
    encoded_formula = number + s

    return encoded_formula

def header_printer(msg, total_len=30):
    dec = total_len - len(msg)
    dec = int(dec / 2)
    dec1 = "\n# " + "-" * (dec) + " "
    if len(msg) % 2 == 1:
        dec += 1
    dec2 = " " + "-" * (dec) + " #"

    return dec1 + msg + dec2

def plt_deco(width=8, height=6.5):
    import matplotlib as mpl
    import matplotlib.pyplot as plt
    tick_labelsize = int(width * 2.5)
    axes_labelsize = width * 5        

    mpl.rcParams['axes.linewidth'] = width / 4
    mpl.rcParams['lines.markeredgewidth'] = width / 4
    mpl.rcParams['lines.linewidth'] = 1.5 
    mpl.rcParams['lines.markersize'] = 15

    mpl.rcParams['axes.labelsize'] = width * 4
    mpl.rcParams['xtick.labelsize'] = tick_labelsize
    mpl.rcParams['ytick.labelsize'] = tick_labelsize

    mpl.rcParams['xtick.major.width'] = width / 4
    mpl.rcParams['xtick.major.size'] = width

    mpl.rcParams['xtick.minor.width'] = width / 6
    mpl.rcParams['xtick.minor.size'] = width / 1.5

    mpl.rcParams['ytick.major.width'] = width / 4
    mpl.rcParams['ytick.major.size'] = width

    mpl.rcParams['ytick.minor.width'] = width / 6
    mpl.rcParams['ytick.minor.size'] = width / 1.5

    mpl.rcParams['lines.linewidth'] = width / 3
    mpl.rcParams['lines.markersize'] = width * 1.5

    mpl.rcParams['legend.fontsize'] = width * 3
    mpl.rcParams['legend.frameon'] = False

    mpl.rcParams["font.family"] = 'Arial'
    
    from cycler import cycler
    mpl.rcParams['axes.prop_cycle'] = cycler('color', ['#377eb8','#4daf4a','#e41a1c','#984ea3','#ff7f00', '#999999', '#a65628','#f781bf', '#17becf', '#bcbd22'])

    fig = plt.figure(figsize=(width, height), facecolor="w")

    return fig

def ssh_command(servername,portnum,username,password,msg):
    command = "sshpass -p \""+password+"\" ssh -p "+portnum+" "+username+"@"+servername+" \""+msg+"\""
    linux_command(command)

def ssh_send_file(servername,portnum,username,password,obj,dst):
    command = 'sshpass -p'+password+' scp -P '+portnum+' -o StrictHostKeyChecking=no '+obj+' '+username+'@'+servername+':'+dst
    linux_command(command)

def ssh_send_directory(servername,portnum,username,password,obj,dst):
    command = 'sshpass -p'+password+' scp -P '+portnum+' -r -o StrictHostKeyChecking=no '+obj+' '+username+'@'+servername+':'+dst
    linux_command(command)
