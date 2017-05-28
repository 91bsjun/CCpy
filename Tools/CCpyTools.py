import os, sys

import numpy as np
import pandas as pd
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
    inputs.sort()
    return inputs

    
def selectVASPInputs(directory_path, ask=True, static=False, band=False):
    """
    Needs edition -> pick <INCAR POSCAR KPOINTS POTCAR> included directories
    """
    all_dirs = [each_dir for each_dir in os.listdir(directory_path) if os.path.isdir(each_dir)]
    all_dirs.sort()
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
    all_dirs = [each_dir for each_dir in os.listdir(directory_path) if os.path.isdir(each_dir)]
    all_dirs.sort()
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

    if len(inputs) == 0:
        print("No available file detected.")
        quit()

    return inputs

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

def vasp_incar_json():
    jstring = """{
    "SYSTEM":"filename",

    "#1 ":"Startparameter for this Run:",
    "NWRITE":"2                   ! LPETIM=F    write-flag & timer",
    "ISTART":"0                   ! job   : 0-new  1-contEcut  2-sameBS",
    "INIWAV":"1                   ! 0-jellium  1-random",
    "IWAVPR":"1                   ! prediction:  0-non 1-charg 2-wave 3-comb",
    "ICHARG":"2                   ! 0-from WF  1-from CHGCAR  2-from atom  11-12-fixed",
    "LWAVE": ".FALSE.             ! determines whether the wavefunctions are written to the WAVECAR file",

    "#2 ":"Electronic Relaxation 1",
    "NELM":"100                   ! number of iterations",
    "EDIFF":"1E-04                ! stopping-criterion for ELM",
    "BMIX":"3.00                  ! sets the cutoff wave vector for Kerker mixing for the magnetization density",
    "ENCUT":"500                  ! Cut-Off Energy",

    "#3 ":"Electronic Relaxation 1",
    "# ALGO":"48                  ! algorithm for the e-relax",
    "LDIAG":"T                    ! sub-space diagonalisation",
    "LREAL":"auto                 ! real-space projection",
    "PREC":"med                   ! accuracy",
    "# NBANDS  ":"30              ! number of bands for diagonalization",

    "#4 ":"Ionic Relaxation",
    "NSW":"200                    ! number of steps for IOM",
    "NBLOCK":"1                   ! inner block",
    "KBLOCK":"10                  ! outer block",
    "IBRION":"2                   ! ionic relax: 0-MD 1-quasi-New 2-CG",
    "ISIF":"3                     ! ion&cell relax: 0-MD 2-ion&stress 3-ion&cell&stress",
    "ISYM":"2                     ! switch symmetry stuff ON (1 or 2) or OFF (0)",
    "# SYMPREC ":" 1e-6",
    "LCORR":"T                    ! Harris-correction to forces",
    "EDIFFG":"-0.04               ! Criterion for geom opt (eV/Ang)",
    "POTIM":"0.50                 ! time-step for ionic motion (fs)",
    "SMASS":"3.00                 ! Nose mass-parameter (am)",

    "#5 DOS":" related values",
    "ISMEAR":"0                   ! Broadening methode -5-tet -1-fermi 0-gaus 1-mp 2-mp2",
    "SIGMA":"0.05                 ! Broadening in eV",
    "LORBIT":"11                  ! l-decomposed DOS",
    "# RWIGS":"1.63  1.00         ! Wigner-Zeits radius",
    "# EMIN":"                    ! Minimum energy for DOS",
    "# EMAX":"                    ! Maximum energy for DOS",
    "# NEDOS":"1001               ! Number of DOS points",
    "# NELECT":"100               ! Total number of electrons",
    "# NUPDOWN":"2                ! Difference between UP&DOWN electrons",

    "#6 Parallelization":"option",
    "LPLANE":"T                   ! Parallelization for PWs",
    "NCORE":"8",
    "LSCALU":"F",
    "NSIM":"4",

    "ISPIN":"1                    ! spin polarized = 2, non spin polarized = 1",
    "# MAGMOM":" 8*0 10*4.5 8*4.5 ! initial magnetic moment for the atoms in the cell",

    "#7 optB86b-vdW functional requires":" vdw_kernel.bindat",
    "# GGA":"MK",
    "# PARAM1":"0.1234",
    "# PARAM2":"1.0000",
    "# LUSE_VDW":".TRUE.",
    "# AGGAC":"0.0000",

    "#8 TS calculation":"         ! default:Nudged Elestic Band method",
    "# ICHAIN":"0                 ! Method (0=NEB, 1=Dynamical matrix, 2=Dimer, 3=Lanczos)",
    "# SPRING":"-5                ! in eV/Ang*2 (sping constant)",
    "# IMAGES":"3                 ! Number of images btw Reactant & Product",
    "# LCLIMB":".true.            ! cNEB: driven up to the saddle point",
    "# LTANGENTOLD":".true.       ! Old central difference tangent",
    "# LDNEB":".true.             ! Modified doubble nudging",
    "# NEBCELL":".true.           ! NEB for variable cell (w/ ISIF=3)",

    "#9 Dipole Correction":"option",
    "# IDIPOL":"3",
    "# LDIPOL":"",

    "#10 lda+u":"parameters",
    "# LMAXMIX":"4",
    "# LDAU":".TRUE.              ! or .FALSE.",
    "# LDAUTYPE":"1               ! or 2",
    "# LDAUL":"2 2 2              ! l-quantum number on which U acts ((1._q,0._q) for each type",
    "# LDAUU":"0 5.00 7.0         ! U coefficient (coulomb interaction) for each species",
    "# LDAUJ":"0 1 1              ! J coefficient (exchange) for each species"
}
"""
    return jstring

def ssh_command(servername,portnum,username,password,msg):
    command = "sshpass -p \""+password+"\" ssh -p "+portnum+" "+username+"@"+servername+" \""+msg+"\""
    linux_command(command)

def ssh_send_file(servername,portnum,username,password,obj,dst):
    command = 'sshpass -p'+password+' scp -P '+portnum+' -o StrictHostKeyChecking=no '+obj+' '+username+'@'+servername+':'+dst
    linux_command(command)

def ssh_send_directory(servername,portnum,username,password,obj,dst):
    command = 'sshpass -p'+password+' scp -P '+portnum+' -r -o StrictHostKeyChecking=no '+obj+' '+username+'@'+servername+':'+dst
    linux_command(command)
