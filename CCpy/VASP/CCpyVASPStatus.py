#!/bin/env python
import os, sys
from subprocess import call as shl
import pandas as pd

from CCpy.Tools.CCpyTools import selectVASPOutputs

        

def selectVASPJobs(directory_path):
    """
    Needs edition -> pick <INCAR POSCAR KPOINTS POTCAR> included directories
    """
    upper_dirs = [each_dir for each_dir in os.listdir(directory_path) if os.path.isdir(each_dir)]
    all_inputs = []
    for d in upper_dirs:
        os.chdir(d)
        all_dirs = [each_dir for each_dir in os.listdir("./") if os.path.isdir(each_dir)]
        all_dirs.sort()        
        for each_dir in all_dirs:
            files = os.listdir(each_dir)
            if "POSCAR" in files and "POTCAR" in files and "KPOINTS" in files and "INCAR" in files:
                os.chdir(each_dir)
                all_inputs.append(os.getcwd())
                os.chdir("../")
        os.chdir("../")    

    all_jobs = all_inputs

    return all_jobs

def checkStarted():
    files = os.listdir("./")
    if len(files) == 4:
        started = False
    else:
        started = True

    return started

def checkDone():
    if "vasp.done" in os.listdir("./") and "OUTCAR" in os.listdir("./"):
        done = True
    else:
        done = False

    return done

def checkTerminated():
    not_calculated = False
    chk = False
    doscar_chk = False
    outcar_chk = False
    if "DOSCAR" in os.listdir("./"):
        doscar = os.popen("tail DOSCAR").readlines()
        if len(doscar) == 0:
            pass
        else:
            doscar_chk=True
    else:
        doscar_chk=False
    if "OUTCAR" in os.listdir("./"):
        outcar = os.popen("tail OUTCAR").readlines()
        if len(outcar) == 0:
            not_calculated = True
            pass
        else:
            outcar_chk = True
            if "User time (sec):" in outcar[0]:
                chk = True
    else:
        pass
        not_calculated = True
    
    properly_terminated = chk
    
    return properly_terminated, not_calculated, doscar_chk, outcar_chk

def checkConverged():
    if "vasp.out" in os.listdir("./"):
        vasp_out = os.popen("tail vasp.out").readlines()
    else:
        vasp_out = "None"
    chk = False
    if len(vasp_out) < 2:
        chk = False
    else:
        if "please rerun with smaller EDIFF" in vasp_out[-2]:
            chk = False
        elif "reached required accuracy" in vasp_out[-1]:
            chk = True

    converged = chk

    return converged

def checkZipped():
    if "CHG.CHGCAR.PROCAR.vasprun.tar.gz" in os.listdir("./"):
        zipped = True
    else:
        zipped = False

    return zipped

def check_vol():
    vol = os.popen("du -hs ~ --exclude ~/.ssh").readlines()
    vol = vol[0].split("\t")
    vol = vol[0]
    home = vol[1].replace("\n","")

    return vol, home
    
        

def main():
    info = {"Dirname":[], "Started": [], "Done":[], "   Properly-terminated":[], "Converged":[], "Zipped":[]}
    #jobs = selectVASPJobs("./")
    jobs = selectVASPOutputs("./", ask=False, sub=True)
    pwd = os.getcwd()
    cnt = 0
    not_calculated_jobs = []
    empty_doscars = []
    empty_outcars = []
    print("\n    Parsing....")
    for each_job in jobs:
        msg = "  [  " + str(cnt+1).rjust(6) + " / " + str(len(jobs)).rjust(6) + "  ]"
        sys.stdout.write(msg)
        sys.stdout.flush()
        sys.stdout.write("\b" * len(msg))
        os.chdir(each_job)
        #reduced_path = each_job.split("/")[-2] + "/" + each_job.split("/")[-1]
        reduced_path = each_job

        started = checkStarted()

        info['Dirname'].append(reduced_path)
        info['Started'].append(started)
        if started:
            terminated, not_calculated, doscar, outcar = checkTerminated()
            if not_calculated:
                #not_calculated_jobs.append(reduced_path)
                not_calculated_jobs.append(each_job)
            if not doscar:
                empty_doscars.append(reduced_path)
            if not outcar:
                empty_outcars.append(reduced_path)
            converged = checkConverged() 
            zipped = checkZipped()
            done = checkDone()
            info['Done'].append(done)
            info['Converged'].append(converged)
            info['Zipped'].append(zipped)
            info['   Properly-terminated'].append(terminated)
        else:
            info['Done'].append(False)
            info['Converged'].append(False)
            info['Zipped'].append(False)
            info['   Properly-terminated'].append(False)

        os.chdir(pwd)
        cnt+=1
    print("\n\nFinish !")

    pd.set_option('expand_frame_repr', False)
    df = pd.DataFrame(info)
    df = df[['Dirname', 'Started', 'Done', '   Properly-terminated', 'Converged', 'Zipped']]

    # -- Show job infos
    total = len(df)
    started = len(df[(df['Started'] == True)])
    done = len(df[(df['Done'] == True)])
    properly_terminated = len(df[(df['   Properly-terminated'] == True)])
    tmp_df = df[(df['Converged'] == False)]
    tmp_df = tmp_df[(tmp_df['Done'] == True)]
    not_converged = len(tmp_df)
    zipped = len(df[(df['Zipped'] == True)])

    counts = {'Total':[total], 'Started':[started], 'Done':[done], 'Zipped':[zipped],
              '   Properly-terminated':[properly_terminated], 'Not converged':[not_converged]}

    count_df = pd.DataFrame(counts)
    count_df = count_df[['Total', 'Started', 'Done', '   Properly-terminated', 'Not converged', 'Zipped']]
    print("\n\n* Current status :")
    print(count_df)

    '''
    # -- Show terminated before calculation started
    print("\n\n* List of jobs that are supposed to be terminated before the calculation started:")
    if len(not_calculated_jobs) == 0:
        print("Empty.")
    else:
        for d in not_calculated_jobs:
            print(d)
        print("Total : " + str(len(not_calculated_jobs)))
    '''
        

    # -- Find not converged jobs
    print("\n\n* Not converged jobs :")
    if len(tmp_df) == 0:
        print("Empty.")
    else:
        print(tmp_df)
        os.chdir(pwd)
        tmp_df.to_csv("Not_Converged.csv")

    '''
    # -- Empty DOSCAR jobs
    print("\n\n* Empty DOSCAR jobs :")
    if len(empty_doscars) == 0:
        print("Empty.")
    else:
        for f in empty_doscars:
            print(f)
    '''

    # -- Empty OUTCAR jobs
    print("\n\n* Empty OUTCAR jobs :")
    if len(empty_outcars) == 0:
        print("Empty.")
    else:
        for f in empty_doscars:
            print(f)

    # -- Save detail infos
    filename = pwd.split("/")[-1] + "_data.csv"
    filename2 = pwd.split("/")[-1] + "_data.txt"
    os.chdir(pwd)
    df.to_csv(filename)
    f = open(filename2, "w")
    f.write(df.to_string())
    f.close()
    print("\n\n* Information file has been saved : " + filename + ", " + filename2)


if __name__ == '__main__':
   main()
    



