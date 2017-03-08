#!/usr/local/bin/python2.7

import os, sys
from subprocess import call as shl
from CCpy.Tools.CCpyTools import get_ip

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [queue name] [divide]")
    print('''--------------------------------------
[1] : From job id
[2] : From keyword'''
          )
    quit()

# -- Check node00
ip = get_ip()
if ip != "166.104.249.249":
    print("DO AT NODE00 !!")
    quit()

# -- Queue command location
queue_path = "/opt/sge/bin/lx24-amd64/"

li = os.popen('CCpyJobs.py').readlines()

ids = []
names = []

index = 0
for i in li:    
    tmp = i.split()
    if index >=0 :
        job_id = tmp[1]
        job_name = tmp[2]
        ids.append(job_id)
        names.append(job_name)
    index+=1

if sys.argv[1] == "1":
    tmp = raw_input("Job ids (1154-1932) ? ")
    nums = tmp.split("-")
    for i in range(int(nums[0]),int(nums[1])+1):
        shl(queue_path+"qdel "+str(i), shell=True)

elif sys.argv[1] == "2":
    lat = raw_input("Keyword ? ")
    nums = []
    for i in range(len(names)):
        if lat in names[i]:
            nums.append(i)

    for i in nums:
        job_id = ids[i]
        shl(queue_path+"qdel "+job_id, shell=True)
        #print(job_id)

elif sys.argv[1] == "3":
    if "f" in sys.argv:
        f = open("fitsvl_list", "r")
        lines = f.readlines()
        f.close()
        inputs = []
        for l in lines:
            inputs.append(l.replace("\n",""))

        finished = []
        for inputfile in inputs:
            jobname = "AT_"+inputfile.split("/")[-1]
            jobname = jobname.replace(".","_").replace("-","_").replace("+","_")
            try:
                f = open(inputfile+"/energy", "r")
                e = f.read()
                f.close()
                if len(e) > 1:
                    finished.append(jobname)
            except:
                pass
        print(finished)
        for f in finished:
            index=0
            for i in range(len(names)):            
                if names[i] == f:
                    shl(queue_path+"qdel "+str(ids[i]), shell=True)
                    #print("qdel "+str(names[i]))
                index+=1            
        
    else:
        pwd = os.getcwd()
        pwd = pwd.split("/")[-1]

        prefix = "AT_"+pwd+"_"

        finished = []
        dirs = [d for d in os.listdir("./") if os.path.isdir(d) if "energy" in os.listdir(d)]
        for d in dirs:
            f = open(d+"/energy", "r")
            e = f.read()
            f.close()
            if len(e) > 1:
                finished.append(d)


        for f in finished:
            index=0
            for i in range(len(names)):            
                if names[i] == prefix+f:
                    shl(queue_path+"qdel "+str(ids[i]), shell=True)
                index+=1

    
