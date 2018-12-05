#!/home/bsjun/.conda/envs/bjun/bin/python

import os, sys, re
import datetime
from datetime import timedelta, date
import getpass
import pandas as pd
from CCpy.Queue import CCpyJobControl

queue_path = ""
queue_info = CCpyJobControl.queue_info

def CCpyqstat(in_user="*", in_status="", node_check=False):
    """
    Modules to show SGE qstat more effectively
    """

    q = queue_path + "qstat -r -u '%s' %s" % (in_user, in_status)
    qstat = os.popen(q).read()

    # -- parsing
    firstline_patt = re.compile("\s+[0-9]+ .*", re.M)
    firstline = firstline_patt.findall(qstat)

    jobname_patt = re.compile("Full jobname:\s+\S+")
    jobname = jobname_patt.findall(qstat)

    queue_patt = re.compile("Master Queue:\s+\S+")
    queue = queue_patt.findall(qstat)

    requeue_patt = re.compile("Hard requested queues:\s+\S+")
    requeue = requeue_patt.findall(qstat)

    user = []
    job_id = []
    status = []
    start_time = []
    slot = []

    for l in firstline:
        splt = l.split()
        user.append(splt[3])
        job_id.append(splt[0])
        status.append(splt[4])
        start_time.append(splt[5] + " " + splt[6])
        slot.append(splt[-1])

    jobnames = []
    for j in jobname:
        jobnames.append(j.split()[2])

    queues = []
    for i in range(len(requeue)):
        if i in range(len(queue)):
            queues.append(queue[i].split()[2])
        else:
            queues.append(requeue[i].split()[3])

    run_time = []
    for t in start_time:
        t = datetime.datetime.strptime(t, '%m/%d/%Y %H:%M:%S')
        now = datetime.datetime.now()
        run = str(now - t).split(".")[0]
        run_time.append(run)

    ps = {'ID': job_id, 'JOBNAME': jobnames, 'START-TIME': start_time, 'RUN-TIME': run_time,
          'QUEUE-NODE': queues, 'SLOTS': slot, '   STATUS': status, 'USER': user}

    pd.set_option('expand_frame_repr', False)
    df = pd.DataFrame(ps)
    df = df[['ID', 'JOBNAME', 'USER', '   STATUS', 'START-TIME', 'RUN-TIME', 'QUEUE-NODE', 'SLOTS']]
    pd.set_option('display.max_rows', None)
    print(bcolors.OKBLUE + "# --- Queue status --- #" + bcolors.ENDC)
    print(df)

    # ------------------ Nodes checking ----------------- #
    if node_check:
        get_waiting_nodes(df)
        get_empty_nodes(df)
        chk_load()

    
def get_empty_nodes(df):
    # ------------------ Nodes checking ----------------- #
    if len(df) != 0:
        for i in range(len(df)):
            running_df = df[(df['   STATUS'] == 'r') | (df['   STATUS'] == 'dr')]
    else:
        running_df = []
        
    # -- make all nodes and slots info
    keys = queue_info.keys()
    queue_nodes = {'QUEUE-NODE':[], 'SLOTS':[]}
    for k in keys:
        for n in queue_info[k][3]:
            queue_nodes['QUEUE-NODE'].append(queue_info[k][2] + "@" + n)
            queue_nodes['SLOTS'].append(queue_info[k][0])
    # -- make running nodes and slots info
    runnings = {}
    for i in range(len(running_df)):
        if running_df['QUEUE-NODE'][i] in runnings.keys():
            runnings[running_df['QUEUE-NODE'][i]]+= int(running_df['SLOTS'][i])
        else:
            runnings[running_df['QUEUE-NODE'][i]] = int(running_df['SLOTS'][i])
    running_nodes = {'QUEUE-NODE':[], 'RUN-SLOTS':[]}
    for key in runnings.keys():
        running_nodes['QUEUE-NODE'].append(key)
        running_nodes['RUN-SLOTS'].append(runnings[key])
    # -- make pd.DataFrame for empty slots
    all_nodes_df = pd.DataFrame(queue_nodes).set_index('QUEUE-NODE')
    running_nodes_df = pd.DataFrame(running_nodes).set_index('QUEUE-NODE')
    concat_df = pd.concat([all_nodes_df, running_nodes_df], axis=1, sort=True)
    concat_df = concat_df.fillna(0)
    concat_df['EMPTY-SLOTS'] = concat_df['SLOTS'] - concat_df['RUN-SLOTS']
    empty_df = concat_df[(concat_df['EMPTY-SLOTS'] != 0)]
    #empty_df[['SLOTS', 'RUN-SLOTS', 'EMPTY-SLOTS']] = empty_df[['SLOTS', 'RUN-SLOTS', 'EMPTY-SLOTS']].astype(int)
    
    print(bcolors.OKBLUE + "# ---- Empty Nodes ----- #" + bcolors.ENDC)
    print(empty_df[['SLOTS', 'RUN-SLOTS', 'EMPTY-SLOTS']].astype(int))

def get_waiting_nodes(df):
    # -- make wating nodes and counting
    if len(df) != 0:
        waiting_df = df[(df['   STATUS'] == 'qw')].reset_index()
    else:
        quit() 
    waitings = {}
    for i in range(len(waiting_df)):
        if waiting_df['QUEUE-NODE'][i] in waitings.keys():
            waitings[waiting_df['QUEUE-NODE'][i]] += 1
        else:
            waitings[waiting_df['QUEUE-NODE'][i]] = 1
    waiting_nodes = {'QUEUE':[], 'WAITING JOBS':[]}
    for key in waitings.keys():
        waiting_nodes['QUEUE'].append(key)
        waiting_nodes['WAITING JOBS'].append(waitings[key])
    waiting_nodes_df = pd.DataFrame(waiting_nodes)
    print(bcolors.OKBLUE + "# ---- Pending Jobs ---- #" + bcolors.ENDC)
    print(waiting_nodes_df)

def chk_load():
    qhost = os.popen('qhost').readlines()
    qhost = [l.replace("\n","") for l in qhost]

    info = {'NODE': [], 'NCPU': [], 'LOAD': [], 'CPU USE (%)': []} 
    for l in qhost:
        spl = l.split()
        if spl[0][:4] == 'node':
            node = spl[0]
            info['NODE'].append(node)
            ncpu = float(spl[2])
            info['NCPU'].append(ncpu)
            load = spl[3]
            info['LOAD'].append(load)
            if load == '-':
                load = -1
            load = float(load)
            cpu_use = round(load / ncpu * 100, 1)
            if cpu_use < 0:
                cpu_use = -1
            info['CPU USE (%)'].append(cpu_use)

    df = pd.DataFrame(info)
    down_df = df[(df['CPU USE (%)'] == -1 )]
    ex_df = df[(df['CPU USE (%)'] > 105)]

    if len(down_df) > 0:
        print(bcolors.OKBLUE + "# ------ DOWN node ----- #" + bcolors.ENDC)
        down_nodes = down_df['NODE'].tolist()
        for down_node in down_nodes:
            print(down_node)
    if len(ex_df) > 0:
        print(bcolors.OKBLUE + "# --- Exceeding 100% cpu use node --- #" + bcolors.ENDC)
        print(ex_df.set_index('NODE'))


        
class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


if __name__=="__main__":
    username = "*"
    status = ""
    for arg in sys.argv:
        if "-m" in arg:
            username = getpass.getuser()
        elif "-u" in arg:
            username = arg.replace("-u","")


        if "-r" in arg:
            status = "-s r"


        if "-h" in arg:
            print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [option2]...")
            print('''--------------------------------------
[option]
-m       : My jobs                  (ex : CCpyqstat.py -m)       (ex : CCpyqstat.py -m -r)
-uNAME   : Specific user's jobs     (ex : CCpyqstat.py -ubsjun)  (ex : CCpyqstat.py -ubsjun -r)
-r       : Current running jobs     (ex : CCpyqstat.py -r)       (ex : CCpyqstat.py -m -r)'''
                  )
            quit()

    if username == "*":
        CCpyqstat(username, status, node_check=True)
    else:
        CCpyqstat(username, status)

