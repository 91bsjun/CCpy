#!/usr/local/bin/python2.7

import os, sys, re
import datetime
from datetime import timedelta, date
import getpass
import pandas as pd

def CCpyqstat(in_user="*",in_status=""):
    """
    Modules to show SGE qstat more effectively
    """
    q = "/opt/sge/bin/lx24-amd64/qstat -u '" + in_user + "' -r " + in_status
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
    print(df)

if __name__=="__main__":
    for arg in sys.argv:
        if "-m" in arg:
            username = getpass.getuser()
        elif "-u" in arg:
            username = arg.replace("-u","")
        else:
            username = "*"

        status = "-r" if "-s r" in arg else ""

        if "-h" in arg:
            print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [option2]...")
            print('''--------------------------------------
[option]
-m       : My jobs                  (ex : CCpyJobs.py -m)
-uNAME   : Specific user's jobs     (ex : CCpyJobs.py -ubsjun)
-r       : Current running jobs     (ex : CCpyJobs.py -r) (ex : CCpyJobs.py m r)'''
                  )
            quit()

    CCpyqstat(in_user=username,in_status=status)
