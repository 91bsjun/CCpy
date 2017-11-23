# Queue
All scripts are supported for SGE only.
## 2.1. Show Job Status
Show job list more effective.
<pre>
[bsjun@node00 TEST]$ CCpyqstat.py 
        ID                  JOBNAME   USER    STATUS           START-TIME          RUN-TIME      QUEUE-NODE SLOTS
0   104595                  IKST_MC  bsjun         r  10/08/2017 17:35:06  46 days, 1:52:50  xeon1.q@node01    16
1   107063  Atraj_nvt_nh_300K_0_1fs   siby         r  10/31/2017 09:53:22  23 days, 9:34:34  xeon3.q@node05    12
2   108619                  IKST_MC  bsjun         r  11/15/2017 17:13:07   8 days, 2:14:49  xeon3.q@node06    24
3   109270        V4Zn_C2N_H4+H2O_8  holee         r  11/23/2017 12:00:37           7:27:19  xeon5.q@node09    72
4   109294             AZB100np_9_A  eunbi         r  11/22/2017 23:05:07          20:22:49  xeon2.q@node03    24
5   109297             AZB100p_10_A  eunbi         r  11/23/2017 05:59:07          13:28:49  xeon2.q@node02    24
6   109299             AZB100p_10_B  eunbi         r  11/23/2017 15:01:07           4:26:49  xeon2.q@node04    24
7   109300         ATS_ZB100_10+H_A  eunbi        qw  11/22/2017 23:12:21          20:15:35         xeon2.q    24
8   109301         ATS_ZB100_10+H_B  eunbi        qw  11/22/2017 23:12:21          20:15:35         xeon2.q    24
9   109302           ATS_ZB100_10_A  eunbi        qw  11/22/2017 23:12:21          20:15:35         xeon2.q    24
10  109303           ATS_ZB100_10_B  eunbi        qw  11/22/2017 23:12:21          20:15:35         xeon2.q    24
11  109304         ATS_ZB111_05+H_A  eunbi        qw  11/22/2017 23:12:21          20:15:35         xeon2.q    24
12  109305         ATS_ZB111_05+H_B  eunbi        qw  11/22/2017 23:12:21          20:15:35         xeon2.q    24
13  109306           ATS_ZB111_05_A  eunbi        qw  11/22/2017 23:12:21          20:15:35         xeon2.q    24
14  109307           ATS_ZB111_05_B  eunbi        qw  11/22/2017 23:12:21          20:15:35         xeon2.q    24
</pre>

## 2.2. Delete Jobs
You can easily delete multiple jobs.
### By job ID
<pre>
[bsjun@node00 TEST]$ CCpyJobDelete.py 1
Job ids (ex: 1154-1232,1357,1411-1422  /  if enter "0": delete all your job.) 
:7344-7352,7359
</pre>
### By job name
<pre>
[bsjun@node00 TEST]$ CCpyJobDelete.py 2
Keyword ? ZnO
</pre>
It will remove all jobs contain "ZnO" in job name.

## 2.3. Submit Jobs
You can make submit jobs of several softwares.
<pre>
[bsjun@node00 TEST]$ CCpyJobSubmit.py 

--------------------------------------------------------------------------------------
     How to use : CCpyJobSubmit.py [Option 1] [Option 2] [Suboptions]
--------------------------------------------------------------------------------------
< Option 1. > Software
    1  : Gaussian09
    2  : VASP
    3  : ATK
    4  : Q-chem
    6  : ATAT
    7  : LAMMPS
    8  : PBS job display

< Option 2. > Queue
    xeon1, xeon2, xeon3, ...

[Suboptions] (optional)
    -d=[integer]    : which divide CPU numbers and memories
                      ex) CCpyJobSubmit.py 2 xeon2 -d=2
                      --> will use half of CPU in xeon2

    -band           : when perform VASP band calculation
                      ex) CCpyJobSubmit.py 2 xeon5 -band
                      ex) CCpyJobSubmit.py 2 xeon5 -band -d=2
                      ex) CCpyJobSubmit.py 2 xeon5 -d=3 -band

    -batch          : run multiple jobs in a single queue
                      (Only VASP supported yet)
                      ex) CCpyJobSubmit.py 2 xeon5 -batch
                      ex) CCpyJobSubmit.py 2 xeon5 -batch -band

    -s              : use scratch directory when run batch jobs
                      (When you calculate quick hundreds jobs, to reduce load in master node)
                      (Only VASP supported yet)

    -a : no check files, calculate all inputs
</pre>
### 2.3.0. Configs
Before run submit codes, you have to modify some environment varaibles in the scripts.
#### CCpyJobControl.py
You have to modify two parts in the top of <code> CCpyJobControl.py </code><br>
First, the path of each run command.
<pre>
# -------------------- Config ---------------------#
queue_path = "/opt/sge/bin/lx24-amd64/"

mpi_run = "mpirun"            # default mpirun
vasp_mpi_run = "mpirun"
atk_mpi_run = "/opt/intel/mpi-rt/4.0.0/bin/mpirun"
lammps_mpirun_path = "mpirun"

vasp_path = "vasp"
g09_path = "g09"
atk_path = "/opt/QuantumWise/VNL-ATK/bin/atkpython"
lammps_path = "lmp_g++"
</pre>
Second, queue information of your system.
<pre>
# -- Queues
#            {"arg":[cpu, mem, queue name]
queue_info = {"xeon1":[16, 32, "xeon1.q"],
              "xeon2":[24, 64, "xeon2.q"],
              "xeon3":[24, 256, "xeon3.q"],
              "xeon4":[36, 256, "xeon4.q"],
              "xeon5":[72, 512, "xeon5.q"],
              "I5":[4, 16, "I5.q"],
              "aws":[36, 48, "all.q"]}
</pre>
- "arg" will be used in the run command <code>CCpyJobSubmit.py 2 <b>xeon1</b></code>
- cpu, mem and queue name are up to your system.
### Common options
<code>CCpyJobSubmit.py</code> run as
<pre>
[bsjun@node00 batch]$ CCpyJobSubmit.py [Option 1] [Option 2] [Suboptions]
</pre>
#### Option 1 must be integer.
<pre>
< Option 1. > Software
    1  : Gaussian09
    2  : VASP
    3  : ATK
    4  : Q-chem
    6  : ATAT
    7  : LAMMPS
    8  : PBS job display
</pre>
#### Option 2 is the queue-argument which indicated in <code>CCpyJobControl.py</code>
In our case,
<pre>
< Option 2. > Queue
    xeon1, xeon2, xeon3, ...
</pre>
#### Suboptions are optional
When you want to devide cores
<pre>
CCpyJobSubmit.py 2 xeon5 -d=3
</pre>

### 2.3.1. Gaussian 09
When you run <code> CCpyJobSubmit.py 1 [queue] </code>. It will detect "*.com" file(s) in current directory.
<pre>
[bsjun@node00 g09]$ ls
bz.com  bz.xyz  H2O.com  H2SO4.com  toluen.xyz

[bsjun@node00 g09]$ CCpyJobSubmit.py 1 xeon2
0 : All files
1 : H2O.com
2 : H2SO4.com
3 : bz.com
Choose file : 1-2
</pre>

### 2.3.2. VASP
When you run <code> CCpyJobSubmit.py 2 [queue] </code>.<br>
It will detect directories which contain VASP inputs (POSCAR, INCAR, KPOINTS, POTCAR)
#### When normal job
<pre>
[bsjun@node00 TEST]$ ls
batch  g09  MoS2.H  mpi.sh  TiNbO4  WS2.H
[bsjun@node00 TEST]$ CCpyJobSubmit.py 2 xeon2
0 : All files
1 : MoS2.H
2 : TiNbO4
3 : WS2.H
Choose file : 0
</pre>
#### When Band calculations
It will detect directories which contain 'Band-DOS' directory.<br>
(From generated by CCpy)
<pre>
[bsjun@node00 TEST]$ CCpyJobSubmit.py 2 xeon2 -band
0 : All files
1 : MoS2.H
2 : WS2.H
Choose file : 1
</pre>
#### Multiple jobs in a single queue
<pre>
[bsjun@node00 batch]$ CCpyJobSubmit.py 2 xeon2 -batch
0 : All files
1 : graphene_000001
2 : graphene_000002
3 : graphene_000003
4 : graphene_000004
5 : graphene_000005
6 : graphene_000006
7 : graphene_000007
8 : graphene_000008
9 : graphene_000009
Choose file : 0
Jobname for this job 
: graphenes
</pre>
If you do this, all jobs (9) will be submitted as "graphenes". <br>
In the case of small jobs (eg. run-time less than 5 minutes), I suggest that use <code> scratch </code> in your system to avoid load.<br>
By adding <code> -scratch </code>
<pre>
[bsjun@node00 batch]$ CCpyJobSubmit.py 2 xeon2 -batch -scratch

### 2.3.3. ATK
