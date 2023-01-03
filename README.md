### Python scripts for Computational Chemistry.

# 1. Initial setup
### 1.1. Clone to your PC or Cluster
<pre>
[user@localhost ~]$ cd /opt/shared
[user@localhost shared]$ git clone https://github.com/91bsjun/CCpy.git
</pre>

### 1.2. Build setup file
##### Highly recommend to use conda environment with >= python3.6
<pre>
[user@localhost shared]$ conda activate CCpy
(CCpy) [user@localhost shared]$ cd CCpy
(CCpy) [user@localhost CCpy]$ python setup.py install
</pre>
##### Test
<pre>
(CCpy) [user@localhost ~]$ python
Python 3.6.8 |Anaconda, Inc.| (default, Dec 30 2018, 01:22:34)
[GCC 7.3.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> import CCpy
>>> 
</pre>
### 1.3. Download python libraries
- Numpy
- Pandas
- Matplotlib
- ...

### 1.4. Installation of Pymatgen
https://pymatgen.org/installation.html


# 2. VASP setup
### 2.1. Set VASP potential environment of Pymatgen (If you use CCpyVASPInputGen.py)
https://pymatgen.org/installation.html#potcar-setup

### 2.2. TEST
<pre>
(CCpy) [user@localhost ~]$ CCpyVASPInputGen.py 

How to use : CCpyVASPInputGen.py [option] [sub_option1] [sub_option2..]
--------------------------------------
[options]
1 : Relaxation calculation  (from initial structure files)
2 : Band-DOS calculation    (after previous calculation) (will be removed, please use -preset option)
3 : Band-DOS calculation    (from initial structure files) (will be removed, please use -preset option)

[sub_options]
ex) CCpyVASPInputGen.py 1 -isif=2 -spin -mag -kp=4,4,2 -vdw=D3damp, -pseudo=Nb_sv, -pot=LDA_54...

    < USE PRESET >
    -preset=[NAME] : [NAME].yaml in ~/.CCpy/vasp/

    < STRUCTURE OPTION >
    -refine_poscar : Use refined structure with space group (sym prec 0.1)

    < INCAR OPTION >
    -sp      : Single point calculation      (DEFAULT : NSW = 200)
    -isif=#  : ISIF value                    (DEFAULT : 3)
    -spin    : Spin polarized calculation    (DEFAULT : unpolarized)
    -mag     : Use magnetic monet parameters (values from config file)
    -ldau    : Use LDA+U parameters          (values from config file)

    van der Waals corrections                (DEFAULT : do not use)
    -vdw=D2     : DFT-D2 method of Grimme                   (VASP.5.2.11)
    -vdw=D3     : zero damping DFT-D3 method of Grimme      (VASP.5.3.4)
    -vdw=D3damp : DFT-D3 method with Becke-Jonson damping   (VASP.5.3.4)
    -vdw=dDsC   : dDsC dispersion correction method         (VASP.5.4.1)
    -vdw=optb88 : optb88 method
    -vdw=optb86b: optb86b method

    < KPOINTS OPTION >
    -kp=#,#,#                                (DEFAULT : reciprocal parameter as devided by 20)

    < POTCAR OPTION >
    -pot=PBE_54 : VASP potential setting     (DEFAULT : PBE_54)
                  Possible potentials = PBE, PBE_52, PBE_54, LDA, LDA_52, LDA_54, PW91, LDA_US, PW91_US
    -pseudo=    : Select pseudo potential    (DEFAULT : normal)
                  ex) -pseudo=Nb_sv,Ti_sv    --> will use 'Nb_sv, Ti_sv' pseudo potential to 'Nb, Ti'

    < ADDITIONAL CALCULATION >
    when use option 'add',
    -dir=[DIRNAME]     : Additional calculation dir under previous run
    -pre_dir=[DIRNAME] : Previous directory name to copy CONTCAR, ... (default ./)
    -preset=[NAME]     : [NAME].yaml in ~/.CCpy/vasp/

    < SEQUENTIAL JOB >
    This method can be used with CCpyJobSubmit.py without vasp input generation.
    -sequence=[FILENAME] : sequence calculation based on presets and dirname in [FILENAME]
    [file example]
    default  ./
    static   ./static
    band     ./Band-DOS


[preset options]
~/.CCpy/vasp/___.yaml


</pre>

### 2.3. Descriptions of VASP input/output
https://github.com/91bsjun/CCpy/tree/master/CCpy/VASP


# 3. Job scheduler settings
#### HPC scheduler highly depends on each HPC system. It should be modified very carefully based on the understanding of scheduler.
### 3.1. Compute nodes information file
#### 3.1.1. Put the <code>yaml</code> file to somewhere that user(s) can read.
Example of <code>scheduler.yaml</code>
<pre>
scheduler_type: PBS
queue:
  xeon1:
    ncpu: 24
    mem: 128
    q_name: xeon1.q
    nodes:
    - node01
    - node02
    - node03
  xeon2:
    ncpu: 36
    mem: 256
    q_name: xeon2.q
    nodes:
    - node04
    - node05
</pre>
- scheduler_type: PBS, SGE or slurm
- xeon1, xeon2 is the alias of each .q to use in <code> CCpyJobSubmit.py </code> scripts.
- nodes are name of each node in q

#### 3.1.2. Add environment
If you are individual user, add to <code>$HOME/.bashrc</code>. Or if you are admin, add to somewhere (like <code>/etc/bashrc</code>) all user can source it.
<pre>
...
...
export CCpy_SCHEDULER_CONFIG='/your/file/location/scheduler.yaml'
</pre>

#### 3.1.3. Test
Run <code>CCpyJobSubmit.py</code>
<pre>
(CCpy) [user@localhost ~]$ CCpyJobSubmit.py

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
    8  : SIESTA
    9  : NVT MD Loop
    10 : CASM VASP job run
    11 : PBS job display
    12 : SIESTA NVT MD Loop

< Option 2. > Queue
    xeon1, xeon2, xeon3, ...

[Suboptions] (optional)
    -n=[integer]    : the number of CPU to use
                      ex) CCpyJobSubmit.py 2 xeon2 -n=8
                      --> will use 8 CPUs in xeon2

    -node=[nodename]: assign specific node to submit
                      ex) CCpyJobSubmit.py 2 xeon2 -node=node03

    <Support for VASP only>
    -sub            : find vasp jobs under sub directories (only support for vasp)
                      ex) CCpyJobSubmit.py 2 xeon4 -sub
                      ex) CCpyJobSubmit.py 2 xeon5 -batch -sub

    -series         : series batch jobs

    -r              : re-calculate unconverged VASP jobs from '01_unconverged_jobs.csv'
                      ex) CCpyJobSubmit.py 2 xeon3 -r
                      --> '01_unconverged_jobs.csv' required which is generated by 'CCpyVASPAnaly.py 0'

    -band           : when perform VASP band calculation
                      ex) CCpyJobSubmit.py 2 xeon5 -band
                      ex) CCpyJobSubmit.py 2 xeon5 -band -n=8
                      ex) CCpyJobSubmit.py 2 xeon5 -n=8 -band

    -batch          : run multiple jobs in a single queue (only support for vasp)
                      ex) CCpyJobSubmit.py 2 xeon5 -batch
                      ex) CCpyJobSubmit.py 2 xeon5 -batch -band

    -scratch        : use scratch directory when run batch jobs
                      (When you calculate quick hundreds jobs, to reduce load in master node)
                      (Only VASP supported yet)
                      ex) CCpyJobSubmit.py 2 xeon5 -batch -scratch
                      ex) CCpyJobSubmit.py 2 xeon5 -batch -sub -scratch

    -a              : no check files, calculate all inputs

    -T              : Assign temperature when NVT MD simulation in VASP
                      ex) CCpyJobSubmit.py 9 xeon6 -n=24 -T=1000

    -specie=        : Assign diffusion element when NVT MD simulation in VASP (optional, default: Li)
                      ex) CCpyJobSubmit.py 9 xeon6 -n=24 -T=1000 -specie=Na

    -loop           : run VASP jobs until converged. (error will be handled using custodian library in pymatgen)
                      ex) CCpyJobSubmit.py 2 xeon5 -loop
                      *** very careful when use this option ***
                      Not recommend under less understanding of VASP jobs

    -dir=[NAME]     : Find vasp jobs under */[NAME]
                      ./graphene/DOS
                      ex) CCpyJobSubmit.py 2 xeon2 -dir=DOS

    -sequence=[FILENAME] : sequence calculation based on presets and dirname in [FILENAME]
    -refine_poscar : option can be used with this option.
    [Sequence file example]
    default  ./
    static   ./static
    band     ./Band-DOS




    *** Queue config file: /hpchome2/7230508/.CCpy/queue_config.yaml ***
    - User can modify software version (ex. vasp_beef, atk2019...)
    - This file is created when CCpyJobSubmit.py is executed without option.
    - Therefore, if you want to regenerate as the default option or
      if an error occurs due to this file, remove the file and execute the A command.

</pre>

### 3.2. Modify queue config file
If <code>CCpyJobSubmit.py</code> had been executed successfully, <code>queue_config.yaml</code> would have been created under <code>$HOME/.CCpy/</code>.
<pre>
qsub: qsub
python_path: /home/user20/.conda/envs/ccpy/bin/python
mpi_run: mpirun
atk_mpi_run: /opt/intel/compilers_and_libraries_2018.1.163/linux/mpi/intel64/bin/mpirun
vasp_path: /opt/vasp/vasp.6.2.1/bin/vasp_std
g09_path: g09
atk_path: /opt/Quantumwise/VNL-ATK-2019.12SP1/bin/atkpython
lammps_path: lmp_g++
siesta_path: siesta
</pre>
This file contains various executable commands that <code>CCpyJobSubmit.py</code> loads when it runs.

### 3.3. Descriptions of detailed job submission methods
https://github.com/91bsjun/CCpy/tree/master/CCpy/Queue

