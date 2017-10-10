# CCpy
Python scripts for Computational Chemistry.

# 1. How to set up CCpy
## 1.1. Clone to your PC or Cluster
<pre>
[user@localhost ~]$ cd /opt/shared
[user@localhost shared]$ git clone https://github.com/91bsjun/CCpy.git
</pre>

## 1.2. Add the path of CCpy to environment
#### Linux
<pre>
[user@localhost shared]$ vi ~/.bashrc              # for an individual user
[user@localhost shared]$ sudo vi /etc/bashrc       # for all users.

....
....
export PYTHONPATH=/opt/shared:$PYTHONPATH          # for using CCpy in python
export PATH=/opt/shared/bin:$PATH                  # enable excutable files of CCpy

[user@localhost shared]$ source ~/.bashrc
[user@localhost shared]$ sudo source /etc/bashrc   # update edited configurations
</pre>
#### Windows
Control pannel -> System -> Advanced system -> Advanced tap -> Environment <br>
New variable as "Name : PYTHONPATH, Path : Download path"
#### Test
<pre>
[user@localhost ~]$ python
Python 2.7.11 (default, Aug  6 2016, 10:12:58) 
[GCC 4.4.7 20120313 (Red Hat 4.4.7-16)] on linux2
Type "help", "copyright", "credits" or "license" for more information.
>>> import CCpy
>>> 
</pre>
## 1.3. Download python libraries
- Numpy
- Pandas
- Matplotlib
- Pymatgen
<pre>
[user@localhost ~]$ pip install numpy pandas matplotlib pymatgen
</pre>

# 2. VASP modules
## 2.1. VASP Input Generation
When you run script without any argument, it returns a manual.
<pre>
[user@localhost ~]$ CCpyVASPInputGen.py

How to use : CCpyVASPInputGen.py [option] [sub_option1] [sub_option2..]
--------------------------------------
[options]
1 : Relaxation calculation  (from initial structure files)
2 : Band-DOS calculation    (after previous calculation)
3 : Band-DOS calculation    (from initial structure files)
4 : Static calculation      (after previous calculation)

[sub_options]
ex) CCpyVASPInputGen.py 1 -isif=2 -spin -mag -kp=4,4,2 -vdw=D3damp, -pseudo=Nb_sv, -pot=LDA_54...

    < INCAR OPTION >
    -sp      : Single point calculation      (DEFAULT : NSW = 200)
    -isif=#  : ISIF value                    (DEFAULT : 3)
    -spin    : Spin polarized calculation    (DEFAULT : unpolarized)
    -mag     : Add magnetic monet parameters (values from Pymatgen)
    -ldau    : Add LDA+U parameters          (values from Pymatgen)

    van der Waals corrections                (DEFAULT : do not use)
    -vdw=D2     : DFT-D2 method of Grimme                   (VASP.5.2.11)
    -vdw=D3     : zero damping DFT-D3 method of Grimme      (VASP.5.3.4)
    -vdw=D3damp : DFT-D3 method with Becke-Jonson damping   (VASP.5.3.4)
    -vdw=dDsC   : dDsC dispersion correction method         (VASP.5.4.1)

    < KPOINTS OPTION >
    -kp=#,#,#                                (DEFAULT : reciprocal parameter as devided by 20)

    < POTCAR OPTION>
    -pot=PBE_54 : VASP potential setting     (DEFAULT : PBE_54)
                  Possible potentials = PBE, PBE_52, PBE_54, LDA, LDA_52, LDA_54, PW91, LDA_US, PW91_US
    -pseudo=    : Select pseudo potential    (DEFAULT : normal)
                  ex) -pseudo=Nb_sv,Ti_sv    --> will use 'Nb_sv, Ti_sv' pseudo potential to 'Nb, Ti'

[preset options]
~/.CCpy/*.json
    

</pre>
### 2.1.1. Basic input generation
Option '1' is a basic input generation. It detects structure file types in current directory.
- \*.cif
- \*.xsd (material studio, P1 space group)
- \*POSCAR\* (VASP structure type, file name contains 'POSCAR')
### 2.1.2. Example I
#### Run with option 1, without additional options
<pre>
[users@localhost structures]$ ls
LiCoBO3.xsd  LiFeBO3.xsd  LiMnBO3.xsd  Mn3O4.cif  TIN.cif  TiO.cif

[users@localhost structures]$ CCpyVASPInputGen.py 1
0 : All files
1 : LiCoBO3.xsd
2 : LiFeBO3.xsd
3 : LiMnBO3.xsd
4 : Mn3O4.cif
5 : TIN.cif
6 : TiO.cif
Choose file : 1-3,6                   # Choose files using dash and comma

</pre>
#### It will return default INCAR options
<pre>
# -------------------------------------------------------- #
#            Here are the current INCAR options            #
# -------------------------------------------------------- #
SYSTEM           = LiCoBO3                       

#1 Startparameter for this Run:
NWRITE           = 2                             ! LPETIM=F    write-flag & timer
ISTART           = 0                             ! job   : 0-new  1-contEcut  2-sameBS
INIWAV           = 1                             ! 0-jellium  1-random
IWAVPR           = 1                             ! prediction:  0-non 1-charg 2-wave 3-comb
ICHARG           = 2                             ! 0-from WF  1-from CHGCAR  2-from atom  11-12-fixed
LWAVE            = .FALSE.                       ! determines whether the wavefunctions are written to the WAVECAR file

#2 Electronic Relaxation 1
NELM             = 100                           ! number of iterations
EDIFF            = 1E-04                         ! stopping-criterion for ELM
BMIX             = 3.00                          ! sets the cutoff wave vector for Kerker mixing for the magnetization density

...
...

#10 lda+uparameters
# LMAXMIX        = 4                             !
# LDAU           = .TRUE.                        ! or .FALSE.
# LDAUTYPE       = 2                             ! or 1
# LDAUL          = 2 2 2 2                       !
# LDAUU          = 0 4.0 0 0                     !
# LDAUJ          = 0 0 0 0                       !

#11 vdWcorrections
# IVDW           = 0                             ! 0-no , 1-DFT-D2_Grimme, 11-DFT-D3_Grimme, 12-DFT-D3_BJ, 4-dDsC

</pre>
#### If you want to edit or add option, fill as below or not, enter "n"
<pre>
* Anything want to modify or add? (ex: ISPIN=2,ISYM=1,PREC=Accurate // without spacing) if not, enter "n" 
: EDIFF=1E-05,IVDW=12
</pre>
#### If you edit option(s) it will return modified INCAR options again, then enter "n" if you done.
<pre>
* Anything want to modify or add? (ex: ISPIN=2,ISYM=1,PREC=Accurate // without spacing) if not, enter "n" 
: n
</pre>
#### Then, it will ask if you want to update preset of INCAR. 
- The preset options might be saved in <code>~/.CCpy/</code> as json type.
- If you edit <code> ~/.CCpy/vasp_incar.json </code> , this script read it when generate INCAR options.
<pre>
* Do you want to update INCAR preset ? (y/n)
: n
</pre>
#### Finally, it will ask create other structures as same as these options.
<pre>
* Do you want create the rest of inputs as same as these INCAR ? (y/n) y
LiFeBO3
LiMnBO3
TiO
</pre>
### 2.1.2. Example II
Following arguments 
