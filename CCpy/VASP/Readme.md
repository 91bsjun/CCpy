# VASP modules
## 2.1. VASP Input Generation
When you run script without any argument, it returns a manual.
<pre>
[user@localhost ~]$ CCpyVASPInputGen.py

How to use : CCpyVASPInputGen.py [option] [sub_option1] [sub_option2..]
--------------------------------------
[options]
1   : Relaxation calculation  (from initial structure files)
2   : Band-DOS calculation    (after previous calculation)
3   : Band-DOS calculation    (from initial structure files)
add : User defined additional calculation from previous calculation


[sub_options]
ex) CCpyVASPInputGen.py 1 -isif=2 -spin -mag -kp=4,4,2 -vdw=D3damp, -pseudo=Nb_sv, -pot=LDA_54...

    < USE PRESET >
    -preset=[NAME] : [NAME].yaml in ~/.CCpy/vasp/

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

    < KPOINTS OPTION >
    -kp=#,#,#                                (DEFAULT : reciprocal parameter as devided by 20)

    < POTCAR OPTION>
    -pot=PBE_54 : VASP potential setting     (DEFAULT : PBE_54)
                  Possible potentials = PBE, PBE_52, PBE_54, LDA, LDA_52, LDA_54, PW91, LDA_US, PW91_US
    -pseudo=    : Select pseudo potential    (DEFAULT : normal)
                  ex) -pseudo=Nb_sv,Ti_sv    --> will use 'Nb_sv, Ti_sv' pseudo potential to 'Nb, Ti'

    < ADDITIONAL CALCULATION >
    when use option 'add', 
    -dir=[DIRNAME]     : Additional calculation dir under previous run
    -pre_dir=[DIRNAME] : Previous directory name to copy CONTCAR, ... (default ./)
    -preset=[NAME]     : [NAME].yaml in ~/.CCpy/vasp/
 

[preset options]
~/.CCpy/vasp/___.yaml

   

</pre>
### 2.1.0. Preset input options
Once you run <code>CCpyVASPInputGen.py</code>, <code>$HOME/.CCpy/vasp/default.yaml</code> will be created.   
This file contains general relaxation options.   
When you run <code>CCpyVASPInputGen.py</code> without any <code>-preset=</code> option, the command will read that file.
#### Use custom preset
- Create <code>my_option.yaml</code> in <code>$HOME/.CCpy/vasp</code>
- Run command with <code>-preset=my_option</code>
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
# ---------- Read INCAR option from /home/user/.CCpy/vasp/default.yaml ---------- #

# ------------------------------------------------------------------ #
#                 Here are the current INCAR options                 #
# ------------------------------------------------------------------ #
SYSTEM           = Graphene                             

# -- Startparameter for this Run
NWRITE           = 2                             ! LPETIM-F    write-flag & timer
ISTART           = 0                             ! job   0-new  1-contEcut  2-sameBS
INIWAV           = 1                             ! 0-jellium  1-random
IWAVPR           = 1                             ! prediction  0-non 1-charg 2-wave 3-comb
ICHARG           = 2                             ! 0-from WF  1-from CHGCAR  2-from atom  11-12-fixed
LWAVE            = .FALSE.                       ! determines write WAVECAR file
LCHARG           = .FALSE.                       ! determines write CHGCAR and CHG

....
....

# -- LDAU+U parameters
# LMAXMIX        = 4                             ! None
# LDAU           = .TRUE.                        ! or .FALSE.
# LDAUTYPE       = 2                             ! 1 or 2 and when 2, U-J
# LDAUL          = 2                             ! l-quantum number for which the on-site interaction is added.
# LDAUU          = 0                             ! the strength of the effective on-site Coulomb interactions
# LDAUJ          = 0                             ! the strength of the effective on-site exchange interactions.

# -- vdW corrections
# IVDW           = 12                            ! specifiy vdW corrections method

* Anything want to modify or add? (ex: ISPIN=2,ISYM=1,#MAGMOM= ) else, enter "n" 
: n

* Use this INCAR to others? (y/n)y


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
#### Then, it will ask create other structures as same as these options.
<pre>
* Use this INCAR to others? (y/n) y
LiFeBO3
LiMnBO3
TiO
</pre>
### 2.1.3. Additional Calculations from previous run
<code>add</code> option allows to generate additional vasp inputs from previous run
- required option: <code>-dir=[NAME]</code> and <code>-preset=[PRESET]</code>
- <code>-dir=[NAME]</code> : additional calculation directory name under previous run
- <code>-preset=[PRESET]</code> : input options for additional calculation (see 2.1.0)
#### Run <code>CCpyVASPInputGen.py add -dir=[DIRNAME] -preset=[PRESET]</code>
example   
Here, we have 2 VASP directories.
<pre>
[user@localhost test]$ ls *
TEST:
CHG     CONTCAR  EIGENVAL  INCAR    OSZICAR  PCDAT   POTCAR  vasp.out     VTEST.e11948  VTEST.o11948  VTEST.pe11948  VTEST.po11948  WAVECAR
CHGCAR  DOSCAR   IBZKPT    KPOINTS  OUTCAR   POSCAR  REPORT  vasprun.xml  VTEST.e11963  VTEST.o11963  VTEST.pe11963  VTEST.po11963  XDATCAR

TEST2:
CHG     CONTCAR  EIGENVAL  INCAR    OSZICAR  PCDAT   POTCAR  vasp.done  vasprun.xml   VTEST.e11963  VTEST.o11963   VTEST.pe11963  VTEST.po11963  XDATCAR
CHGCAR  DOSCAR   IBZKPT    KPOINTS  OUTCAR   POSCAR  REPORT  vasp.out   VTEST.e11948  VTEST.o11948  VTEST.pe11948  VTEST.po11948  WAVECAR

</pre>
Create preset files in <code>$HOME/.CCpy/vasp</code>
<pre>
[user@localhost test]$ ls ~/.CCpy/vasp/
default.yaml  phonon.yaml
</pre>
When you run <code>CCpyVASPInputGen.py add -dir=phonon_opt -preset=phonon</code> it will find finished jobs.   
And create inputs using <code>$HOME/.CCpy/vasp/phonon.yaml</code> under <code>phonon_opt</code> directory.
<pre>
[user@localhost test]$ CCpyVASPInputGen.py add -dir=phonon_opt -preset=phonon
1 : TEST
2 : TEST2
0 : All files
Choose file : 0
TEST/phonon_opt
TEST2/phonon_opt

[user@localhost test]$ ls TEST/phonon_opt/ TEST2/phonon_opt/
TEST2/phonon_opt/:
INCAR  KPOINTS  POSCAR  POTCAR

TEST/phonon_opt/:
INCAR  KPOINTS  POSCAR  POTCAR
</pre>
If you want to copy outputs from previous run, add file lists under <code>KEEP_FILES</code> in <code>[preset].yaml</code> file
##### <code>[peset].yaml</code>
<pre>
KPOINTS:
  length: 30
KEEP_FILES:
  - CONTCAR
  - CHGCAR
  - WAVECAR
INCAR:
  SYSTEM: Li7PS6    
  
  SECTION1: '# -- Startparameter for this Run'
  NWRITE: 2

</pre>
### 2.1.4. Generate Inputs for Band Structure
You can make inputs for calculating VASP band structure as <code> CCpyVASPInputGen.py 2 </code>
<pre>
[user@localhost test]$ CCpyVASPInputGen.py 2
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
Choose file : 

</pre>

If you have line-mode KPOINTS file, put to current directory as <code>KPOINTSP</code>
### example of line-mode KPOINTS file
<pre>
Line_mode KPOINTS file
20
Line_mode
Reciprocal
0.0 0.0 0.0 \Gamma
0.5 0.0 0.0 M

0.5 0.0 0.0 M
0.333333333333 0.333333333333 0.0 K

0.333333333333 0.333333333333 0.0 K
0.0 0.0 0.0 \Gamma
</pre>

If you do not prepare it, the code will make new line-mode KPOINTS file
Type <code> Enter </code> following question.
<pre>
* Line-mode KPOINTS file (KPOINSTP) is not detected.

* Give line-mode KPOINTS file name, else Enter (make new)
: 
</pre>
Then it will ask choose k-points to use in calculating band structure
<pre>
# -------------------------------------------------------- #
#              Make new line-mode KPOINTS file             #
# -------------------------------------------------------- #

* Available k-points in this structure
\Gamma : 0.0 0.0 0.0 
M : 0.5 0.0 0.0 
K : 0.333333333333 0.333333333333 0.0 
A : 0.0 0.0 0.5 
L : 0.5 0.0 0.5 
H : 0.333333333333 0.333333333333 0.5 

* Choose k-points to use Band calculations (ex: \Gamma,M,K,L) 
: \Gamma,K
</pre>

When finishels
d this process, the code will make <code>KPOINTSP</code> file in current directory.
And it will use this file to other inputs.

\* POTCAR directory path in pymatgen/io/vasp/inputs.py PotcarSingle should be edited.


## 2.2. VASP Output Analysis
<pre>
[bsjun@cms PD]$ CCpyVASPAnal.py 

How to use : CCpyVASPAnal.py [option] [sub_option1] [sub_option2..]
"--------------------------------------
[suboptions]
-sub : deep in subdirectories

[options]
d : Clear VASP output files (except of POSCAR, POTCAR, KPOINTS, INCAR)
    ex) CCpyVASPAnal.py d

0 : Check vasp job status.
    ex) CCpyVASPAnal.py 0

1 : Get final structures
    ex) CCpyVASPAnal.py 1

2 : Get final total energy list
    ex) CCpyVASPAnal.py 2 n  : sub option n -> do not show plot

3 : Energy & Cell volume convergence plot
    ex) CCpyVASPAnal.py 3 n  : sub option n -> do not show plot

4 : Generate cif file from POSCAR or CONTCAR
    ex) CCpyVASPAnal.py 4

-zip : zip unnecessary files (remove CHG, zip CHGCAR DOSCAR PROCAR XDATCAR)
    ex) CCpyVASPAnal.py -zip      -> user choose directories
    ex) CCpyVASPAnal.py -zip -sub -> user choose directories (include subdirectories)
    ex) CCpyVASPAnal.py -zip -auto        -> automatically detect converged jobs
    ex) CCpyVASPAnal.py -zip -auto -sub   ->               (include subdirectories)
    ex) CCpyVASPAnal.py -zip -bg          -> detect and zip converged jobs every 30 minutes
    ex) CCpyVASPAnal.py -zip -bg -sub     ->               (include sub directories)
</pre>
Using suboption <code>-sub</code>
This command finds VASP jobs in all subdirectories.

<pre>
[bsjun@cms PD]$ CCpyVASPAnaly.py 2 n -sub
                         Directory  Total energy(eV)     Converged
0         ./Li-P-S-Cl_cifs/Cl16_41        -28.834834          True
1          ./Li-P-S-Cl_cifs/Cl4_39         -7.554547          True
2          ./Li-P-S-Cl_cifs/Cl4_40         -7.350384          True
3      ./Li-P-S-Cl_cifs/Li12P28_48       -188.843065          True
4                   ./Li12P4S16_72       -140.544282          True
5                           ./FeO3       -185.313499          True

</pre> <br>

<pre>
[bsjun@cms PD]$ CCpyVASPAnal.py 1 -sub
1 : ./Li-P-S-Cl_cifs/Cl16_41
2 : ./Li-P-S-Cl_cifs/Cl4_39
3 : ./Li-P-S-Cl_cifs/Cl4_40
4 : ./Li-P-S-Cl_cifs/Li12P28_48
0 : All files
Choose file :
</pre>

### 2.2.1. Option 'd' <code> CCpyVASPAnal.py d </code>
Delete output files except of POSCAR, INCAR, POTCAR, KPOINTS.

### 2.2.2. Option '0'<code> CCpyVASPAnal.py 0 </code>
Monitoring job status.<br>
This command may not be able to find unhandled error messages.
<pre>
[bsjun@node00 Rb-Cu-I-Cl]$ CCpyVASPAnal.py 0

    Parsing VASP jobs....
  [      68 /     68  ]

* Current status :
   Total      End of calculation  Converged  Unconverged  Zipped
0     68                      68         65            3      65

* Unconverged jobs : 3 (01_unconverged_jobs.csv)
    Directory             Status     End of calculation Converged Zipped
3  Cu13I15_39  Error termination                   True     False  False
4  Cu14I19_35  Error termination                   True     False  False
5  Cu16I19_34  Error termination                   True     False  False
You can recalculate using '01_unconverged_jobs.csv' file.

* Detail information saved in: 00_jobs_status.txt
</pre>

### 2.2.3.
### 2.2.4.
### 2.2.5.
### 2.2.6.

### 2.2.7. Option '-zip' <code> CCpyVASPAnal.py -zip </code>
Zip unnecessary VASP output files. (-sub option also can be used) <br>
- Remove CHG.
- Zip CHGCAR, DOSCAR, PROCAR and XDATCAR

##### User can choose directory to zip by <code> CCpyVASPAnal.py -zip </code><br>
<code> CCpyVASPAnal.py -zip -sub </code> also available.
<pre>
[bsjun@cms Li-P-S-Cl_cifs]$ CCpyVASPAnal.py -zip
1 : Cl16_41
2 : Cl4_39
3 : Cl4_40
4 : Li12P28_48
5 : Li12P4S16_72
6 : Li14P6S22_75
7 : Li1Cl1_55
0 : All files
Choose file :
</pre>
##### <code> CCpyVASPAnal.py -zip -auto </code> option will automatically find VASP output directory and zip converged jobs.
<pre>
[bsjun@cms 06_mechanism]$ CCpyVASPAnal.py -zip -auto -sub
# ----------- Parsing -------------- #

    Parsing VASP jobs....
  [       6 /      6  ]

* Current status :
   Total      End of calculation  Converged  Unconverged  Zipped
0      6                       6          6            0       0

* Unconverged jobs : 0 (01_unconverged_jobs.csv)

* Detail information saved in: 00_jobs_status.txt


# ----------- Zipping -------------- #
Current directory: ./0.3/MoS2.H_FO       [       3 /      6  ]
</pre>

##### <code> CCpyVASPAnal.py -zip -bg </code> option will excute loop every 30 minutes to find unverged VASP jobs and zip them.
This option may be useful when you do many number of calculations.
<pre>
[bsjun@cms Li-P-S-Cl_cifs]$ CCpyVASPAnal.py -zip -bg
Start loop..

loop 1


# ----------- Parsing -------------- #

    Parsing VASP jobs....
  [      78 /     78  ]

* Current status :
   Total      End of calculation  Converged  Unconverged  Zipped
0     78                      75         75            3      72

* Unconverged jobs : 0 (01_unconverged_jobs.csv)

* Detail information saved in: 00_jobs_status.txt


# ----------- Zipping -------------- #

Current directory: ./Li12P28_48    [     3/     3]
Done.
Rest 30 minutes..
</pre>
