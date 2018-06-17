# VASP modules
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
                  Possible potentials = PBE, PBE_52, PBE_54, LDA, LDA_52, LDA_54, PW91, LDA_US, PW91_US, USPP
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
### 2.1.3. Example II
Following arguments 

### 2.1.4. Generate Inputs for Band Structure
You can make inputs for calculating VASP band structure as <code> CCpyVASPInputGen.py 2 </code>
<pre>
[bsjun@cms test]$ CCpyVASPInputGen.py 2
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

#### Option 'd' <code> CCpyVASPAnal.py d </code>
Delete output files except of POSCAR, INCAR, POTCAR, KPOINTS.

#### Option '0'<code> CCpyVASPAnal.py 0 </code>
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

#### Option '-zip' <code> CCpyVASPAnal.py -zip </code>
Zip unnecessary VASP output files. (-sub option also can be used) <br>
- Remove CHG.
- Zip CHGCAR, DOSCAR, PROCAR and XDATCAR

##### User can choose directory to zip by <code> CCpyVASPAnal.py -zip </code>
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
