### Python scripts for Computational Chemistry.

# 1. How to set up CCpy
## 1.1. Clone to your PC or Cluster
<pre>
[user@localhost ~]$ cd /opt/shared
[user@localhost shared]$ git clone https://github.com/91bsjun/CCpy.git
</pre>

## 1.2. Build setup file
<pre>
[user@localhost shared]$ cd CCpy
[user@localhost CCpy]$ python setup.py install
</pre>
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

## 1.4. Set VASP potential path environment (If you use CCpyVASPInputGen.py)
Functional directories of VASP in pymatgen is set to
<pre>
    functional_dir = {"PBE": "POT_GGA_PAW_PBE",
                      "PBE_52": "POT_GGA_PAW_PBE_52",
                      "PBE_54": "POT_GGA_PAW_PBE_54",
                      "LDA": "POT_LDA_PAW",
                      "LDA_52": "POT_LDA_PAW_52",
                      "LDA_54": "POT_LDA_PAW_54",
                      "PW91": "POT_GGA_PAW_PW91",
                      "LDA_US": "POT_LDA_US",
                      "PW91_US": "POT_GGA_US_PW91"}
</pre>
So you have to modify directory names or add symbolic link as mentioned in above.
<pre>
[user@localhost VAPS_Potential]$ ls
POT_GGA_PAW_PBE/     POT_GGA_PAW_PBE_54/  POT_LDA_PAW_52/     
POT_GGA_PAW_PBE_52/  POT_LDA_PAW/         POT_LDA_PAW_54/  
</pre>
Now, add environment to your bashrc
<pre>
[user@localhost ~]$ sudo vi /etc/bashrc

...
export VASP_PSP_DIR=/opt/vasp/VASP_Potential
...
</pre>

## 1.5. TEST
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
