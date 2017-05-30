#!/usr/local/bin/python2.7
"""
This is a package for ROE(Reorganization energy) calculation using Gaussian09
Input : a single molecular structure file (*.xyz, *.car, *.xsd)
"""

import os, sys

from CCpy.Gaussian.Gaussianio import GaussianInput as GI
from CCpy.Tools.CCpyTools import selectInputs, linux_command
from CCpy.Tools.CCpyTools import linux_command as lc


try:
    filename = sys.argv[1]
    name = filename.split(".")[0]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " structure_filename [option] [option2]")
    print('''--------------------------------------
[option]
1 : Step  1. Make input files
2 : Step  2. Submit jobs
3 : Step  3. Calculate Reorganization energy

ex)
1. Make input
[user@localhost] CCpyROEcalc.py benezene.xyz 1
2. Submit jobs
[user@localhost] CCpyROEcalc.py benezene.xyz 2 xeon3
3. Calculate ROE (when all jobs have been finished)
[user@localhost] CCpyROEcalc.py benezene.xyz 3
'''
          )
    quit()

try:
    step = sys.argv[2]
except:
    print("Assign step. (ex" + sys.argv[0].split("/")[-1] + "benzene.xyz 1")
    quit()

functional = "B3LYP"
basis = "6-31G*"
opt = "gfinput gfprint SCF(maxcycle=512,conver=6) opt=gediis"
sp = "gfinput gfprint SCF(maxcycle=512,conver=6) sp"


# --------- Step 1. Make input files --------- #
if step == "1":
    # --------- optimize -1,0,+1 state --------- #
    try:
        os.mkdir(name)
    except:
        chk = raw_input(name + " directory is already exist. Do you want remove it? (y/n) ")
        if chk == "y":
            lc("rm -rf ./"+name)
            os.mkdir(name)
        else:
            quit()

    os.chdir(name)
    lc("cp ../" + filename + " ./")

    # neutral structure
    os.mkdir("neutral")
    os.chdir("neutral")
    os.mkdir("./check")
    lc("cp ../" + filename + " ./")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=0, multi=1, options=opt, options2="")
    myGI.newCalc(filename, comname=name+"_neut")
    os.chdir("../")

    # anaion structure
    os.mkdir("anion")
    os.chdir("anion")
    os.mkdir("./check")
    lc("cp ../" + filename + " ./")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=-1, multi=2, options=opt, options2="")
    myGI.newCalc(filename, comname=name+"_anion")
    os.chdir("../")

    # cation structure
    os.mkdir("cation")
    os.chdir("cation")
    os.mkdir("./check")
    lc("cp ../" + filename + " ./")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=-1, multi=2, options=opt, options2="")
    myGI.newCalc(filename, comname=name+"_cation")
    os.chdir("../")

    # --------- single point state of each structure --------- #
    # anion, cation state of neutral structure
    os.chdir("neutral")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=-1, multi=2, options=sp, options2="")
    myGI.additionalCalc(name+"_neut.chk", comname=name+"_neut_anion")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=1, multi=2, options=sp, options2="")
    myGI.additionalCalc(name + "_neut.chk", comname=name + "_neut_cation")
    os.chdir("../")

    # neutral state of anion structure
    os.chdir("anion")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=0, multi=1, options=sp, options2="")
    myGI.additionalCalc(name + "_anion.chk", comname=name + "_anion_neut")
    os.chdir("../")

    # neutral state of cation structure
    os.chdir("cation")
    myGI = GI(nproc=16, mem=64, functional=functional, basis=basis,
              chg=0, multi=1, options=sp, options2="")
    myGI.additionalCalc(name + "_cation.chk", comname=name + "_cation_neut")
    os.chdir("../")

# -------------- 2. Submit jobs -------------- #
elif step == "2":
    # 1) neutral, anion, cation structures
    # 2) anion, cation state of neutral structure
    #    neutral state of anion structure
    #    neutral state of cation structure

    # Check inputs whether inputs are exist.
    if name in os.listdir("./"):
        subdirs = os.listdir(name)
        if "neutral" not in subdirs or "anion" not in subdirs or "cation" not in subdirs:
            print("Make inputs first.")
            quit()
        os.chdir(name)
    else:
        print("Make inputs first.")
        quit()

    # Parsing queue
    try:
        queue = sys.argv[3]
    except:
        print("You seems to miss type queue name.")
        print("ex: [user@localhost] CCpyROEcalc.py benezene.xyz 2 xeon3")
        quit()

    queue_info = {"xeon1":[16, 32, "xeon1.q"],
                  "xeon2":[24, 64, "xeon2.q"],
                  "xeon3":[24, 256, "xeon3.q"],
                  "xeon4":[36, 256, "xeon4.q"],
                  "xeon5":[72, 512, "xeon5.q"],
                  "I5":[4, 16, "I5.q"]}

    inputpaths = ["./neutral/"+name+"_neut.com",
                  "./anion/"+name+"_anion.com",
                  "./cation/" + name + "_cation.com",
                  "./neutral/" + name + "_neut_anion.com",
                  "./neutral/" + name + "_neut_cation.com",
                  "./anion/" + name + "_anion_neut.com",
                  "./cation/" + name + "_cation_neut.com"]
    inputfiles = [i.split("/")[-1] for i in inputpaths]

    cpu, mem, q = queue_info[queue][0], queue_info[queue][1], queue_info[queue][2]

    # Change %nproc and %mem in inputfiles
    for inputfile in inputpaths:
        f = open(inputfile, "r")
        lines = f.readlines()
        f.close()
        f = open(inputfile, "w")
        for line in lines:
            if "%nproc=" in line:
                f.write("%nproc=" + str(cpu) + "\n")
            elif "%mem=" in line:
                f.write("%mem=" + str(mem) + "Gb\n")
            else:
                f.write(line)
        f.close()

    # set jobname
    jobname = "GROE_" + name
    jobname = jobname.replace(".", "_").replace("-", "_")

    # make queue submit file
    mpi = '''#!/bin/csh

# pe request

#$ -pe mpi_%d %d

# our Job name
#$ -N %s

#$ -S /bin/csh

#$ -q %s

#$ -V

#$ -cwd

echo "Got $NSLOTS slots."
cat $TMPDIR/machines

set  MPI_HOME=/opt/mpi/intel-parallel-studio2013sp1/openmpi-1.6.5
set  MPI_EXEC=$MPI_HOME/bin/mpirun

 cd $SGE_O_WORKDIR
 cd neutral
 g09 %s
 cd ../anion
 g09 %s
 cd ../cation
 g09 %s
 cd ../neutral
 g09 %s
 g09 %s
 cd ../anion
 g09 %s
 cd ../cation
 g09 %s

    ''' % (cpu, cpu, jobname, q, inputfiles[0], inputfiles[1], inputfiles[2], inputfiles[3], inputfiles[4], inputfiles[5], inputfiles[6])

    f = open("mpi.sh", "w")
    f.write(mpi)
    f.close()

    linux_command("qsub mpi.sh; rm ./mpi.sh")

# -------------- 3. Calculate ROE -------------- #
elif step == "3":
    from CCpy.Gaussian.Gaussianio import GaussianOutput as GO
    # Check whether calculations have been done.
    if name in os.listdir("./"):
        subdirs = os.listdir(name)
        if "neutral" not in subdirs or "anion" not in subdirs or "cation" not in subdirs:
            print("Make inputs first.")
            quit()
        os.chdir(name)
    else:
        print("Make inputs first.")
        quit()

    outputpaths = ["./neutral/" + name + "_neut.log",
                   "./neutral/" + name + "_neut_anion.log",
                   "./neutral/" + name + "_neut_cation.log",
                   "./anion/" + name + "_anion.log",
                   "./anion/" + name + "_anion_neut.log",
                   "./cation/" + name + "_cation.log",
                   "./cation/" + name + "_cation_neut.log"]
    energies = []
    for log in outputpaths:
        final_energy = GO(log).getFinalEnergy()
        energies.append(final_energy)

    neut_neut, neut_anion, neut_cation = energies[0], energies[1], energies[2]
    anion_anion, anion_neut, cation_cation, cation_neut = energies[3], energies[4], energies[5], energies[6]

    hole_ROE = ((neut_cation - neut_neut) + (cation_neut - cation_cation)) * 27.2144
    elec_ROE = ((neut_anion - neut_neut) + (anion_neut - anion_anion)) * 27.2144

    f = open("ROE.dat", "w")
    f.write("hole ROE = " + str(hole_ROE) + "\n")
    f.write("elec ROE = " + str(elec_ROE) + "\n")
    f.close()
    print("ROE.dat")