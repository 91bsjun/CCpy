#!/usr/local/bin/python2.7

import os, sys
from subprocess import call as shl


# -- Queue command location
queue_path = "/opt/sge/bin/lx24-amd64/"

# -- Queues
xeon1 = [16, 32, "xeon1.q"] # node01
xeon2 = [24, 64, "xeon2.q"] # node02, node03, node04
xeon3 = [24, 256, "xeon3.q"] # node05, node06
xeon4 = [32, 256, "xeon4.q"] # node07
xeon5 = [72, 512, "xeon5.q"] # node08, node09
I5 = [4, 16, "I5.q"]

class JobSubmit():
    def __init__(self, inputfile, queue, divided):
        self.inputfile = inputfile

        if queue == "xeon1":
            queue = xeon1
        elif queue == "xeon2":
            queue = xeon2
        elif queue == "xeon3":
            queue = xeon3
        elif queue == "xeon4":
            queue = xeon4
        elif queue == "xeon5":
            queue = xeon5
        elif queue == "I5":
            queue = I5
        cpu, mem, q = queue[0], queue[1], queue[2]

        self.cpu = cpu
        self.mem = mem
        self.q = q
        self.divided = divided

    def gaussian(self, cpu=None, mem=None, q=None):
        inputfile = self.inputfile
        
        cpu, mem, q = self.cpu, self.mem, self.q
        d = self.divided
        
        cpu = cpu / d
        mem = mem / d

        f = open(inputfile, "r")
        lines = f.readlines()
        f.close()

        f = open(inputfile, "w")
        for line in lines:
            if "%nproc=" in line:
                f.write("%nproc="+str(cpu)+"\n")
            elif "%mem=" in line:
                f.write("%mem="+str(mem)+"Gb\n")
            else:
                f.write(line)
        f.close()

        jobname = "G"+inputfile.replace(".com","")
        jobname = jobname.replace(".","_").replace("-","_")

        mpi = '''#!/bin/csh
#!/bin/csh

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

 g09 %s
 
'''%(cpu, cpu, jobname, q, inputfile)
        
        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(queue_path+"qsub mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)


    def vasp(self, cpu=None, mem=None, q=None, band=False, static=False):
        inputfile = self.inputfile

        cpu, q = self.cpu, self.q
        d = self.divided
        
        cpu = cpu / d        

        os.chdir(inputfile)
        if band:
            jobname = "VB"+inputfile
            jobname = jobname.replace(".","_").replace("-","_")
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


 cd $SGE_O_WORKDIR
 
 cd PreCalc
 mpirun -np $NSLOTS /opt/vasp/vasp.5.4.1/bin/vasp
 cd ..

 cd Band-DOS
 mv INCAR INCAR~
 mv KPOINTS KPOINTS~
 
 cp ../PreCalc/* . >& /dev/null
 mv INCAR~ INCAR
 mv KPOINTS~ KPOINTS
 mpirun -np $NSLOTS /opt/vasp/vasp.5.4.1/bin/vasp
 cd ..

 '''%(cpu, cpu, jobname, q)

        elif static:
            jobname = "VS"+inputfile
            jobname = jobname.replace(".","_").replace("-","_")
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


 cd $SGE_O_WORKDIR
 
 cd STATIC
 mpirun -np $NSLOTS /opt/vasp/vasp.5.4.1/bin/vasp
 cd ..


 '''%(cpu, cpu, jobname, q)
        else:
            jobname = "V"+inputfile
            jobname = jobname.replace(".","_").replace("-","_")
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


 cd $SGE_O_WORKDIR

 mpirun -np $NSLOTS /opt/vasp/vasp.5.4.1/bin/vasp
 '''%(cpu, cpu, jobname, q)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(queue_path+"qsub mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)
        

    def qchem(self, cpu=None, mem=None, q=None):
        inputfile = self.inputfile
        outputfile = inputfile.replace(".in", ".out")
        
        cpu, mem, q = self.cpu, self.mem, self.q
        d = self.divided
        
        cpu = cpu / d
        mem = mem / d

        jobname = "Q"+inputfile.replace(".in","")
        jobname = jobname.replace(".","_").replace("-","_")
        
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

setenv QCSCRATCH /scratch
setenv QCAUX /opt/QChem4.2/qcaux
source /opt/QChem4.2/qcenv.csh

 cd $SGE_O_WORKDIR
 
qchem %s %s

'''%(cpu, cpu, jobname, q, inputfile, outputfile)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(queue_path+"qsub mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)


    def atat(self, cpu=None, mem=None, q=None):
        dirname = os.getcwd()
        dirname = dirname.split("/")[-1]
        
        inputfile = self.inputfile

        cpu, q = self.cpu, self.q
        d = self.divided
        
        cpu = cpu / d

        if "/" in inputfile and "p+" in inputfile:            
            jobname = "AT_"+inputfile.split("/")[-1]
        else:
            jobname = "AT_"+dirname+"_"+inputfile
        jobname = jobname.replace(".","_").replace("-","_").replace("+","_")

        os.chdir(inputfile)
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


 cd $SGE_O_WORKDIR

 pollmach runstruct_vasp mpirun -np %d
 '''%(cpu, cpu, jobname, q, cpu)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(queue_path+"qsub mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def pbs_runner(self, cpu=None, mem=None, q=None):
        inputfile = self.inputfile

        cpu, mem, q = self.cpu, self.mem, self.q
        d = self.divided

        cpu = cpu / d
        mem = mem / d

        jobname = inputfile.replace(".py","")

        mpi = '''#!/bin/csh
#!/bin/csh

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

 python %s

    ''' % (cpu, cpu, jobname, q, inputfile)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(queue_path+"qsub mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)