import os, sys
from subprocess import call as shl


# -------------------- Config ---------------------#
mpi_run = "/opt/mpi/intel-parallel-studio2013sp1/openmpi-1.6.5/bin/mpirun"
queue_path = "/opt/sge/bin/lx24-amd64/"
vasp_path = "/opt/vasp/vasp.5.4.1/bin/vasp"
g09_path = "g09"

# -- Queues
#               "arg":[cpu, mem, queue name]
queue_info = {"xeon1":[16, 32, "xeon1.q"],    # node01
              "xeon2":[24, 64, "xeon2.q"],    # node02, node03, node04
              "xeon3":[24, 256, "xeon3.q"],   # node05, node06
              "xeon4":[36, 256, "xeon4.q"],   # node07
              "xeon5":[72, 512, "xeon5.q"],   # node08, node09, node10
              "I5":[4, 16, "I5.q"]}

class JobSubmit():
    def __init__(self, inputfile, queue, divided):
        self.inputfile = inputfile

        cpu, mem, q = queue_info[queue][0], queue_info[queue][1], queue_info[queue][2]

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

set  MPI_EXEC=%s

 cd $SGE_O_WORKDIR

 %s %s
 
'''%(cpu, cpu, jobname, q, mpi_run, g09_path, inputfile)
        
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

        # -- Band calculation after previous calculation
        # -- Generate Precalc -> Band-DOS
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
 %s -np $NSLOTS %s
 cd ..

 cd Band-DOS
 mv INCAR INCAR~
 mv KPOINTS KPOINTS~
 
 cp ../PreCalc/* . >& /dev/null
 mv INCAR~ INCAR
 mv KPOINTS~ KPOINTS
 %s -np $NSLOTS %s
 cd ..

 '''%(cpu, cpu, jobname, q, mpi_run, vasp_path, mpi_run, vasp_path)

        # -- Static calculation at STATiC directory
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
 %s -np $NSLOTS %s
 cd ..


 '''%(cpu, cpu, jobname, q, mpi_run, vasp_path)

        # -- Normal VASP calculation
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

 %s -np $NSLOTS %s
 '''%(cpu, cpu, jobname, q, mpi_run, vasp_path)

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
set  MPI_EXEC=%s

setenv QCSCRATCH /scratch
setenv QCAUX /opt/QChem4.2/qcaux
source /opt/QChem4.2/qcenv.csh

 cd $SGE_O_WORKDIR
 
qchem %s %s

'''%(cpu, cpu, jobname, q, mpi_run, inputfile, outputfile)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(queue_path+"qsub mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def ATK(self, cpu=None, mem=None, q=None):
        inputfile = self.inputfile

        cpu, mem, q = self.cpu, self.mem, self.q
        d = self.divided

        cpu = cpu / d
        mem = mem / d

        jobname = "A" + inputfile.replace(".py", "")
        jobname = jobname.replace(".", "_").replace("-", "_")
        outputfile = inputfile.replace(".py",".out")

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

set MPI_HOME=/opt/intel/mpi-rt/4.0.0
set MPI_EXEC=$MPI_HOME/bin/mpirun

setenv OMP_NUM_THREADS 1
setenv OMP_DYNAMIC FALSE
setenv LD_PRELOAD "libGLU.so libstdc++.so.6"

setenv QUANTUM_LICENSE_PATH 6200@166.104.249.249

cd $SGE_O_WORKDIR

env | grep PRELOAD
$MPI_EXEC -n %d /opt/QuantumWise/VNL-ATK/bin/atkpython %s > %s

''' % (cpu, cpu, jobname, q, cpu, inputfile, outputfile)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(queue_path + "qsub mpi.sh", shell=True)
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

    # -- To show SGE queue system that " I'm running now "
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