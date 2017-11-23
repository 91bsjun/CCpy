import os, sys
from subprocess import call as shl


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

# -- Queues
#               "arg":[cpu, mem, queue name]
queue_info = {"xeon1":[16, 32, "xeon1.q"],    # node01
              "xeon2":[24, 64, "xeon2.q"],    # node02, node03, node04
              "xeon3":[24, 256, "xeon3.q"],   # node05, node06
              "xeon4":[36, 256, "xeon4.q"],   # node07
              "xeon5":[72, 512, "xeon5.q"],   # node08, node09, node10
              "I5":[4, 16, "I5.q"],
              "aws":[36, 48, "all.q"]}

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

 cd $SGE_O_WORKDIR

 %s %s
 
'''%(cpu, cpu, jobname, q, g09_path, inputfile)
        
        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(queue_path+"qsub mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)


    def vasp(self, cpu=None, mem=None, q=None, band=False, dirpath=None):
        inputfile = self.inputfile

        cpu, q = self.cpu, self.q
        d = self.divided
        
        cpu = cpu / d        

        # -- Band calculation after previous calculation
        if band:
            jobname = "VB" + inputfile
        else:
            jobname = "V" + inputfile
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


 cd %s

 %s -np $NSLOTS %s < /dev/null > vasp.out
 touch vasp.done

 '''%(cpu, cpu, jobname, q, dirpath, mpi_run, vasp_path)

        pwd = os.getcwd()
        os.chdir(dirpath)
        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()
        #shl(queue_path+"qsub mpi.sh", shell=True)
        #shl("rm -rf ./mpi.sh", shell=True)
        os.chdir(pwd)

    def vasp_batch(self, cpu=None, mem=None, q=None, band=False, dirs=None, scratch=False):

        cpu, q = self.cpu, self.q
        d = self.divided

        cpu = cpu / d

        # -- Band calculation after previous calculation
        jobname = raw_input("Jobname for this job \n: ")

        runs = ""
        each_run = "%s -np $NSLOTS %s < /dev/null > vasp.out\ntouch vasp.done" % (mpi_run, vasp_path)
        for d in dirs:
            if scratch:
                dir_path = "/scratch/vasp" + d
                runs += "mkdir -p " + dir_path + "\n"           # make dir under /scratch/vasp
                runs += "cp " + d + "/* " + dir_path + "\n"     # copy original to /scratch/vasp
                runs += "cd " + dir_path + "\n"                 # chg dir to /scratch/vasp
                runs += each_run + "\n"                         # run vasp
                runs += "cp " + dir_path + "/* " + d + "\n"     # copy finished job to original dir
                runs += "rm -rf " + dir_path + "\n\n"             # remove finished job under /scratch/vasp
            else:
                runs += "cd " + d + "\n"
                runs += each_run
        mpi = '''#!/bin/csh
#$ -pe mpi_%d %d
#$ -N %s
#$ -q %s
#$ -V
#$ -cwd

%s
         ''' % (cpu, cpu, jobname, q, runs)

        pwd = os.getcwd()
        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()
        #shl(queue_path + "qsub mpi.sh", shell=True)
        #shl("rm -rf ./mpi.sh", shell=True)

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
# setenv LD_PRELOAD "libGLU.so libstdc++.so.6"

setenv QUANTUM_LICENSE_PATH 6200@166.104.249.249

cd $SGE_O_WORKDIR

env | grep PRELOAD
$MPI_EXEC -n %d %s %s > %s

''' % (cpu, cpu, jobname, q, cpu, atk_path, inputfile, outputfile)

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
            jobname = "AT_" + inputfile.split("/")[-1]
        else:
            jobname = "AT_" + dirname + "_" + inputfile
        jobname = jobname.replace(".", "_").replace("-", "_").replace("+", "_")

        os.chdir(inputfile)
        mpi = '''#!/bin/csh

# pe request
#$ -pe mpi_%d %d

# Job name
#$ -N %s

#$ -S /bin/csh
#$ -q %s
#$ -V
#$ -cwd

echo "Got $NSLOTS slots."
cat $TMPDIR/machines

cd $SGE_O_WORKDIR

runstruct_vasp -ng mpirun -np %d
rm wait
 ''' % (cpu, cpu, jobname, q, cpu)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(queue_path + "qsub mpi.sh", shell=True)
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

    def lammps(self, cpu=None, q=None):
        inputfile = self.inputfile
        outputfile = inputfile.replace("in.", "out.")

        cpu, q = self.cpu, self.q
        d = self.divided

        cpu = cpu / d

        jobname = "L" + inputfile.replace("in.", "")
        jobname = jobname.replace(".", "_").replace("-", "_")

        mpi = '''#!/bin/csh

# pe request

#$ -pe mpi_%d %d

# our Job name
#$ -N %s

#$ -S /bin/csh

#$ -q %s

#$ -V

#$ -cwd

 cd $SGE_O_WORKDIR

 %s -np %d %s < %s | tee %s

    ''' % (cpu, cpu, jobname, q, lammps_mpirun_path, cpu, lammps_path, inputfile, outputfile)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(queue_path + "qsub mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)