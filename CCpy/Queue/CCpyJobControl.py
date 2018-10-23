import os, sys
from subprocess import call as shl

# -- version chk
version = sys.version
if version[0] == '3':
    raw_input = input

# -------------------- Config ---------------------#
"""
Up to own HPC system
Only SGE queue system allowed
"""
# queue_path = "/opt/sge/bin/lx24-amd64/"
queue_path = ""
qsub = "qsub"

python_path = "/home/shared/anaconda3/envs/CCpy/bin/python"
mpi_run = "mpirun"            # default mpirun
vasp_mpi_run = "mpirun"
#atk_mpi_run = "/opt/intel/mpi-rt/4.0.0/bin/mpirun"
atk_mpi_run = "/opt/intel/compilers_and_libraries_2018.1.163/linux/mpi/intel64/bin/mpirun"
lammps_mpirun_path = "mpirun"

vasp_run = "mpirun -np $NSLOTS vasp < /dev/null > vasp.out"
#vasp_path="/opt/vasp/vasp5.4.4-beef/bin/vasp_std"

g09_path = "g09"

#atk_path = "/opt/QuantumWise/VNL-ATK/bin/atkpython"
#atk_path = "/opt/QuantumWise/VNL-ATK-2016.3/bin/atkpython"
#atk_path = "/opt/QuantumWise/VNL-ATK-2017.0/bin/atkpython"
atk2017 = "/opt/QuantumWise/VNL-ATK-2017.2/bin/atkpython"
atk2018 = "/opt/QuantumWise/VNL-ATK-2018.06/bin/atkpython"

lammps_path = "lmp_g++"

# -- Queue and nodes settings
#            {"arg":[cpu, mem, queue name, [hosts,]]
queue_info = {"xeon1":[16, 32, "xeon1.q",['node01']],
              "xeon2":[24, 64, "xeon2.q",['node02','node03','node04']],
              "xeon3":[24, 256, "xeon3.q",['node05', 'node06']],
              "xeon4":[36, 256, "xeon4.q",['node07']],
              "xeon5":[72, 512, "xeon5.q",['node08','node09','node10']],
              "xeon6":[48, 512, "xeon6.q",['node12']],
              "xeon7":[52, 192, "xeon7.q",['node13']],
              "epyc":[64, 256, "epyc.q",['node11']]}


class JobSubmit():
    def __init__(self, inputfile, queue, n_of_cpu):
        self.inputfile = inputfile

        cpu, mem, q = queue_info[queue][0], queue_info[queue][1], queue_info[queue][2]

        self.cpu = cpu
        self.mem = mem
        self.q = q
        if n_of_cpu:
            self.n_of_cpu = n_of_cpu
        else:
            self.n_of_cpu = cpu
        self.divided = cpu / self.n_of_cpu

    def gaussian(self, cpu=None, mem=None, q=None):
        inputfile = self.inputfile
        
        cpu, mem, q = self.n_of_cpu, self.mem, self.q
        d = self.divided

        mem = int(mem / d)

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

        shl(queue_path + qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)


    def vasp(self, cpu=None, mem=None, q=None, band=False, phonon=False, dirpath=None, loop=False):
        global vasp_run
        inputfile = self.inputfile

        cpu, q = self.n_of_cpu, self.q

        # -- Band calculation after previous calculation
        if band:
            jobname = "VB" + inputfile
        elif phonon:
            jobname = "VP" + inputfile
        elif loop:
            jobname = "VL" + inputfile
            from CCpy.Package.VASPOptLoopQueScript import VASPOptLoopQueScriptString
            script_string = VASPOptLoopQueScriptString()
            script_filename = ".VASPOptLoop.py"
            f = open(script_filename, "w")
            f.write(script_string)
            f.close()
            script_path = os.getcwd() + "/" + script_filename
            vasp_run = "%s %s\nrm %s" % (python_path, script_path, script_path)
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
 rm vasp.done
 %s
 touch vasp.done

 '''%(cpu, cpu, jobname, q, dirpath, vasp_run)

        pwd = os.getcwd()
        os.chdir(dirpath)
        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()
        shl(queue_path + qsub + " mpi.sh", shell=True)
        #shl("rm -rf ./mpi.sh", shell=True)
        os.chdir(pwd)

    def vasp_batch(self, cpu=None, mem=None, q=None, band=False, dirs=None, scratch=False, loop=False):
        global vasp_run
        cpu, q = self.n_of_cpu, self.q

        # -- Band calculation after previous calculation
        jobname = raw_input("Jobname for this job \n: ")

        runs = ""
        script_path = None
        if loop:
            from CCpy.Package.VASPOptLoopQueScript import VASPOptLoopQueScriptString
            script_string = VASPOptLoopQueScriptString()
            script_filename = ".VASPOptLoop.py"
            f = open(script_filename, "w")
            f.write(script_string)
            f.close()
            script_path = os.getcwd() + "/" + script_filename
            each_run = "rm vasp.done\n%s %s\ntouch vasp.done\n" % (python_path, script_path)
        else:
            each_run = "rm vasp.done\n%s\ntouch vasp.done\n" % vasp_run
        for d in dirs:
            if scratch:
                dir_path = "/scratch/vasp" + d
                runs += "mkdir -p " + dir_path + "\n"           # make dir under /scratch/vasp
                runs += "cp " + d + "/* " + dir_path + "\n"     # copy original to /scratch/vasp
                runs += "cd " + dir_path + "\n"                 # chg dir to /scratch/vasp
                runs += each_run + "\n"                         # run vasp
                runs += "cp " + dir_path + "/* " + d + "\n"     # copy finished job to original dir
                runs += "rm -rf " + dir_path + "\n\n"           # remove finished job under /scratch/vasp
            else:
                runs += "cd " + d + "\n"
                runs += each_run
        if loop:
            runs += "rm %s" % script_path
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
        shl(queue_path + qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def qchem(self, cpu=None, mem=None, q=None):
        inputfile = self.inputfile
        outputfile = inputfile.replace(".in", ".out")
        
        cpu, mem, q = self.n_of_cpu, self.mem, self.q

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

        shl(queue_path + qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def ATK(self, cpu=None, mem=None, q=None, atk_version="atk2017"):
        if atk_version == "atk2018":
            atk_path = atk2018
        else:
            atk_path = atk2017

        inputfile = self.inputfile

        cpu, mem, q = self.n_of_cpu, self.mem, self.q

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

#set MPI_HOME=/opt/intel/mpi-rt/4.0.0
#set MPI_EXEC=$MPI_HOME/bin/mpirun
set MPI_EXEC=%s

setenv OMP_NUM_THREADS 1
setenv OMP_DYNAMIC FALSE
# setenv LD_PRELOAD "libGLU.so libstdc++.so.6"

setenv QUANTUM_LICENSE_PATH 6200@166.104.249.249

cd $SGE_O_WORKDIR

env | grep PRELOAD
$MPI_EXEC -n %d %s %s > %s

''' % (cpu, cpu, jobname, q, atk_mpi_run, cpu, atk_path, inputfile, outputfile)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(queue_path + qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def atat(self, cpu=None, mem=None, q=None):
        dirname = os.getcwd()
        dirname = dirname.split("/")[-1]

        inputfile = self.inputfile

        cpu, q = self.n_of_cpu, self.q

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

        shl(queue_path + qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    # -- To show SGE queue system that " I'm running now "
    def pbs_runner(self, cpu=None, mem=None, q=None):
        inputfile = self.inputfile

        cpu, q = self.n_of_cpu, self.q

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

        shl(queue_path + qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def lammps(self, cpu=None, q=None):
        inputfile = self.inputfile
        outputfile = inputfile.replace("in.", "out.")

        cpu = self.n_of_cpu
        q = self.q
        d = self.divided

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

        shl(queue_path + qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def AIMD_NVT_Loop(self, cpu=None, mem=None, q=None, structure_filename=None, temp=None):
        # -- load loop queue script
        from CCpy.Package.NVTLoopQueScript import NVTLoopQueScriptString
        script_string = NVTLoopQueScriptString()
        script_filename = ".AIMDLoop.py"
        f = open(script_filename, "w")
        f.write(script_string)
        f.close()

        cpu, q = self.n_of_cpu, self.q

        jobname = "NVT%s%dK" % (structure_filename.replace(".cif",""), temp)

        mpi = '''#!/bin/csh
# pe request

#$ -pe mpi_%d %d

# our Job name 
#$ -N %s

#$ -q %s

#$ -V

#$ -cwd


%s %s %s %s
rm %s''' % (cpu, cpu, jobname, q, python_path, script_filename, structure_filename, temp, script_filename)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(queue_path + qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)
