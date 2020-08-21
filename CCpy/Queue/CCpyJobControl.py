import os, sys
from subprocess import call as shl
from collections import OrderedDict
import yaml
def represent_dictionary_order(self, dict_data):
    return self.represent_mapping('tag:yaml.org,2002:map', dict_data.items())
yaml.add_representer(OrderedDict, represent_dictionary_order)

# -- version chk
version = sys.version
if version[0] == '3':
    raw_input = input

# -------------------- Config ---------------------#
"""
Up to own HPC system
Only SGE queue system allowed
"""

# -- Queue and nodes settings
try:
    CCpy_SCHEDULER_CONFIG = os.environ['CCpy_SCHEDULER_CONFIG']
except:
    print('''Error while load $CCpy_SCHEDULER_CONFIG file.
Please check the example of scheduler config file at https://github.com/91bsjun/CCpy/tree/master/CCpy/Queue''')
    quit()

queue_info = yaml.load(open(CCpy_SCHEDULER_CONFIG, 'r'))


class JobSubmit:
    def __init__(self, inputfile, queue, n_of_cpu, node=None, init_only=False):
        home = os.getenv("HOME")
        user_queue_config = "%s/.CCpy/queue_config.yaml" % home
        if not os.path.isfile(user_queue_config):
            from pathlib import Path
            MODULE_DIR = Path(__file__).resolve().parent
            default_queue_config = str(MODULE_DIR) + "/queue_config.yaml"
            if ".CCpy" not in os.listdir(home):
                os.mkdir(".CCpy")
            os.system('cp %s %s' % (default_queue_config, user_queue_config))

        if init_only:
            return

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

        # -- read configs from queue_config.yaml            
        yaml_string = open(user_queue_config, "r").read()
        queue_config = yaml.load(yaml_string)
            
        self.qsub = queue_config['qsub']

        self.python_path = queue_config['python_path']
        self.mpi_run = queue_config['mpi_run']

        self.atk_mpi_run = queue_config['atk_mpi_run']

        vasp_path = queue_config['vasp_path']
        self.vasp_run = "%s -np $NSLOTS %s < /dev/null > vasp.out" % (self.mpi_run, vasp_path)

        self.g09_path = queue_config['g09_path']
        self.atk_path = queue_config['atk_path']

        self.lammps_mpirun_path = self.mpi_run
        self.lammps_path = queue_config['lammps_path']
        
        self.siesta_path = queue_config['siesta_path']


        # -- queue settings
        self.pe_request = "#$ -pe mpi_%d %d" % (self.n_of_cpu, self.n_of_cpu)
        self.queue_name = "#$ -q %s" % self.q if self.q else ""
        self.node_assign = ""
        if node:
            self.node_assign = "#$ -l h=%s" % node

    def gaussian(self, ):
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
                f.write("%nproc=" + str(cpu) + "\n")
            elif "%mem=" in line:
                f.write("%mem=" + str(mem) + "Gb\n")
            else:
                f.write(line)
        f.close()

        jobname = "G" + inputfile.replace(".com", "")
        jobname = jobname.replace(".", "_").replace("-", "_")

        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# queue name
%s

# node
%s

#$ -V
#$ -cwd


cd $SGE_O_WORKDIR

%s %s

''' % (jobname, self.pe_request, self.queue_name, self.node_assign, self.g09_path, inputfile)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def gaussian_batch(self, input_files):
        cpu, mem, q = self.n_of_cpu, self.mem, self.q
        d = self.divided

        mem = int(mem / d)

        for inputfile in input_files:
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

        jobname = raw_input("Jobname for this job \n: ")
        runs = ""
        for each_input in input_files:
            runs += "%s %s\nsleep 10\n" % (self.g09_path, each_input)

        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# queue name
%s

# node
%s

#$ -V
#$ -cwd


cd $SGE_O_WORKDIR

%s

''' % (jobname, self.pe_request, self.queue_name, self.node_assign, runs)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)


    def vasp(self, band=False, dirpath=None, loop=False):
        inputfile = self.inputfile

        vasp_run = self.vasp_run
        # -- Band calculation after previous calculation
        if band:
            jobname = "VB" + inputfile
        elif loop:
            from pathlib import Path
            MODULE_DIR = Path(__file__).resolve().parent
            loop_opt_script = str(MODULE_DIR) + "/../Package/VASPOptLoop.py"
            os.system('cp %s ./.VASPOptLoop.py' % loop_opt_script)
            script_filename = ".VASPOptLoop.py"
            script_path = os.getcwd() + "/" + script_filename
            # self.vasp_run = "%s %s\nrm %s" % (self.python_path, script_path, script_path)
            self.vasp_run = "%s %s" % (self.python_path, script_path)
        else:
            jobname = "V" + inputfile
        jobname = jobname.replace(".", "_").replace("-", "_").replace("/", "_")
        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# queue name
%s

# node
%s

#$ -V
#$ -cwd

cd %s
if (-f "vasp.done") then
    rm vasp.done
endif
%s
touch vasp.done

 ''' % (jobname, self.pe_request, self.queue_name, self.node_assign, dirpath, vasp_run)

        pwd = os.getcwd()
        os.chdir(dirpath)
        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()
        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)
        os.chdir(pwd)

    def vasp_batch(self, dirs=None, scratch=False, loop=False):
        """
        Run multiple VASP jobs in a single queue
        """
        jobname = raw_input("Jobname for this job \n: ")

        runs = ""
        script_path = None

        vasp_run = self.vasp_run

        if loop:
            from CCpy.Package.VASPOptLoopQueScript import VASPOptLoopQueScriptString
            script_string = VASPOptLoopQueScriptString()
            script_filename = ".VASPOptLoop.py"
            f = open(script_filename, "w")
            f.write(script_string)
            f.close()
            script_path = os.getcwd() + "/" + script_filename
            each_run = "rm vasp.done\n%s %s\ntouch vasp.done\nsleep 30\n" % (self.python_path, script_path)
        else:
            each_run = "rm vasp.done\n%s\ntouch vasp.done\nsleep 30\n" % vasp_run
        for d in dirs:
            # if use scratch, copy input to /scratch/vasp and run job in that dir,
            # when finished, copy to original working directory
            # scratch is recommended when perform small jobs
            if scratch:
                dir_path = "/scratch/vasp" + d
                runs += "mkdir -p " + dir_path + "\n"  # make dir under /scratch/vasp
                runs += "cp " + d + "/* " + dir_path + "\n"  # copy original to /scratch/vasp
                runs += "cd " + dir_path + "\n"  # chg dir to /scratch/vasp
                runs += each_run + "\n"  # run vasp
                runs += "cp " + dir_path + "/* " + d + "\n"  # copy finished job to original dir
                runs += "rm -rf " + dir_path + "\n\n"  # remove finished job under /scratch/vasp
            # change dir to each input and run 'each_run'
            else:
                runs += "cd " + d + "\n"
                runs += each_run
        # if loop:
        #    runs += "rm %s" % script_path
        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# node
%s

# queue name
%s

#$ -V
#$ -cwd

%s
         ''' % (jobname, self.pe_request, self.queue_name, self.node_assign, runs)

        pwd = os.getcwd()
        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()
        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def qchem(self):
        inputfile = self.inputfile
        outputfile = inputfile.replace(".in", ".out")

        jobname = "Q" + inputfile.replace(".in", "")
        jobname = jobname.replace(".", "_").replace("-", "_")

        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# queue name
%s

# node
%s

#$ -V
#$ -cwd

set  MPI_HOME=/opt/mpi/intel-parallel-studio2013sp1/openmpi-1.6.5
set  MPI_EXEC=%s

setenv QCSCRATCH /scratch
setenv QCAUX /opt/QChem4.2/qcaux
source /opt/QChem4.2/qcenv.csh

cd $SGE_O_WORKDIR

qchem %s %s

''' % (jobname, self.pe_request, self.queue_name, self.node_assign, self.mpi_run, inputfile, outputfile)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def ATK(self, atk_version="atk2017"):
        inputfile = self.inputfile

        jobname = "A" + inputfile.replace(".py", "")
        jobname = jobname.replace(".", "_").replace("-", "_")
        outputfile = inputfile.replace(".py", ".out")

        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# queue name
%s

# node
%s

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

''' % (jobname, self.pe_request, self.queue_name, self.node_assign, self.atk_mpi_run, self.n_of_cpu, self.atk_path,
       inputfile, outputfile)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def atat(self):
        dirname = os.getcwd()
        dirname = dirname.split("/")[-1]

        inputfile = self.inputfile

        if "/" in inputfile and "p+" in inputfile:
            jobname = "AT_" + inputfile.split("/")[-1]
        else:
            jobname = "AT_" + dirname + "_" + inputfile
        jobname = jobname.replace(".", "_").replace("-", "_").replace("+", "_")

        os.chdir(inputfile)
        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# queue name
%s

# node
%s

#$ -V
#$ -cwd

echo "Got $NSLOTS slots."
cat $TMPDIR/machines

cd $SGE_O_WORKDIR

runstruct_vasp -ng mpirun -np %d
rm wait
 ''' % (jobname, self.pe_request, self.queue_name, self.node_assign, self.n_of_cpu)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    # -- To show SGE queue system that " I'm running now "
    def pbs_runner(self):
        inputfile = self.inputfile

        jobname = inputfile.replace(".py", "")

        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# queue name
%s

# node
%s

#$ -V
#$ -cwd

cd $SGE_O_WORKDIR

python %s

    ''' % (jobname, self.pe_request, self.queue_name, self.node_assign, inputfile)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def lammps(self):
        inputfile = self.inputfile
        outputfile = inputfile.replace("in.", "out.")

        jobname = "L" + inputfile.replace("in.", "")
        jobname = jobname.replace(".", "_").replace("-", "_")

        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# queue name
%s

# node
%s

#$ -V
#$ -cwd

cd $SGE_O_WORKDIR

%s -np %d %s < %s | tee %s

    ''' % (jobname, self.pe_request, self.queue_name, self.node_assign, self.lammps_mpirun_path, self.n_of_cpu,
           self.lammps_path, inputfile, outputfile)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def AIMD_NVT_Loop(self, structure_filename=None, temp=None, specie="Li", screen='no_screen'):
        # -- load loop queue script
        from CCpy.Package.Diffusion.NVTLoopQueScript import NVTLoopQueScriptString
        script_string = NVTLoopQueScriptString()
        script_filename = ".AIMDLoop.py"
        f = open(script_filename, "w")
        f.write(script_string)
        f.close()

        jobname = "NVT%s_%dK" % (structure_filename.replace(".cif", ""), temp)

        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# queue name
%s

# node
%s

#$ -V
#$ -cwd


%s %s %s %s %s %s
''' % (jobname, self.pe_request, self.queue_name, self.node_assign, self.python_path,
       script_filename, structure_filename, temp, specie, screen)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def AIMD_NVT_Loop_batch(self, structure_files=None, temp=None, specie="Li", screen='no_screen'):
        # -- load loop queue script
        from CCpy.Package.Diffusion.NVTLoopQueScript import NVTLoopQueScriptString
        script_string = NVTLoopQueScriptString()
        script_filename = ".AIMDLoop.py"
        f = open(script_filename, "w")
        f.write(script_string)
        f.close()

        jobname = input("Job name: ")

        runs = ""
        pwd = os.getcwd()
        if 'structures' not in os.listdir('./'):
            os.mkdir('structures')
        for structure_filename in structure_files:
            dirname = structure_filename.replace(".cif", "")
            runs += "cp %s structures; mkdir %s; mv %s %s; cp %s %s; cd %s\n" % (structure_filename, dirname, structure_filename, dirname, script_filename, dirname, dirname)
            runs += "%s %s %s %s %s %s\n\n" % (self.python_path, script_filename, structure_filename, temp, specie, screen)
            runs += "cd %s \n" % pwd

        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# queue name
%s

# node
%s

#$ -V
#$ -cwd


%s
''' % (jobname, self.pe_request, self.queue_name, self.node_assign, runs)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def casm_run(self):
        jobname = raw_input("Job name: ")

        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# queue name
%s

# node
%s

#$ -V
#$ -cwd

casm-calc --run

        ''' % (jobname, self.pe_request, self.queue_name, self.node_assign)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def siesta(self):
        input_filename = self.inputfile.split("/")[-1]
        dir_path = self.inputfile.replace(input_filename, "")
        jobname = "S" + input_filename.replace(".fdf", "")
        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# queue name
%s

# node
%s

#$ -V
#$ -cwd

cd %s
mpirun -np $NSLOTS %s < %s > siesta.out

        ''' % (jobname, self.pe_request, self.queue_name, self.node_assign, dir_path, self.siesta_path, input_filename)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def siesta_AIMD_NVT_Loop(self, structure_filename=None, temp=None, specie="Li"):
        # -- load loop queue script
        from CCpy.Package.Diffusion.SIESTA_NVTLoopQueScript import NVTLoopQueScriptString
        script_string = NVTLoopQueScriptString()
        script_filename = ".AIMDLoop.py"
        f = open(script_filename, "w")
        f.write(script_string)
        f.close()

        jobname = "SNVT%s_%dK" % (structure_filename.replace(".cif", ""), temp)

        mpi = '''#!/bin/csh
# Job name 
#$ -N %s

# pe request
%s

# queue name
%s

# node
%s

#$ -V
#$ -cwd


%s %s %s %s %s
''' % (jobname, self.pe_request, self.queue_name, self.node_assign, self.python_path,
        script_filename, structure_filename, temp, specie)

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)
