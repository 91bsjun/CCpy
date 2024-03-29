import os, sys
from subprocess import call as shl
from collections import OrderedDict
import yaml
import time
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
"""

SGE_submit_file_framework = """#!/bin/csh
#$ -q {qname}
#$ -N {jobname}
#$ -pe mpi_{ncpu} {ncpu}
#$ -V
#$ -cwd
"""

PBS_submit_file_framework = """#!/bin/sh -x
#PBS -l select={nselect}:ncpus={ncpu}:mpiprocs={ncpu}:Qlist=vasp
#PBS -q {qname}
#PBS -N {jobname}

cd $PBS_O_WORKDIR

nodes=`cat $PBS_NODEFILE`      # Nodes in job

jobidcut=`echo $PBS_JOBID|cut -d. -f1`
          bhosts=bhosts
          lamhosts=lamhosts
          rm -f $bhosts
          rm -f $lamhosts.*
          #Need to truncate anything after a '.'
          nodes=`echo $nodes | sed 's/\.\w*//g'`
          for node in $nodes; do
              nodecut=`echo $node|cut -d- -f1`
              echo "$nodecut       slots=${{NCPUS}}" >> $bhosts
          done

source /opt/intel/oneapi/setvars.sh
export LD_LIBRARY_PATH=/usr/apps/openmpi_1_8/lib:/usr/apps/fftw-2.1.5/double/lib:$LD_LIBRARY_PATH
export PATH=/opt/intel/oneapi/mpi/2021.4.0/bin:$PATH
NP=`/usr/bin/wc -l $PBS_NODEFILE | awk '{{ print $1 }}'`
"""  

Slurm_submit_file_framework = ""

class JobSubmit:
    def __init__(self, inputfile, queue, n_of_cpu, node=None, init_only=False):
        self.inputfile = inputfile
        home = os.getenv("HOME")
        # -- read configs from queue_config.yaml
        user_queue_config = f"{home}/.CCpy/queue_config.yaml"
        if not os.path.isfile(user_queue_config):
            from pathlib import Path
            MODULE_DIR = Path(__file__).resolve().parent
            default_queue_config = str(MODULE_DIR) + "/queue_config.yaml"
            if ".CCpy" not in os.listdir(home):
                os.mkdir(f"{home}/.CCpy")
            os.system(f"cp {default_queue_config} {user_queue_config}")

        if init_only:
            return

        yaml_string = open(user_queue_config, "r").read()
        queue_config = yaml.load(yaml_string)
        self.queue_config = queue_config
        

        # -- Queue and nodes settings
        try:
            CCpy_SCHEDULER_CONFIG = os.environ['CCpy_SCHEDULER_CONFIG']
        except:
            print('''Error while load $CCpy_SCHEDULER_CONFIG file.
            Please check the example of scheduler config file at https://github.com/91bsjun/CCpy/tree/master/CCpy/Queue''')
            quit()
        scheduler_config = yaml.load(open(CCpy_SCHEDULER_CONFIG, 'r'))

        if queue not in scheduler_config['queue'].keys():
            print(f"'{queue}' queue argument is not in queue configuration file ({CCpy_SCHEDULER_CONFIG}), \nCurrent available:", list(scheduler_config['queue'].keys()))
            quit()

        cpu = scheduler_config['queue'][queue]['ncpu']
        mem = scheduler_config['queue'][queue]['mem']
        q = scheduler_config['queue'][queue]['q_name']

        self.cpu = cpu
        self.mem = mem
        self.q = q
        self.n_of_nodes = 1 # select multi nodes
        if n_of_cpu:
            self.n_of_cpu = n_of_cpu
        else:
            self.n_of_cpu = cpu
        self.divided = cpu / self.n_of_cpu
        if self.n_of_cpu > cpu: # select multi nodes
            self.n_of_nodes = int(self.n_of_cpu / cpu) # select multi nodes

        self.qsub = queue_config['qsub']

        self.python_path = queue_config['python_path']
        self.mpi_run = queue_config['mpi_run']

        self.atk_mpi_run = queue_config['atk_mpi_run']

        self.vasp_path = queue_config['vasp_path']
# >>>>>>>>>>>>>>>>>>>>>>> !!! modify below line up to your system !!! <<<<<<<<<<<<<<<<<<<<<<<<< #
        self.vasp_run = f"{self.mpi_run} -launcher rsh -np $NP -machinefile $PBS_NODEFILE {self.vasp_path} < /dev/null > vasp.out" # !!!! <-- HMC vasp run type
        # self.vasp_run = f"{self.mpi_run} -np $NSLOTS {self.vasp_path} < /dev/null > vasp.out" # !!!! <-- CMS vasp run type

        self.g09_path = queue_config['g09_path']
        self.atk_path = queue_config['atk_path']

        self.lammps_mpirun_path = self.mpi_run
        self.lammps_path = queue_config['lammps_path']
        
        self.siesta_path = queue_config['siesta_path']

        # -- queue settings
        #self.pe_request = "#$ -pe mpi_%d %d" % (self.n_of_cpu, self.n_of_cpu)
        #self.queue_name = "#$ -q %s" % self.q if self.q else ""

        self.scheduler_type = scheduler_config['scheduler_type']

        if self.scheduler_type == "PBS":
            mpi = PBS_submit_file_framework
        elif self.scheduler_type == "SGE":
            mpi = SGE_submit_file_framework
        elif self.scheduler_type == "Slurm":
            mpi = Slurm_submit_file_framework

        if node and self.scheduler_type == "SGE":
            mpi += f"#$ -l h={node}\n"
            
        self.mpi = mpi
        
            

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

        if self.scheduler_type == "PBS":
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu, nselect=self.n_of_nodes)
        else:
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu)
        mpi += f"{self.g09_path} {inputfile}\n"

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

        if self.scheduler_type == "PBS":
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu, nselect=self.n_of_nodes)
        else:
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu)
        mpi += runs

        mpi_filename = "mpi_%s.sh" % jobname
        f = open(mpi_filename, "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " %s" % mpi_filename, shell=True)
        shl("rm -rf ./mpi.sh", shell=True)


    def vasp(self, band=False, dirpath=None, loop=False, sequence=False, refine_poscar=False):
        inputfile = self.inputfile

        #vasp_run = self.vasp_run
        # -- Band calculation after previous calculation
        if band:
            jobname = "VB" + inputfile
        elif loop:
            from pathlib import Path
            MODULE_DIR = Path(__file__).resolve().parent
            loop_opt_script = str(MODULE_DIR) + "/../Package/VASPOptLoop.py"
            os.system(f'cp {loop_opt_script} ./.VASPOptLoop.py')
            script_filename = ".VASPOptLoop.py"
            script_path = os.getcwd() + "/" + script_filename
            # self.vasp_run = "%s %s\nrm %s" % (self.python_path, script_path, script_path)
            self.vasp_run = f"{self.python_path} {script_path}"
            jobname = "VL" + inputfile
        elif sequence:
            from pathlib import Path
            MODULE_DIR = Path(__file__).resolve().parent
            sequence_job_script = str(MODULE_DIR) + "/../Package/VASPSequenceJobs.py"
            os.system(f'cp {sequence_job_script} ./.VASPSequenceJobs.py')
            script_filename = ".VASPSequenceJobs.py"
            script_path = os.getcwd() + "/" + script_filename
            
            sequence_file = sequence

            loop_opt_script = str(MODULE_DIR) + "/../Package/VASPOptLoop.py"
            os.system(f'cp {loop_opt_script} ./.VASPOptLoop.py')
            loop_opt_script_filename = ".VASPOptLoop.py"
            loop_opt_script_path = os.getcwd() + "/" + loop_opt_script_filename
            
            self.vasp_run = f"{self.python_path} {script_path} {inputfile} {sequence_file} {self.python_path} {loop_opt_script_path} {refine_poscar}"
            jobname = "VLS" + inputfile
        else:
            jobname = "V" + inputfile
        jobname = jobname.replace(".", "_").replace("-", "_").replace("/", "_").replace("(", "_").replace(")", "_")
        
        tmp_dirpath = dirpath.replace("(", "\(").replace(")", "\)")

        if self.scheduler_type == "PBS":
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu, nselect=self.n_of_nodes)
        else:
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.n_of_cpu)

        if not sequence:
            mpi += f"cd {tmp_dirpath} \n"
            mpi += f"{self.vasp_run}\n"
            mpi += "touch vasp.done \n"

            pwd = os.getcwd()
            os.chdir(dirpath)
            if 'vasp.done' in os.listdir():
                os.remove('vasp.done')
        else:
            mpi += f"{self.vasp_run}\n"
        mpi_filename = f"mpi_{jobname}.sh"
        f = open(mpi_filename, "w")
        f.write(mpi)
        f.close()
        shl(f"{self.qsub} {mpi_filename}", shell=True)
        time.sleep(0.5)
        #shl("rm -rf ./%s" % mpi_filename, shell=True)
        #os.chdir(pwd)

    def vasp_batch(self, dirs=None, scratch=False, loop=False, jobname=None, sequence=False, refine_poscar=False):
        """
        Run multiple VASP jobs in a single queue
        """
        if not jobname:
            jobname = raw_input("Jobname for this job \n: ")

        runs = ""
        script_path = None

        pwd = os.getcwd()
        if loop:
            from pathlib import Path
            MODULE_DIR = Path(__file__).resolve().parent
            loop_opt_script = str(MODULE_DIR) + "/../Package/VASPOptLoop.py"
            os.system(f'cp {loop_opt_script} ./.VASPOptLoop.py')
            script_filename = ".VASPOptLoop.py"
            script_path = os.getcwd() + "/" + script_filename
            each_run = f"{self.python_path} {script_path}\n"
            each_run += "touch vasp.done\n"
            each_run += "sleep 30\n"
        elif sequence:
            from pathlib import Path
            MODULE_DIR = Path(__file__).resolve().parent
            sequence_job_script = str(MODULE_DIR) + "/../Package/VASPSequenceJobs.py"
            os.system(f'cp {sequence_job_script} ./.VASPSequenceJobs.py')
            script_filename = ".VASPSequenceJobs.py"
            script_path = os.getcwd() + "/" + script_filename
            sequence_file = sequence

            loop_opt_script = str(MODULE_DIR) + "/../Package/VASPOptLoop.py"
            os.system(f'cp {loop_opt_script} ./.VASPOptLoop.py')
            loop_opt_script_filename = ".VASPOptLoop.py"
            loop_opt_script_path = os.getcwd() + "/" + loop_opt_script_filename            
        else:
            each_run = f"{self.vasp_run}\n"
            each_run += "touch vasp.done\n"
            each_run += "sleep 30\n" 
            
        for d in dirs:
            # if use scratch, copy input to /scratch/vasp and run job in that dir,
            # when finished, copy to original working directory
            # scratch is recommended when perform small jobs
            if not sequence:
                os.chdir(d)
                d = d.replace("(", "\(").replace(")", "\)")
                if 'vasp.done' in os.listdir():
                    os.remove('vasp.done')
                os.chdir(pwd)
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
            else:
                inputfile = d    # vasp input files (d) is cif file when sequence run
                runs += f"{self.python_path} {script_path} {inputfile} {sequence_file} {self.python_path} {loop_opt_script_path} {refine_poscar}\n"
                runs += "sleep 30\n\n"

        if self.scheduler_type == "PBS":
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu, nselect=self.n_of_nodes)
        else:
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.n_of_cpu)
        mpi += runs

        pwd = os.getcwd()
        mpi_filename = f"mpi_{jobname}.sh"
        f = open(mpi_filename, "w")
        f.write(mpi)
        f.close()
        shl(f"{self.qsub} {mpi_filename}", shell=True)
        time.sleep(0.5)
        #shl("rm -rf ./%s" % mpi_filename, shell=True)

    def qchem(self):
        inputfile = self.inputfile
        outputfile = inputfile.replace(".in", ".out")

        jobname = "Q" + inputfile.replace(".in", "")
        jobname = jobname.replace(".", "_").replace("-", "_")

        if self.scheduler_type == "PBS":
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu, nselect=self.n_of_nodes)
        else:
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.n_of_cpu)
        mpi += "set  MPI_HOME=/opt/mpi/intel-parallel-studio2013sp1/openmpi-1.6.5\n"
        mpi += f"set  MPI_EXEC={self.mpi_run}\n\n"
        mpi += "setenv QCSCRATCH /scratch\n"
        mpi += "setenv QCAUX /opt/QChem4.2/qcaux\n"
        mpi += "source /opt/QChem4.2/qcenv.csh\n\n"
        mpi += f"qchem {inputfile} {outputfile}\n"

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

        if self.scheduler_type == "PBS":
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu, nselect=self.n_of_nodes)
        else:
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.n_of_cpu)
        mpi += f"set MPI_EXEC={self.atk_mpi_run}\n"
        mpi += "setenv OMP_NUM_THREADS 1\n"
        mpi += "setenv OMP_DYNAMIC FALSE\n\n"
        mpi += "setenv QUANTUM_LICENSE_PATH 6200@166.104.249.249\n\n"
        mpi += f"$MPI_EXEC -n {self.n_of_cpu} {self.atk_path} {inputfile} {outputfile}\n"

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

        if self.scheduler_type == "PBS":
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu, nselect=self.n_of_nodes)
        else:
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.n_of_cpu)

        mpi += f"runstruct_vasp -ng mpirun -np {self.n_of_cpu}\n"
        mpi += "rm wait\n"

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

        if self.scheduler_type == "PBS":
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu, nselect=self.n_of_nodes)
        else:
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.n_of_cpu)

        mpi += f"{self.lammps_mpirun_path} -np {self.n_of_cpu} {self.lammps_path} {inputfile} {outputfile}\n"

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def AIMD_NVT_Loop(self, structure_filename=None, temp=None, specie="Li", screen='no_screen', max_step=250, vdw=False):
        from pathlib import Path
        MODULE_DIR = Path(__file__).resolve().parent
        loop_opt_script = str(MODULE_DIR) + "/../Package/Diffusion/NVTLoopQueScript.py"
        os.system('cp %s ./.NVTLoopQueScript.py' % loop_opt_script)
        script_filename = ".NVTLoopQueScript.py"
        script_path = os.getcwd() + "/" + script_filename

        jobname = "NVT%s_%dK" % (structure_filename.replace(".cif", ""), temp)

        if self.scheduler_type == "PBS":
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu, nselect=self.n_of_nodes)
        else:
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.n_of_cpu)
        mpi += f"{self.python_path} {script_filename} {structure_filename} {temp} {specie} {screen} {max_step} {vdw}\n"

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def AIMD_NVT_Loop_batch(self, structure_files=None, temp=None, specie="Li", screen='no_screen', max_step=250, vdw=False):
        from pathlib import Path
        MODULE_DIR = Path(__file__).resolve().parent
        loop_opt_script = str(MODULE_DIR) + "/../Package/Diffusion/NVTLoopQueScript.py"
        os.system('cp %s ./.NVTLoopQueScript.py' % loop_opt_script)
        script_filename = ".NVTLoopQueScript.py"
        script_path = os.getcwd() + "/" + script_filename

        jobname = input("Job name: ")

        runs = ""
        pwd = os.getcwd()
        if 'structures' not in os.listdir('./'):
            os.mkdir('structures')
        for structure_filename in structure_files:
            dirname = structure_filename.replace(".cif", "")
            runs += "cp %s structures; mkdir %s; mv %s %s; cp %s %s; cd %s\n" % (structure_filename, dirname, structure_filename, dirname, script_filename, dirname, dirname)
            runs += "%s %s %s %s %s %s %s %s\n\n" % (self.python_path, script_filename, structure_filename, temp, specie, screen, max_step, vdw)
            runs += "cd %s \n" % pwd

        if self.scheduler_type == "PBS":
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu, nselect=self.n_of_nodes)
        else:
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.n_of_cpu)
        mpi += runs

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def casm_run(self):
        jobname = raw_input("Job name: ")

        if self.scheduler_type == "PBS":
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu, nselect=self.n_of_nodes)
        else:
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.n_of_cpu)

        mpi += "casm-calc --run\n"

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)

    def siesta(self):
        input_filename = self.inputfile.split("/")[-1]
        dir_path = self.inputfile.replace(input_filename, "")
        jobname = "S" + input_filename.replace(".fdf", "")

        if self.scheduler_type == "PBS":
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu, nselect=self.n_of_nodes)
        else:
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.n_of_cpu)

        mpi += f"{self.mpirun} -np {self.n_of_cpu} {self.siesta_path} < {dirpath} > siesta.out\n"

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

        if self.scheduler_type == "PBS":
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.cpu, nselect=self.n_of_nodes)
        else:
            mpi = self.mpi.format(qname=self.q, jobname=jobname, ncpu=self.n_of_cpu)

        mpi += f"{self.python_path} {script_filename} {structure_filename} {temp} {specie}\n"

        f = open("mpi.sh", "w")
        f.write(mpi)
        f.close()

        shl(self.qsub + " mpi.sh", shell=True)
        shl("rm -rf ./mpi.sh", shell=True)
