import os, sys
import re
import time
from pymatgen.core.structure import IStructure
from pymatgen.io.vasp.inputs import Incar

def get_vasp_status():
    from custodian.vasp.handlers import VaspErrorHandler, UnconvergedErrorHandler
    converged = False

    # -- 1. check error
    # create max ionic termination
    #subset = VaspErrorHandler.error_msgs
    #incar = Incar.from_file("INCAR")
    #try:
    #    # if NSW not mentioned in INCAR file, default NSW=0
    #    nsw = incar['NSW']
    #except:
    #    nsw = 0
    #subset['max_ionic'] = ['%s F=' % (nsw)]    
    #veh = VaspErrorHandler(errors_subset_to_catch=subset)
    
    veh = VaspErrorHandler()
    err = veh.check()
    err_action = None
    if err:
        err_action = veh.correct()
    # -- 2. check unconverged
    else:
        ueh = UnconvergedErrorHandler()
        err = ueh.check()
        if err:
            err_action = ueh.correct()

    if err_action:
        write_log(str(err_action))
        converged = False
    else:
        converged = True

    return str(converged)

def get_final_energy():
    if "OUTCAR" not in os.listdir("./"):
        write_log("Error jobs")
        return "err", "err"

    OUTCAR = open("OUTCAR", "r").read()
    # -- energy parsing
    findE = re.compile("free  energy   TOTEN  =\s+\S+", re.M)
    strings = findE.findall(OUTCAR)
    e = []
    for s in strings:
        try:
            e.append(float(s.split()[4]))
        except:
            return "err", "err"

    if len(e) == 0:
        return "err", "err"

    return round(e[0], 8), round(e[-1], 8)


def get_structure_info(filename):
    structure = IStructure.from_file(filename)
    lattice = structure.lattice
    a, b, c, alpha, beta, gamma, vol = lattice.a, lattice.b, lattice.c, lattice.alpha, lattice.beta, lattice.gamma, lattice.volume

    return [a, b, c, alpha, beta, gamma, vol]


def write_log(msg):
    f = open("job_log", "a")
    f.write(msg + "\n")
    f.close()


def write_energy(x, y):
    f = open("final_energy.csv", "a")
    f.write(str(x) + "," + str(y) + "\n")
    f.close()


def check_empty():
    outputs = ["CONTCAR", "OUTCAR", "OSZICAR", "vasp.out"]
    for filename in outputs:        
        if os.path.getsize(filename) == 0:
            write_log(filename + " is empty, in this case ERR happened. Check your input options")
            os.system("touch loop.done")
            quit()

home = os.getenv("HOME")
user_queue_config = f"{home}/.CCpy/queue_config.yaml"
MODULE_DIR = Path(__file__).resolve().parent
if not os.path.isfile(user_queue_config):
    default_queue_config = str(MODULE_DIR) + "/queue_config.yaml"
    if ".CCpy" not in os.listdir(home):
        os.mkdir(f"{home}/.CCpy")
    os.system(f"cp {default_queue_config} {user_queue_config}")
# -- read configs from queue_config.yaml
yaml_string = open(user_queue_config, "r").read()
queue_config = yaml.load(yaml_string)
vasp_path = queue_config['vasp_path']
# >>>>>>>>>>>>>>>>>>>>>>> !!! modify below line up to your system !!! <<<<<<<<<<<<<<<<<<<<<<<<< #
vasp_run = f"mpirun -launcher rsh -np 16 -machinefile $PBS_NODEFILE {vasp_path} < /dev/null > vasp.out" # !!!! <-- to edit !!!!

if __name__ == "__main__":
    # -- reset logfile
    if "job_log" in os.listdir("./"):
        os.remove("job_log")
    if "final_energy.csv" in os.listdir("./"):
        os.remove("final_energy.csv")
    f = open("final_energy.csv", "w")
    f.write("Loop index,Final energy (eV)\n")
    f.close()
    # -- first calc begin
    lat = get_structure_info("POSCAR")
    write_log("Initial structure.  a: %.2f  b: %.2f  c: %.2f  alpha: %.2f  beta: %.2f  gamma: %.2f  volume: %.2f" % (lat[0], lat[1], lat[2], lat[3], lat[4], lat[5], lat[6]))
    write_log("Start first relaxation")
    
    loop = 0
    write_log("\nLoop: " + str(loop))
    #os.system("mpirun -np $NSLOTS vasp < /dev/null > vasp.out")
    os.system(vasp_run)
    time.sleep(30)

    # -- parsing output
    c = get_vasp_status()
    init_e, final_e = get_final_energy()
    if init_e == "err":
        write_log("Convergence: %s  Initial energy: %s    Final energy: %s    dE: %s" % (str(c), str(init_e), str(final_e), "err"))
        write_log("Final structure.  ERR")
    else:
        write_log("Convergence: %s  Initial energy: %s    Final energy: %s    dE: %s" % (str(c), str(init_e), str(final_e), str(final_e - init_e)))
        lat = get_structure_info("CONTCAR")
        write_log("Final structure.  a: %.2f  b: %.2f  c: %2.f  alpha: %.2f  beta: %.2f  gamma: %.2f  volume: %.2f" % (lat[0], lat[1], lat[2], lat[3], lat[4], lat[5], lat[6]))
    write_energy(loop, final_e)
    while c == "False":
        # -- new calc begin
        loop += 1
        os.system("rm vasp.done")
        #os.system("mpirun -np $NSLOTS vasp < /dev/null > vasp.out")
        os.system(vasp_run)
        time.sleep(30)
        write_log("\nLoop: " + str(loop))
        c = get_vasp_status()
        init_e, final_e = get_final_energy()        
        if init_e == "err":
            write_log("Convergence: %s  Initial energy: %s    Final energy: %s    dE: %s" % (str(c), str(init_e), str(final_e), "err"))
            write_log("Final structure.  ERR")
        else:
            write_log("Convergence: %s  Initial energy: %s    Final energy: %s    dE: %s" % (str(c), str(init_e), str(final_e), str(final_e - init_e)))
            lat = get_structure_info("CONTCAR")
            write_log("Final structure.  a: %.2f  b: %.2f  c: %2.f  alpha: %.2f  beta: %.2f  gamma: %.2f  volume: %.2f" % (lat[0], lat[1], lat[2], lat[3], lat[4], lat[5], lat[6]))
        write_energy(loop, final_e)
        # -- terminate max loop creteria
        if loop == 20:
            c = "Failed"
    if c == "Failed":
        write_log("\nConvergence Failed!")
    else:
        write_log("\nConvergence done!")
    os.system("touch loop.done")
    quit()
