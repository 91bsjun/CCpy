#!/usr/local/bin/python2.7

import os,sys
from subprocess import call as shl

import matplotlib.pyplot as plt

from CCpy.Tools.CCpyStructure import NonPeriodicCoordinates as npc
from CCpy.myGausssum import bsjunGausssum

from pymatgen.io.gaussian import GaussianOutput as pGO

class GaussianInput():
    def __init__(self, nproc=None, mem=None, functional=None, basis=None,
                 options=None, chg=None, multi=None, options2=None):
        self.nproc = nproc
        self.mem = mem
        self.functional = functional
        self.basis = basis
        self.options = options
        self.chg = chg
        self.multi = multi
        self.options2 = options2


    def comGen(self, name=None, nproc=None, mem=None, chk=None, functional=None, basis=None,
               options=None, chg=None, multi=None, coordinates=None, options2=None):      
        f = open(name+".com","w")
        f.write('''%%nproc=%d
%%mem=%dGb
%%chk=./check/%s
#p %s/%s gfinput gfprint SCF(maxcycle=512,conver=6) %s

%s

%d %d
%s
%s



''' % (nproc, mem, chk, functional, basis, options, name, chg, multi, coordinates, options2)
                )
        print(name+".com")
        

    def newCalc(self, filename, comname=None):            
        if ".car" in filename:
            coordinates = npc(filename).carFile()
            name = filename.replace(".car","")
        elif ".xyz" in filename:
            coordinates = npc(filename).xyzFile()
            name = filename.replace(".xyz","")
        elif ".xsd" in filename:
            coordinates = npc(filename).xsdFile()
            name = filename.replace(".xsd","")
        else:
            print("Unvalid file format\n")
            quit()

        if comname is not None:
            name = comname
            
        chk = name+".chk"

        self.comGen(name=name, nproc=self.nproc, mem=self.mem, chk=chk, functional=self.functional, basis=self.basis,
               options=self.options, chg=self.chg, multi=self.multi, coordinates=coordinates, options2=self.options2)
        

    def additionalCalc(self, chk_filename, comname=None):
        if ".chk" in chk_filename:
            name = chk_filename.replace(".chk","")
        else:
            print("Unvaild file format\n")
            quit()

        if comname is not None:
            name = comname

        if "geom=check" not in self.options:
            self.options += " geom=check "
        if "guess=read" not in self.options:
            self.options += " guess=read "

        self.comGen(name=name, nproc=self.nproc, mem=self.mem, chk=chk_filename, functional=self.functional, basis=self.basis,
               options=self.options, chg=self.chg, multi=self.multi, coordinates="", options2=self.options2)
        
class GaussianOutput():
    def __init__(self, filename):
        f = open(filename, "r")
        lines = f.readlines()
        f.close()

        self.filename = filename
        self.log_file_lines = lines
        self.name = filename.replace(".log","")

    def scfConvergence(self):
        cnt = 0
        indice = []
        energies = []
        for line in self.log_file_lines:            
            if "SCF Done:" in line:
                energy = float(line.split()[4])
                energies.append(energy)
                indice.append(cnt)
                cnt += 1

        return indice, energies

    def getFinalEnergy(self):
        indice, energies = self.scfConvergence()
        final_energy = energies[-1]

        return final_energy

    def getFinalStructure(self):
        filename = self.filename
        name = self.name
        
        go = pGO(filename)
        try:
            final_structure = go.final_structure
        except Exception as e:
            if str(e) == "list index out of range":
                f = open(filename,"r")
                lines = f.readlines()
                f.close()
                f = open(filename+"_tmp","w")
                for i in lines:
                    if "Input orientation:" in i:
                        f.write("                         Standard orientation:                         \n")
                    else:
                        f.write(i)
                f.close()
                go = pGO(filename+"_tmp")
                final_structure = go.final_structure
                subprocess.call("rm -rf ./"+filename+"_tmp", shell=True)

        

        n_atoms = len(final_structure)
        
        atoms = []
        coords = []

        for site in final_structure:
            atom = site.species_string
            coord = site.coords
            x = coord[0]
            y = coord[1]
            z = coord[2]

            atoms.append(atom)
            coords.append(coord)

        self.n_atoms = n_atoms
        self.atoms = atoms
        self.coords = coords

        return n_atoms, atoms, coords

    def getUVData(self):
        filename = self.filename
        
        no = []
        energy = []
        wavelength = []
        osc = []
        strength = []
        energy2 = []
        major_contribs = []
        minor_contribs = []

        try:
            os.mkdir("./GaussSum_tmp")
        except:
            pass
        try:
            gausssum = bsjunGausssum.App(filename)
            ok = gausssum.moresult()
            ok = gausssum.etresult()

            uvdata = open("./GaussSum_tmp/UVData.txt","r")
            uv_lines = uvdata.readlines()
            uvdata.close()
            cnt=0
            for i in uv_lines:
                if cnt >=2:
                    tmp = i.split("\t")
                    no.append(tmp[0])
                    energy.append(tmp[1])
                    wavelength.append(tmp[2])
                    osc.append(tmp[3])
                    strength.append(tmp[4])
                    major_contribs.append(tmp[5])
                    tmp[6]=tmp[6].replace("\n","")
                    minor_contribs.append(tmp[6])

                cnt+=1

            logdata = open(filename,"r")
            line = logdata.readline()
            while line:
                line = logdata.readline()
                if "Excited State" in line:
                    tmp = line.split()
                    energy2.append(tmp[4])

            df = pd.DataFrame({'Energy (cm-1)':energy,
                               'Wave_Length (nm)':wavelength,
                               'Oscillation':osc,
                               'Strength':strength,
                               'Energy (eV)':energy2,
                               'Major Contribs':major_contribs,
                               'Minor Contribs':minor_contribs},
                              index = no)
            df = df[['Energy (eV)', 'Wave_Length (nm)', 'Oscillation', 'Major Contribs', 'Minor Contribs']]
            logdata.close()
            df.to_csv(filename.replace(".log","")+"_UVdata.csv")
        except Exception as e:
            print("* ERROR MESSAGE:")
            if str(e) == "[Errno 2] No such file or directory: './GaussSum_tmp/UVData.txt'":
                print("UV calculation have not perforemd.")
            else:
                print(str(e))
        shl("rm -rf ./GaussSum_tmp/",shell=True)

    def getThermalData(self):
        lines = self.log_file_lines
        final_energy = self.getFinalEnergy()

        for line in lines:
            spl = line.split()
            if "Zero-point correction" in line:
                zero_point_correction_energy = float(spl[-2].replace("\n",""))
            elif "Thermal correction to Energy" in line:
                thermal_correction_energy = float(spl[-1].replace("\n",""))
            elif "Thermal correction to Enthalpy" in line:
                thermal_correction_enthalpy = float(spl[-1].replace("\n",""))
            elif "Thermal correction to Gibbs Free Energy" in line:
                thermal_correction_gibsfree_energy = float(spl[-1].replace("\n",""))

        return zero_point_correction_energy, thermal_correction_energy, thermal_correction_enthalpy, thermal_correction_gibsfree_energy


    def getOrbitalData(self):
        filename = self.filename
        gausssum = bsjunGausssum.App(filename)

        os.mkdir("./GaussSum_tmp")
        mo = gausssum.moresult()
        
        ob_file = open("./GaussSum_tmp/orbital_data.txt","r")
        lines = ob_file.readlines()
        ob_file.close()
        shl("rm -rf ./GaussSum_tmp/",shell=True)

        homo = int(lines[1].split()[1])
        data = {}
        for line in lines:
            spl = line.split()
            if len(spl) == 4:
                data[int(spl[0])] = [spl[1], float(spl[2]), spl[3]]

        self.homo = homo
        self.data = data

        return homo, data

    def chkTerminatedState(self):
        lines = self.log_file_lines

        for line in lines:
            if "Normal termination" in line:
                state = "Normal termination"
            elif "Error termination" in line:
                state = "Error termination"
            else:
                state = "Not finished or stopped"

        return state
        
def getCubefile(chkfile, no):
    name = chkfile.replace(".chk","")
    shl("formchk "+chkfile, shell=True)
    fchk = chkfile.replace(".chk",".fchk")
    f = open(fchk,"r")
    lines = f.readlines()
    f.close()
    for line in lines:
        if "Number of alpha electrons" in line:
            homo = int(line.split()[-1].replace("\n",""))
            lumo = homo + 1
    print(name+"_H.cube")
    shl("cubegen 0 MO="+str(homo)+" "+fchk+" "+name+"_H.cube", shell=True)
    print(name+"_L.cube")
    shl("cubegen 0 MO="+str(lumo)+" "+fchk+" "+name+"_L.cube", shell=True)
    for i in range(no):
        i+=1
        print(name+"_H-"+str(i)+".cube")
        shl("cubegen 0 MO="+str(homo-i)+" "+fchk+" "+name+"_H-"+str(i)+".cube", shell=True)
        print(name+"_L+"+str(i)+".cube")
        shl("cubegen 0 MO="+str(lumo+i)+" "+fchk+" "+name+"_L+"+str(i)+".cube", shell=True)
    
