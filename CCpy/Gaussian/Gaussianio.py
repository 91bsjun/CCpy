#!/usr/local/bin/python2.7

import os,sys

import matplotlib.pyplot as plt

from CCpy.Tools.CCpyStructure import NonPeriodicCoordinates as npc
from CCpy.Tools.CCpyTools import linux_command as lc
from CCpy.Gaussian.myGausssum import bsjunGausssum

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
#p %s/%s %s

%s

%d %d
%s
%s



''' % (int(nproc), int(mem), chk, functional, basis, options, name, int(chg), int(multi), coordinates, options2)
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
            self.options += " geom=check"
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

        return float(final_energy)

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
                lc("rm -rf ./"+filename+"_tmp")

        

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
        lc("rm -rf ./GaussSum_tmp/")

    def getThermalData(self):
        """
        Zero-point correction=                          .023261 (Hartree/Particle)
        Thermal correction to Energy=                   .026094
        Thermal correction to Enthalpy=                 .027038
        Thermal correction to Gibbs Free Energy=        .052698
        Sum of electronic and zero-point Energies=      -527.492585      E0= Eelec+ZPE
        Sum of electronic and thermal Energies=         -527.489751      E=  E0+ Evib+ Erot+Etrans
        Sum of electronic and thermal Enthalpies=       -527.488807      H=  E+RT
        Sum of electronic and thermal Free Energies=    -527.463147      G=  H-TS
        """
        lines = self.log_file_lines
        final_energy = self.getFinalEnergy()

        for line in lines:
            spl = line.split()
            if "Sum of electronic and zero-point Energies=" in line:
                zero_point_correction_energy = float(spl[-1].replace("\n",""))
            elif "Sum of electronic and thermal Energies=" in line:
                thermal_correction_energy = float(spl[-1].replace("\n",""))
            elif "Sum of electronic and thermal Enthalpies=" in line:
                thermal_correction_enthalpy = float(spl[-1].replace("\n",""))
            elif "Sum of electronic and thermal Free Energies=" in line:
                thermal_correction_gibsfree_energy = float(spl[-1].replace("\n",""))

        return zero_point_correction_energy, thermal_correction_energy, thermal_correction_enthalpy, thermal_correction_gibsfree_energy


    def getOrbitalData(self):
        filename = self.filename
        f = open(filename, "r").read()
        lines = f.split("\n")
        for i in range(len(lines)):
            if "Orbital energies and kinetic energies (alpha):" in lines[i]:
                start = i + 1
            elif "Total kinetic energy from orbitals" in lines[i]:
                finish = i

        H = []
        L = []
        for i in range(start, finish + 1):
            spl = lines[i].split()
            if 'O' in spl:
                H.append([int(spl[0]), float(spl[2]) * 27.221])
            elif 'V' in spl:
                L.append([int(spl[0]), float(spl[2]) * 27.221])

        H.reverse()
        L.reverse()

        data = {'Orbital': [], 'Energy': []}
        for i in range(len(L)):
            name = "L+" + str(len(L) - i - 1)
            if name == "L+0":
                name = "LUMO"
            index = "#" + str(L[i][0]) + " : " + name
            data['Orbital'].append(index)
            data['Energy'].append(L[i][1])

        for i in range(len(H)):
            name = "H-" + str(i)
            if name == "H-0":
                name = "HOMO"
                homo = H[i][0]
            index = "#" + str(H[i][0]) + " : " + name
            data['Orbital'].append(index)
            data['Energy'].append(H[i][1])

        self.homo = homo
        self.data = data

        return homo, data

    def chkTerminatedState(self):
        state = "Not finished or stopped"
        f = os.popen("tail %s" % self.filename).read()
        if "Normal termination" in f:
            state = "Normal termination"
        elif "Error termination" in f:
            state = "Error termination"

        return state
        
def getCubefile(chkfile, no):
    name = chkfile.replace(".chk","")
    lc("formchk "+chkfile)
    fchk = chkfile.replace(".chk",".fchk")
    f = open(fchk,"r")
    lines = f.readlines()
    f.close()
    for line in lines:
        if "Number of alpha electrons" in line:
            homo = int(line.split()[-1].replace("\n",""))
            lumo = homo + 1
    print(name+"_H.cube")
    lc("cubegen 0 MO="+str(homo)+" "+fchk+" "+name+"_H.cube")
    print(name+"_L.cube")
    lc("cubegen 0 MO="+str(lumo)+" "+fchk+" "+name+"_L.cube")
    for i in range(no):
        i+=1
        print(name+"_H-"+str(i)+".cube")
        lc("cubegen 0 MO="+str(homo-i)+" "+fchk+" "+name+"_H-"+str(i)+".cube")
        print(name+"_L+"+str(i)+".cube")
        lc("cubegen 0 MO="+str(lumo+i)+" "+fchk+" "+name+"_L+"+str(i)+".cube")
    
