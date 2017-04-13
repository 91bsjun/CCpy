#!/usr/local/bin/python2.7

import os,sys
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

from CCpy.Gaussian.Gaussianio import GaussianOutput as GO
from CCpy.Gaussian.Gaussianio import getCubefile
from CCpy.Tools.CCpyStructure import NonPeriodicCoordinates as npc
from CCpy.Tools.CCpyTools import selectInputs, linux_command

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option]")
    print('''--------------------------------------
[0] : Check termination status
[1] : Plot optimization convergence
[2] : Get final energy
[3] : Get final structure
[4] : Get UV data
[5] : Get Orbital data
[6] : Get thermal correction data
[7] : Make Cube file'''
          )
    quit()

ask = True
if "a" in sys.argv:
    ask = False
    
log_anal = ["0","1","2","3","4","5","6"]
if sys.argv[1] in log_anal:
    input_marker = [".log"]
    logs = selectInputs(input_marker, "./", ask=ask)

if sys.argv[1] == "0":
    info = {}
    states = []
    for log in logs:
        state = GO(log).chkTerminatedState()
        states.append(state)
    info["Status"] = states
    df = pd.DataFrame(info, index=logs)
    print(df)

elif sys.argv[1] == "1":
    for log in logs:
        steps, energies = GO(log).scfConvergence()
        plt.plot(steps, energies, marker="o")
        plt.tight_layout()
        plt.xlabel("Steps", fontsize=15)
        plt.ylabel("Energy (hatree)", fontsize=15)
        plt.title(log.replace(".log",""), fontsize=18)
        plt.show()
        
elif sys.argv[1] == "2":
    final_energies = []
    xs = []
    x = 0
    for log in logs:
        final_energy = GO(log).getFinalEnergy()
        final_energies.append(final_energy)
        xs.append(x)
        x+=1
        
    hatree_energies = np.array(final_energies)
    ev_energies = hatree_energies * 27211.4
    kcal_energies = hatree_energies * 627.503
    kJ_energies = hatree_energies * 2625.5
    relative_energeis = hatree_energies - min(hatree_energies)
    
    data = {}
    data["Energy (hatree)"] = hatree_energies
    data["Energy (meV)"] = ev_energies
    data["Energy (kcal/mol)"] = kcal_energies
    data["Energy (kJ/mol)"] = kJ_energies
    data["Relative (hatree)"] = relative_energeis

    df = pd.DataFrame(data, index=logs)
    df = df[["Relative (hatree)", "Energy (hatree)", "Energy (meV)", "Energy (kcal/mol)", "Energy (kJ/mol)"]]
    print(df[["Relative (hatree)", "Energy (hatree)"]])
    df.to_csv("FinalEnergies.csv")

elif sys.argv[1] == "3":
    for log in logs:
        mygo = GO(log)
        n_atoms, atoms, coords = mygo.getFinalStructure()

        mynpc = npc()
        mynpc.name, mynpc.n_atoms, mynpc.atoms, mynpc.coords = mygo.name, n_atoms, atoms, coords
        mynpc.to_xyzFile()   
        

elif sys.argv[1] == "4":
    for log in logs:
        GO(log).getUVData()

elif sys.argv[1] == "5":
    df_li = []    
    for log in logs:
        orbital_data = {}
        homo, data = GO(log).getOrbitalData()
        orbital_energy = []
        orbital_index = []
        keys = data.keys()
        keys.reverse()
        for key in keys:
            orbital_index.append(data[key][0])
            orbital_energy.append(data[key][1])
        orbital_data[log] = orbital_energy
        df = pd.DataFrame(orbital_data, index=orbital_index)
        df_li.append(df)
        
    df = pd.concat(df_li, axis=1)
    print(df)
    df.to_csv("OrbitalData.csv")
    

elif sys.argv[1] == "6":
    zero_correction_es = []
    thermal_correction_es = []
    thermal_correction_hs = []
    thermal_correction_gs = []
    for log in logs:
        zero_correction_e, thermal_correction_e, thermal_correction_h, thermal_correction_g = GO(log).getThermalData()
        zero_correction_es.append(zero_correction_e)
        thermal_correction_es.append(thermal_correction_e)
        thermal_correction_hs.append(thermal_correction_h)
        thermal_correction_gs.append(thermal_correction_g)

    if len(thermal_correction_es) == 0:
        print("No thermal data.")
        quit()

    data = {}
    data["Zero-point correction Energy (kcal/mol)"] = np.array(zero_correction_es) * 627.503
    data["Energy (kcal/mol)"] = np.array(thermal_correction_es) * 627.503
    data["Enthalpy (kcal/mol)"] = np.array(thermal_correction_hs) * 627.503
    data["Gibbs Free Energy (kcal/mol)"] = np.array(thermal_correction_gs) * 627.503
    data["Entropy (kcal/mol)"] = (data["Enthalpy (kcal/mol)"] - data["Gibbs Free Energy (kcal/mol)"]) / 298.15

    df = pd.DataFrame(data, index=logs)
    df = df[["Zero-point correction Energy (kcal/mol)", "Energy (kcal/mol)", "Gibbs Free Energy (kcal/mol)", "Enthalpy (kcal/mol)", "Entropy (kcal/mol)"]]
    df.to_csv("ThermalData.csv")

    print(df)

elif sys.argv[1] == "7":
    dirs = os.listdir("./")
    all_chks = []
    if "check" in dirs:
        os.chdir("check")

    ask = True
    if "a" in sys.argv:
        ask = False

    input_marker = [".chk"]
    chks = selectInputs(input_marker, "./", ask=ask)

    try:
        no = int(sys.argv[2])
    except:
        no = input("ex) If range : 1 => HOMO, LUMO, H-1, L+1\n Range ? ")

    for chk in chks:
        getCubefile(chk, no)    
    
else:
    print("Unvalid option.")
    shl(sys.argv[1], shell=True)












        
