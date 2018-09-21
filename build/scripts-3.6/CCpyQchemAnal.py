#!/home/shared/anaconda3/envs/CCpy_tmp/bin/python

import os, sys
from subprocess import call as shl
import pandas as pd

from CCpy.Qchem.Qchemio import QchemOutput as QO
from CCpy.Qchem.Qchemio import calcMobility
from CCpy.Tools.CCpyTools import selectInputs

"""
This script is for analysing the output files of Q-chem calculation.

Fisrt, do option [1].
This command will collect charge transfer integrals of output file in "00Data_[directoryname].csv"

Next, do option [2].
This command will calculate mobility using
charge transfer integrals, center-to-center distance and reorganization energy.
You can put in the reorganization energy on running this script but the script also can read the "ROE.dat" file
(example of ROE.dat)
Lambd_hole    0.16306433049542213
Lambd_elec    0.27177007413795184
"""

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option]")
    print('''--------------------------------------
[1] : Get charge transfer integrals
[2] : Calculate mobility (using ROE)
* end with \"a\" covers all subdirectories
* ROE.dat required option \"2 a\"'''
          )
    quit()

ask = True
if "a" in sys.argv:
    ask = False

if sys.argv[1] == "1":
    output_marker = [".out"]
    all_files = selectInputs(output_marker, "./", ask=ask)

    hole_files = []
    elec_files = []

    for each_file in all_files:
        if "_H.out" in each_file:
            hole_files.append(each_file)
        elif "_E.out" in each_file:
            elec_files.append(each_file)

    hole_files.sort()
    elec_files.sort()

    hole_vs = []
    elec_vs = []

    for hole in hole_files:
        v = QO(hole).getChargetransferIntegral()
        hole_vs.append(v)

    for elec in elec_files:
        v = QO(elec).getChargetransferIntegral()
        elec_vs.append(v)

    pwd = os.getcwd()
    pwd = pwd.split("/")[-1]
    datafile = "00Data_"+pwd+".csv"

    try:
        df = pd.read_csv(datafile, index_col=0)
    except:
        try:
            df = pd.read_csv(".00Data_"+pwd+".csv", index_col=0)
        except:
            print("Data file doesn't exist. Please retry generate input file process to make .csv file")
            quit()

    try:
        hole_inputs = df['Hole Files']
        elec_inputs = df['Elec Files']
    except:
        print("You already performed this step or .csv file has been modified.")
        quit()
    names = []
    for i in range(len(hole_inputs)):
        if hole_inputs[i].replace(".in","") == hole_files[i].replace(".out","") and elec_inputs[i].replace(".in","") == elec_files[i].replace(".out",""):
            names.append(hole_inputs[i].replace("_H.in",""))
        else:
            print(hole_inputs[i].replace(".in","") , elec_files[i].replace(".out",""))
    df['Name'] = names
    df['Charge transfer intergral (hole)'] = hole_vs
    df['Charge transfer intergral (elec)'] = elec_vs
    df = df[['Name','Distance', 'Charge transfer intergral (hole)', 'Charge transfer intergral (elec)']]

    df.to_csv("00Data_"+pwd+".csv")

if sys.argv[1] == "2":
    pwd = os.getcwd()
    pwd = pwd.split("/")[-1]
    datafile = "00Data_"+pwd+".csv"
    try:
        df = pd.read_csv(datafile, index_col=0)
    except:
        print("Datafile doesn't exist : "+datafile)
    try:
        hole_vs = df['Charge transfer intergral (hole)']
    except:
        print("Perform option [1] first.")
        quit()

    try:
        f = open("ROE.dat", "r")
        lines = f.readlines()
        f.close()
        hole_roe = float(lines[0].split()[-1])
        elec_roe = float(lines[1].split()[-1])
    except:
        try:
            hole_roe = float(sys.argv[2])
            elec_roe = float(sys.argv[3])
        except:
            hole_roe = float(raw_input("Hole ROE : "))
            elec_roe = float(raw_input("Elec ROE : "))

    

    hole_V = df['Charge transfer intergral (hole)']
    elec_V = df['Charge transfer intergral (elec)']
    r = df['Distance']    

    df['DC (hole)'], df['Mobility (hole)'] = calcMobility(hole_V, r, hole_roe)
    df['DC (elec)'], df['Mobility (elec)'] = calcMobility(elec_V, r, elec_roe)

    df = df[['Name', 'Distance', 'Charge transfer intergral (hole)', 'Charge transfer intergral (elec)',
             'DC (hole)', 'DC (elec)', 'Mobility (hole)', 'Mobility (elec)']]

    df.to_csv(datafile)
    
