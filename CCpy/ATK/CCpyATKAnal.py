#!/bin/env python
import os, sys
import re
import matplotlib
from matplotlib import colors, ticker, cm, rc, style
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import tables as tb
import warnings
warnings.filterwarnings("ignore")
from CCpy.Tools.CCpyTools import selectInputs


def transmission_3d_hdf5(hdf5_filename, trans_max=False):
    root_name = hdf5_filename.replace('.hdf5', '')
    
    h5file = tb.open_file(hdf5_filename, "a")
    raw_data = {}
    # e_val:: energies // t_val:: transmission data // bias
    energies = None
    max_t = 0
    for node in h5file:
        nodes = str(node)
        # -- parsing energy (energy values identical in case of all other voltages)
        if "BaseTransmissionSpectrum/energies/array/data" in nodes:
            voltage = float(nodes.split('/')[1].split()[0].replace('trans', ''))
            if voltage not in raw_data.keys():
                raw_data[voltage] = {}
            raw_data[voltage]['e_val'] = node.read()
            energeis = node.read()
        # -- parsing transmission
        elif "transmission/0/data" in nodes:
            voltage = float(nodes.split('/')[1].split()[0].replace('trans', ''))
            if voltage not in raw_data.keys():
                raw_data[voltage] = {}
            val = node.read()
            val = val.squeeze()
            raw_data[voltage]['t_val'] = val
            for v in val:
                max_t = max(max_t, v)
        # -- parsing bais
        elif "electrode_voltages/array/data" in nodes:
            voltage = float(nodes.split('/')[1].split()[0].replace('trans', ''))
            if voltage not in raw_data.keys():
                raw_data[voltage] = {}
            raw_data[voltage]['bias'] = node.read()

    data = {}
    for key in raw_data.keys():
        data['energy'] = raw_data[key]['e_val']
        data[key] = raw_data[key]['t_val']

    df = pd.DataFrame(data)
    df.set_index('energy', inplace=True)

    df.to_csv(root_name + "_Transmission.csv")
    print("\"" + root_name + "_Transmission.csv\" has been saved.")

    # ---- plotting
    # -- convert matrix form
    dat_mat = df.as_matrix()
    convert_mat = []
    for i in range(len(dat_mat[0])):
        tmp = []
        for j in dat_mat:
            tmp.append(j[i])
        convert_mat.append(tmp)
    convert_mat = np.array(convert_mat)

    # -- set level
    if trans_max:
        trans_max = trans_max
    else:
        trans_max = max_t

    lev = [round(f, 2) for f in np.linspace(0, trans_max, 50)]

    # -- bias
    bias = max(df.keys())
    plt.plot([0, bias / 2], [0, bias], color='w', ls='--', lw=2)
    plt.plot([0, -bias / 2], [0, bias], color='w', ls='--', lw=2)

    # -- color map
    own_cmap = cusmtom_cmap()    
    #plt.contourf(df.index, df.keys(), convert_mat, cmap=cm.gist_ncar, levels=lev, extend="both")
    plt.contourf(df.index, df.keys(), convert_mat, cmap=own_cmap, levels=lev, extend="both")
    cbar = plt.colorbar()

    cbar.ax.tick_params(labelsize=14)

    csfont = {'fontname':'Times New Roman'}
    plt.xlabel("Energy (eV)", fontsize=24, weight='bold', **csfont)
    plt.ylabel("Applied Voltage(V)", fontsize=24, weight='bold', **csfont)
    plt.tick_params(axis='both', which='major', labelsize=14)

    plt.tight_layout()
    plt.savefig(root_name + "_3d-Transmission.png", dpi=500)
    print("\"" + root_name + "_3d-Transmission.png\" has been saved.")
    
    plt.show()

def cusmtom_cmap():
    import pickle
    #with open("/home/shared/GitHub/CCpy/CCpy/Tools/ATK_transmission_cmap.pkl", "rb") as mydata:
    with open("/home/shared/GitHub/CCpy/CCpy/Tools/ATKsuleecmap.pkl", "rb") as mydata:
        custom_cmap = pickle.load(mydata, encoding='latin1')


    cm = matplotlib.colors.ListedColormap(custom_cmap / 255.0)
    return cm

    

if __name__ == '__main__':
    try:
        chk = sys.argv[1]
    except:
        print('''
            CCpyATKAnal.py [1]   :   Show IV Curve (Bias voltage, Gate voltage
            CCpyATKAnal.py [2]   :   Show 2d Transmission Spectrum  (Bias voltage)
            CCpyATKAnal.py [3]   :   Show 3d Transmission Spectrum  (Bias voltage)
                <sub option>
                CCpyATKAnal [3] [value] : set z-value limitation
                ex) CCpyATKAnal 3 0.6
            '''
              )
        quit()

    if chk == '3':
        input_file = selectInputs(['.hdf5'], './')
        if len(input_file) != 1:
            print('Only single file available.')
            quit()
        z_limit = False
        if len(sys.argv) == 3:
            z_limit = float(sys.argv)
        transmission_3d_hdf5(input_file[0], z_limit)

