#!/bin/env python
import os, sys
import numpy as np
import pandas as pd
import pickle

from pymatgen.core.structure import IStructure
from pymatgen.core.lattice import Lattice

from CCpy.Tools.CCpyStructure import PeriodicStructure
from CCpy.Tools.CCpyTools import plt_deco

version = sys.version
if version[0] == '3':
    raw_input = input

try:
    chk = sys.argv[3]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option] [sub_option1] [sub_option2..]")
    print('''--------------------------------------
[Option]
1: Generate shifted CIF files
2: Update final DB with final energies and grouping similar energies
3: Plotting

* Required options : [option] [structure filename] [basename]
    [basename] will be directory where the genereated files located.
    ex) CCpyLayerShift.py 1 Graphene.cif graphene_shift

* Sub options
    <When Option 1 (CIF gen)
    -tol=0.1        : fractional coordinates tolerance for grouping layer (default: 0.01)
    -x=0.05         : fractional shift distance for x-axis. if=0, fix (default: 0.1)
    -y=0.05         : fractional shift distance for y-axis. if=0, fix (default: 0.1)
    -z=0.05         : fractional shift distance for z-axis. if=0, fix (default: set to 0.1 Angstrom (cartesian))
                      2.5 < interlayer distance < 5 (Angstrom)

    <When Option 2 
    -etol = 0.0001      : tolerance energy for grouping structures by similar energy

    <When Option 3 (plot)
    -3d : plot 3d surface
    -2d : plot 2d (useful when check energy via stacking distance)

* Notice before run
- Set vacumm direction to z-axis (c-axis)
- Interlayer distance will be properly calculated when gamma=90

* Example)
<option 1. Generate CIF files>
[user@localhost work]$ CCpyLayerShift.py 1 Graphene.cif graphene_shift -x=0.05 z=0
                       (z-axis will be fixed)

<opiton 2. Analysis after calculation>
[user@localhost work]$ CCpyLayerShift.py 2 Graphene.cif graphene_shift

<option 3. Plotting 3d or 2d>
[user@localhost work]$ CCpyLayerShift.py 3 Graphene.cif graphene_shift -3d
[user@localhost work]$ CCpyLayerShift.py 3 Graphene.cif graphene_shift -2d
  
    '''
          )
    quit()


if not sys.argv[1].isdigit():
    print("First option must be as integer.")
    quit()
filename = sys.argv[2]
basename = sys.argv[3]
dbname = basename + "_DB.csv"
final_dbname = basename + "_DB_final.csv"
sorted_final_dbname = basename + "_DB_final_sorted.csv"


# ---------- Parsing structure file                   
read_structure = PeriodicStructure(filename=filename)
atoms, fcoords, lattice_v = read_structure.cifFile()
lat_param = read_structure.latticeGen()



def cif_gen():
    # --------- Configurations
    layer_tol = 0.01
    x_ratio = 0.1
    y_ratio = 0.1
    z_ratio = round(0.1 / lat_param[2], 8)
    for s in sys.argv:
        if "-tol=" in s:
            layer_tol = float(s.replace("-tol=",""))
        elif "-x" in s:
            x_ratio = float(s.replace("-x=",""))
        elif "-y" in s:
            y_ratio = float(s.replace("-y=",""))
        elif "-z" in s:
            z_ratio = float(s.replace("-z=",""))


    # ---------- Distinct layer
    '''
    # Old version..
    layer1_index = []               # indice of 1st layer
    layer2_index = []               # indice of 2nd layer
    layer1_z = None                 # z-coordinate of 1st layer
    layer2_z = None                 # z-coordinate of 2nd layer

    sum_layer1_z = 0
    sum_layer2_z = 0

    for i in range(len(fcoords)):
        if i == 0:
            layer1_z = fcoords[i][2]        # set z-coordinate of first line atom to layer1
            layer1_index.append(i)
            sum_layer1_z += fcoords[i][2]
        else:
            if layer1_z - layer_tol < fcoords[i][2] < layer1_z + layer_tol:
                layer1_index.append(i)
                sum_layer1_z += fcoords[i][2]
            elif layer2_z == None:
                layer2_z = fcoords[i][2]
                layer2_index.append(i)
                sum_layer2_z += fcoords[i][2]
            elif layer2_z - layer_tol < fcoords[i][2] < layer2_z + layer_tol:
                layer2_index.append(i)
                sum_layer2_z += fcoords[i][2]
            else:
                print("ERROR:: Cannot distinct 2-layered system.")
                print("Try again with increase tolerance. Or set slab direction to z-axis(c dierction).")
                quit()
    avg_layer1_z = sum_layer1_z / len(layer1_index)
    avg_layer2_z = sum_layer2_z / len(layer2_index)
    layer_distance = round(np.dot((avg_layer1_z - avg_layer2_z), lattice_v)[2][2], 4)
    layer_distance = abs(layer_distance)
    '''
    # New version
    layer1_index = []               # indice of 1st layer
    layer2_index = []               # indice of 2nd layer

    sum_layer1_z = 0
    sum_layer2_z = 0
    criteria_z = float(raw_input("Criterial fractional coordinate of z- (ex: 0.5)\n:"))
    for i in range(len(fcoords)):
        if fcoords[i][2] >= criteria_z:
            layer1_index.append(i)
            sum_layer1_z += fcoords[i][2]
        else:
            layer2_index.append(i)
            sum_layer2_z += fcoords[i][2]
    avg_layer1_z = sum_layer1_z / len(layer1_index)
    avg_layer2_z = sum_layer2_z / len(layer2_index)
    layer_distance = round(np.dot((avg_layer1_z - avg_layer2_z), lattice_v)[2][2], 4)
    layer_distance = abs(layer_distance)


    # ---------- Set layer1 as upper layer
    if avg_layer2_z > avg_layer1_z:
        tmp_index = layer1_index
        layer1_index = layer2_index
        layer2_index = tmp_index



    # ---------- Shift range
    x_shift = []
    y_shift = []
    z_shift = []
    total_x_shift = 0
    if x_ratio == 0:
        x_shift.append(0)
    else:
        while total_x_shift <= 1:    
            total_x_shift = round(total_x_shift, 8)
            if total_x_shift < 1:
                x_shift.append(total_x_shift)
            total_x_shift += x_ratio

    total_y_shift = 0        
    if y_ratio == 0:
        y_shift.append(0)
    else:
        while total_y_shift <= 1:    
            total_y_shift = round(total_y_shift, 8)
            if total_y_shift < 1:
                y_shift.append(total_y_shift)
            total_y_shift += y_ratio

    if z_ratio == 0:
        z_shift.append(0)
    else:
        # -- for z-axis: 2.5 <  d  < 5.0
        frac_layer_distance = abs(avg_layer1_z - avg_layer2_z)
        min_distance = 2.5 / lat_param[2]
        max_distance = 5.0 / lat_param[2]
        total_z_shift = 0.0
        # -- closer
        while frac_layer_distance - total_z_shift > 2.5 / lat_param[2]:
            total_z_shift = round(total_z_shift, 8)
            z_shift.append(total_z_shift)
            total_z_shift += z_ratio
        z_shift.reverse()
        total_z_shift = 0.0
        # -- farther
        while frac_layer_distance + total_z_shift < 5 / lat_param[2]:
            total_z_shift = round(total_z_shift, 8)
            if -total_z_shift not in z_shift:
                z_shift.append(-total_z_shift)
            total_z_shift += z_ratio

    print("Total number of structures: " + str(len(x_shift) * len(y_shift) * len(z_shift)))

    # ----- fractional coordiantes shift
    def get_shifted_structure(original_fcoords=None, layer_index=None, axis=None, shift_val=None):
        """
        order of atoms == order of original_foords
        axis = 0, 1, 2 for x, y, z
        shift_range (fractional) = [0.1, 0.2, ...]  (0 < value < 1)

        return shifted fractional coordinates
        """
        shifted_fcoords = np.array(original_fcoords, copy=True)

        for i in layer_index:
            shifted_fcoords[i][axis] = original_fcoords[i][axis] + shift_val
        
            
        return shifted_fcoords


    # ------ main
    os.mkdir(basename)
    os.chdir(basename)
    db = {'filename': [], 'x index': [], 'y index': [], 'z index':[],
          'shifted frac x': [], 'shifted frac y': [], 'shifted frac z': [], 'stacking distance': []}
    xcnt = 0
    for x in x_shift:
        x_shifted_fcoords = get_shifted_structure(original_fcoords=fcoords, layer_index=layer2_index, axis=0, shift_val=x)
        ycnt = 0
        for y in y_shift:
            xy_shifted_fcoords = get_shifted_structure(original_fcoords=x_shifted_fcoords, layer_index=layer2_index, axis=1, shift_val=y)
            zcnt = 0
            for z in z_shift:
                xyz_shifted_fcoords = get_shifted_structure(original_fcoords=xy_shifted_fcoords, layer_index=layer2_index, axis=2, shift_val=z)
                filename = (basename + "_x%3d" % xcnt + "y%3d" % ycnt + "z%3d" % zcnt).replace(" ","0") + ".cif"
                pmg_structure = IStructure(lattice_v, atoms, xyz_shifted_fcoords)
                pmg_structure.to(fmt="cif", filename=filename)
                db['filename'].append(filename)
                db['x index'].append(xcnt)
                db['y index'].append(ycnt)
                db['z index'].append(zcnt)
                db['shifted frac x'].append(x)
                db['shifted frac y'].append(y)
                db['shifted frac z'].append(z)

                # inter layer distance
                shifted_layer_distance = round(np.dot(z, lattice_v)[2][2], 4)
                stacking_distance = layer_distance - shifted_layer_distance
                db['stacking distance'].append(stacking_distance)
                zcnt += 1
            ycnt += 1
        xcnt += 1

    # ------ save db info
    df = pd.DataFrame(db)
    df.to_csv("../" + dbname)
    print("* DB information has been saved : '%s', DO NOT REMOVE THIS FILE." % dbname)


def get_final_energies():
    # concat original DB & energy DB
    DB_df = pd.read_csv(dbname)
    os.chdir(basename)
    os.system("CCpyVASPAnal.py 2 n")
    energy_df = pd.read_csv("03_%s_FinalEnergies.csv" % basename)
    #energy_df = pd.read_csv("bilayer_scripts_FinalEnergies.csv")
    DB_df['final energy (eV)'] = energy_df['Total energy (eV)']
    DB_df['relative energy (eV)'] = DB_df['final energy (eV)'] - DB_df['final energy (eV)'].min()
    sorted_df = DB_df.sort_values(by='final energy (eV)')
    os.chdir("../")


    # -- grouping by simiarl energy
    etol = 0.001
    for s in sys.argv:
        if "-etol" in s:
            etol = float(s.replace("-etol",""))
    group_index = []
    gi = 1
    former_e = None
    for e in sorted_df['relative energy (eV)']:
        if not former_e:
            former_e = e
            group_index.append(gi)
        else:
            if e - former_e > etol:
                gi += 1
                former_e = e
            group_index.append(gi)
    sorted_df['group'] = group_index

    DB_df.to_csv(final_dbname, index=False)
    sorted_df.to_csv(sorted_final_dbname, index=False)
    

    return sorted_df
    

def get_3d_plot(df, plot_group=False):
    import matplotlib
    from matplotlib import rc
    import matplotlib.pyplot as plt
    from matplotlib import colors, ticker, cm, rc
    from mpl_toolkits.mplot3d import Axes3D
    
    xy = ['x', 'y']
    for axis in xy:
        index_name = axis + " index"
        index = []
        for v in df[index_name]:
            if v not in index:
                index.append(v)
        if len(index) == 1:
            print("Not proper data to plot 3d surface")
            quit()

    # ------ choose z-index to make 3d plot of x,y
    fixed_axis_df = get_fixed_axis_df(df, 'z')

    # ------ lowest group
    grp1_df = fixed_axis_df[(fixed_axis_df['group'] == 1)]
    grp2_df = fixed_axis_df[(fixed_axis_df['group'] == 2)]
    grp3_df = fixed_axis_df[(fixed_axis_df['group'] == 3)]
    grp4_df = fixed_axis_df[(fixed_axis_df['group'] == 4)]
    grp5_df = fixed_axis_df[(fixed_axis_df['group'] == 5)]
    grp1_x = grp1_df['shifted frac x']
    grp1_y = grp1_df['shifted frac y']
    grp2_x = grp2_df['shifted frac x']
    grp2_y = grp2_df['shifted frac y']
    grp3_x = grp3_df['shifted frac x']
    grp3_y = grp3_df['shifted frac y']
    grp4_x = grp4_df['shifted frac x']
    grp4_y = grp4_df['shifted frac y']
    grp5_x = grp5_df['shifted frac x']
    grp5_y = grp5_df['shifted frac y']



    # ------ make 3d data format
    fixed_axis_df['relative energy (meV)'] = fixed_axis_df['relative energy (eV)'] * 1000
    plot_df = fixed_axis_df.pivot('shifted frac y', 'shifted frac x', 'relative energy (meV)')

    plot_df.to_csv("%s_3dplot.csv" % basename, index=False)
    

    # ------ make 3d plot
    fig = plt_deco(7, 5)
    # fig = plt.figure(figsize=(7,5))
    
    X = plot_df.columns.values
    Y = plot_df.index.values
    Z = plot_df.values
    
    x_label = r'$\Delta$ x'
    y_label = r'$\Delta$ y'

    plt.xlabel(x_label)
    plt.ylabel(y_label)

    # x, y
    if plot_group:
        plt.plot(grp1_x, grp1_y, marker="o", color="w", lw=0)
        plt.plot(grp2_x, grp2_y, marker="<", color="y", lw=0)
        plt.plot(grp3_x, grp3_y, marker="s", color="c", lw=0)
        plt.plot(grp4_x, grp4_y, marker="D", color="g", lw=0)
        plt.plot(grp5_x, grp5_y, marker="x", color="r", lw=0)

    # -- make levels
    unit = (abs(Z.min() - Z.max())) / 50
    lev = []
    lev_val = Z.min()
    while lev_val < Z.max() - 0.3:
        lev_val = round(lev_val, 2)
        lev.append(lev_val)
        lev_val += unit
    
    plt.contourf(X, Y, Z, cmap=cm.jet, levels=lev, extend="both")
    plt.colorbar()
    plt.tight_layout()

    plt.show()


def get_2d_plot(df):
    import matplotlib.pyplot as plt
    # ------ choose z-index to make 3d plot of x,y
    fixed_axis_df = get_fixed_axis_df(df, 'x')
    fixed_axis_df = get_fixed_axis_df(fixed_axis_df, 'y')

    x = fixed_axis_df['stacking distance']
    y = fixed_axis_df['final energy (eV)']
    #x_label = r"$\Delta$ z"
    x_label = r"Stacking distance ($\mathrm{\AA}$)"
    y_label = "Energy (eV)"

    plt.xlabel(x_label, fontsize=19)
    plt.ylabel(y_label, fontsize=19)
    plt.tick_params(axis='both', which='major', labelsize=12)

    plt.plot(x, y, color="b", marker="o", lw=1.5)
    plt.tight_layout()
    plt.show()


def get_fixed_axis_df(df, axis):
    """
    df: dbname
    axis: "x" or "y" or "z"
    return fixed axis index
    """
    # -- ex) choose z-index to make 3d plot of x,y
    index_name = axis + " index"
    index = []
    for v in df[index_name]:
        if v not in index:
            index.append(v)
    print(index_name + " list")
    for i in index:
        print(i)
    if len(index) == 1:
        fixed_axis_index = index[0]
    else:
        fixed_axis_index = int(raw_input("\n* Pick index of " + axis + "-axis to fix (ex: 3)\n: "))
        
    fixed_axis_df = df[(df[index_name] == int(fixed_axis_index))]
    return fixed_axis_df
                                         

if __name__ == "__main__":
    if sys.argv[1] == "1":
        cif_gen()
    elif sys.argv[1] == "2":
        if dbname not in os.listdir("./"):
            print("Cannot find '%s' file." % dbname)
            quit()
        elif final_dbname in os.listdir("./"):
            print("%s file already exist. If you want to re-anlayze, remove it and try again." % final_dbname)
        else:
            df = get_final_energies()
    elif sys.argv[1] == "3":
        if sys.argv[4] == "-3d":
            if sorted_final_dbname in os.listdir("./"):
                df = pd.read_csv(sorted_final_dbname)
            else:
                df = get_final_energies()
            group = False
            if '-group' in sys.argv:
                group = True
            get_3d_plot(df, plot_group=group)
        elif sys.argv[4] == "-2d":
            if final_dbname not in os.listdir("./"):
                df = get_final_energies()
            else:
                df = pd.read_csv(final_dbname)
            df = df.sort_values(by='stacking distance')            
            get_2d_plot(df)
























