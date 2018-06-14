#!/usr/bin/env python
# -*- coding:utf-8 -*-

import tables as tb
import os ,sys ,re
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib import colors, ticker, cm, rc

__author__ = "Hoijung Jung"

def print_list(list_):
    print("0  :  All file")
    for i in range(0,len(list_)):
        print( i +1  ," : ", os.path.split(list_[i])[-1])

def search_2(word_,path_):
    list_word=[]
    try:
        x = os.listdir(path_)
        for xx in x:
            full_name_1 = os.path.join(path_, xx)
            if os.path.isdir(full_name_1):
                continue
            else:
                file_name = os.path.splitext(full_name_1)[-1]
                if file_name == word_:
                    list_word.append(full_name_1)
        return list_word
    except PermissionError:
        pass


def str2int(strr):
    sel = strr.split(",")
    sel_list = []
    for j in sel:
        if isNumber(j):
            sel_list.append(int(j))
        elif "-" in j:
            lk = j.split("-")
            if len(lk) != 2:
                print("-을 잘못쓰신것같은데요?")
                break
            for ll in (range(0, int(lk[-1]) - int(lk[0]) + 1)):
                sel_list.append(int(lk[0]) + int(ll))
    if 0 in sel_list:
        sel_list="all"
    #여기서 나오는 값들은 모두 -1을 해줘야지 실제 list와 같아짐
    return sel_list

def isNumber(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

def IVCurve(item):
    h5file = tb.open_file(item, "a")

    t_val = []
    # xx=currents,t_val=electrode_voltage
    for node in h5file:
        nodes = str(node)
        if "/IVCurve_0/currents" in nodes:
            if "array/data" in nodes:
                xx = node
        if "/IVCurve_0/transmissions" in nodes:
            if "electrode_voltages/array/data" in nodes:
                t_val.append(node)

    c_val = xx.read()

    # currents의 값을 얻는 것
    for node in h5file:
        nodes = str(node)
        if "/IVCurve_0/biases" in nodes:
            if "array/data" in nodes:
                yy = node

    biass = yy.read()

    #all은 final list, 여기에 각각 좌우전압과, 순서를 정한다
    all = []
    for i in range(0, len(t_val)):
        mid_ = []
        sece = str(i).split("/")
        for j in sece:
            if isNumber(j):
                mid_.append(int(j))
        tt = t_val[i].read()
        mid_.append(tt[0])
        mid_.append(tt[1])
        all.append(mid_)

    #currensts 에 실제 전류값을 저장
    currents = []
    for i in range(0, len(c_val)):
        currents.append(c_val[i])

    #bias에 실제 전압을 저장
    bias = []
    for j in range(0, len(biass)):
        bias.append(float(biass[j]))

    sort_currents=[]

    for j in range(0,len(bias)):
        mid_=[]
        mid_.append(currents[j])
        mid_.append(bias[j])
        sort_currents.append(mid_)

    bias.sort()
    real_current=[]
    for j in bias:
        for jt in range(0,len(bias)):
            if j == sort_currents[jt][1]:
                real_current.append(sort_currents[jt][0])

    gate_voltages=bias

    f = open(item + "_IV.csv", "w")
    f.write("voltage,current\n")
    for i in range(len(gate_voltages)):
        f.write(str(gate_voltages[i]) + "," + str(real_current[i]) + "\n")
    f.close()
    print("\"" + item + "_IV.csv\" has been saved.")

    plt.xlabel("Voltage (V)", fontsize=15)
    plt.ylabel(r"Current ($\mu$A)", fontsize=15)
    plt.plot(gate_voltages, real_current, marker='o')
    plt.tight_layout()
    plt.savefig(item + "_IV.png", dpi=500)
    print("\"" + item + "_IV.png\" has been saved.")
    plt.show()

def transmission_2D(item):
    h5file = tb.open_file(item, "a")
    t_val=[]
    e_val=[]

    #e_val에는 energies 를 , t_vasl 에는 transmission data를 넣는다
    for node in h5file:
        nodes = str(node)
        if "/IVCurve_0/transmissions" in nodes:
            if "BaseTransmissionSpectrum/energies/array/data" in nodes:
                e_val.append(node)
            elif "transmission/0/data" in nodes:
                t_val.append(node)

    # bias의 값을 얻는 것
    for node in h5file:
        nodes = str(node)
        if "/IVCurve_0/biases" in nodes:
            if "array/data" in nodes:
                yy = node

    #bias값중에서 원하는 voltage 을 선택해주는 부분
    biass = yy.read()
    bias = []
    for j in range(0, len(biass)):
        bias.append(float(biass[j]))

    # transmission 에서 path 00인것을 찾는데스웅
    for node in h5file:
        nodes = str(node)
        if "/IVCurve_0/transmissions" in nodes:
            if "/BaseTransmissionSpectrum/path/data" in nodes:
                pp = node

    ppp = pp.read()
    KPP = 0
    for j in range(0, len(ppp)):
        if str(ppp[j][0]) == "0.0" and str(ppp[j][1]) == "0.0" and str(ppp[j][2]) == "0.0":
            KPP = j

    #transvalue에서 value을 얻는닷 그전에 어떤것이 na,nd 에서 00 인지를 판단해야됨
    #ala_ 앞에는 voltage, 뒤에는 path0,0 인상태의 값
    ala = []
    for i in range(0, len(t_val)):
        mid_ = []
        sece = str(i).split("/")
        for j in sece:
            if isNumber(j):
                mid_.append(int(j))
        tt = t_val[i].read()
        mid_.append(tt[:])
        ala.append(mid_)

    # E value get but it all same so we using only 0 value ==ale[0][1]
    ale = []
    for i in range(0, len(e_val)):
        mid_ = []
        sece = str(i).split("/")
        for j in sece:
            if isNumber(j):
                mid_.append(int(j))
        ee = e_val[i].read()
        mid_.append(ee[:])
        ale.append(mid_)

    # all E_value get
    sss = np.shape(ale[0][1])
    E_value = []
    for e in range(0, sss[0]):
        E_value.append(ale[0][1][e])

    #final_t의 맨처음에는 순서가, 그다음에는 trans value 가 들어가짐
    final_t = []
    for a in range(0, len(ala)):
        mid__ = []
        mid__.append(ala[a][0])
        for b in range(0, len(ala[a][1])):
            mid__.append(ala[a][1][b][KPP])
        final_t.append(mid__)

    #final_t의 맨처음숫자를 bias 로 바꿔줌
    for ii in range(0, len(ala)):
        final_t[ii][0] = bias[ii]

    #그후 bias를 sort 시킨후에, final_sort 도 똑같이 sort 시켜줌
    bias.sort()
    final_sort = []
    for jj in range(0, len(final_t)):
        for kk in final_t:
            if bias[jj] == kk[0]:
                final_sort.append(kk)

    for i in range(0, len(bias)):
        print(i, " : ", bias[i])

    tyty=input("please select the Number  :  ")

    sel = tyty.split(",")
    sel_list = []
    for j in sel:
        if isNumber(j):
            sel_list.append(int(j))
        elif "-" in j:
            lk = j.split("-")
            if len(lk) != 2:
                print("-을 잘못쓰신것같은데요?")
                break
            for ll in (range(0, int(lk[-1]) - int(lk[0]) + 1)):
                sel_list.append(int(lk[0]) + int(ll))

    tmp=sel_list

    #일단 정리해서 저장해둠
    ccss = open(item + "_trans.csv", "w")

    for i in range(0, len(E_value)):
        tot = ""
        if i == 0:
            tot += "energy"
            for j in range(0, len(ala)):
                tot += "," + str(bias[j])
            tot += "\n"
            ccss.write(tot)
        else:
            tot += str(E_value[i - 1])
            for j in range(0, len(final_t)):
                tot += "," + str(final_sort[j][i])
            tot += "\n"
            ccss.write(tot)

    ccss.close()
    #pandas를 만들음

    f = open(item + "_trans.csv", "r")
    fa = f.readlines()
    fa_z = fa[0].split(",")
    del fa_z[0]
    fa_z[-1] = fa_z[-1][:-2]

    data = {}
    data["energy"] = E_value
    for fs in range(0, len(final_sort)):
        data[float(final_sort[fs][0])] = final_sort[fs][1:]

    df = pd.DataFrame(data)
    df.set_index('energy', inplace=True)

    indice = []
    for i in tmp:
        indice.append(df.keys()[int(i)])
    df[indice].plot(lw=1.5)

    trans_max = False
    if trans_max:
        plt.ylim(0, float(trans_max))
    plt.ylabel("Transmission", fontsize=20)
    plt.xlabel("Energy", fontsize=20)
    plt.axvline(x=0, color="k", ls='--')
    plt.legend(loc=9, prop={'size': 14})
    plt.show()

    plt.savefig(item + "_2d-Transmission.png")
    print("\"" + item + "_2d-Transmission.png\" has been saved.")

def transmission_3D(item):
    h5file = tb.open_file(item, "a")
    t_val=[]
    e_val=[]

    #e_val에는 energies 를 , t_vasl 에는 transmission data를 넣는다
    for node in h5file:
        nodes = str(node)
        if "/IVCurve_0/transmissions" in nodes:
            if "BaseTransmissionSpectrum/energies/array/data" in nodes:
                e_val.append(node)
            elif "transmission/0/data" in nodes:
                t_val.append(node)

    # bias의 값을 얻는 것
    for node in h5file:
        nodes = str(node)
        if "/IVCurve_0/biases" in nodes:
            if "array/data" in nodes:
                yy = node

    #bias값중에서 원하는 voltage 을 선택해주는 부분
    biass = yy.read()
    bias = []
    for j in range(0, len(biass)):
        bias.append(float(biass[j]))

    # transmission 에서 path 00인것을 찾는데스웅
    for node in h5file:
        nodes = str(node)
        if "/IVCurve_0/transmissions" in nodes:
            if "/BaseTransmissionSpectrum/path/data" in nodes:
                pp = node

    ppp = pp.read()
    KPP = 0
    for j in range(0, len(ppp)):
        if str(ppp[j][0]) == "0.0" and str(ppp[j][1]) == "0.0" and str(ppp[j][2]) == "0.0":
            KPP = j

    #transvalue에서 value을 얻는닷 그전에 어떤것이 na,nd 에서 00 인지를 판단해야됨
    #ala_ 앞에는 voltage, 뒤에는 path0,0 인상태의 값
    ala = []
    for i in range(0, len(t_val)):
        mid_ = []
        sece = str(i).split("/")
        for j in sece:
            if isNumber(j):
                mid_.append(int(j))
        tt = t_val[i].read()
        mid_.append(tt[:])
        ala.append(mid_)

    # E value get but it all same so we using only 0 value ==ale[0][1]
    ale = []
    for i in range(0, len(e_val)):
        mid_ = []
        sece = str(i).split("/")
        for j in sece:
            if isNumber(j):
                mid_.append(int(j))
        ee = e_val[i].read()
        mid_.append(ee[:])
        ale.append(mid_)

    # all E_value get
    sss = np.shape(ale[0][1])
    E_value = []
    for e in range(0, sss[0]):
        E_value.append(ale[0][1][e])

    #final_t의 맨처음에는 순서가, 그다음에는 trans value 가 들어가짐
    trans_max=0
    final_t = []
    for a in range(0, len(ala)):
        mid__ = []
        mid__.append(ala[a][0])
        for b in range(0, len(ala[a][1])):
            if ala[a][1][b][KPP] > trans_max:
                trans_max=ala[a][1][b][KPP]
            mid__.append(ala[a][1][b][KPP])
        final_t.append(mid__)

    #final_t의 맨처음숫자를 bias 로 바꿔줌
    for ii in range(0, len(ala)):
        final_t[ii][0] = bias[ii]

    #그후 bias를 sort 시킨후에, final_sort 도 똑같이 sort 시켜줌
    bias.sort()
    final_sort = []
    for jj in range(0, len(final_t)):
        for kk in final_t:
            if bias[jj] == kk[0]:
                final_sort.append(kk)

    #일단 정리해서 저장해둠
    ccss = open(item + "_trans.csv", "w")

    for i in range(0, len(E_value)):
        tot = ""
        if i == 0:
            tot += "energy"
            for j in range(0, len(ala)):
                tot += "," + str(bias[j])
            tot += "\n"
            ccss.write(tot)
        else:
            tot += str(E_value[i - 1])
            for j in range(0, len(final_t)):
                tot += "," + str(final_sort[j][i])
            tot += "\n"
            ccss.write(tot)

    ccss.close()
    #pandas를 만들음

    f = open(item + "_trans.csv", "r")
    fa = f.readlines()
    fa_z = fa[0].split(",")
    del fa_z[0]
    fa_z[-1] = fa_z[-1][:-2]

    data = {}
    data["energy"] = E_value
    for fs in range(0, len(final_sort)):
        if float(final_sort[fs][0]) < 0:
            continue
        data[float(final_sort[fs][0])] = final_sort[fs][1:]


    lev=[]
    val=0.0

    for i in range(int(round(float(trans_max), 0)) * 5):
        val = i * 0.2
        lev.append(val)

    max_bias=float(bias[-1])

    df = pd.DataFrame(data)
    df.set_index('energy', inplace=True)

    df.to_csv(item + "_Transmission.csv")
    print("\"" + item + "_Transmission.csv\" has been saved.")

    dat_mat = df.as_matrix()
    convert_mat = []
    for i in range(len(dat_mat[0])):
        tmp = []
        for j in dat_mat:
            tmp.append(j[i])
        convert_mat.append(tmp)
    convert_mat = np.array(convert_mat)

    plt.plot([0, max_bias/2], [0, max_bias], color='w', ls='--', lw=2.0)
    plt.plot([0, -max_bias/2], [0, max_bias], color='w', ls='--', lw=2.0)
    plt.contourf(df.index, df.keys(), convert_mat, cmap=cm.jet, levels=lev, extend="both")
    plt.colorbar()
    plt.xlabel("Energy (eV)", fontsize=14)
    plt.ylabel("Applied Voltage(V)", fontsize=14)

    plt.savefig(item + "_3d-Transmission.png", dpi=500)
    print("\"" + item + "_3d-Transmission.png\" has been saved.")

    plt.show()


#--------------------------------------------------------------------------------

if not len(sys.argv) == 2:
    print("옵션을 추가해주세요")
    print("1 = IV curve")
    print("2 = 2Dtransmission plot")
    print("3 = 3Dtransmission plot")
    sys.exit()

list_hdf5=search_2(".hdf5",os.getcwd())

print_list(list_hdf5)
input_xx=input("원하시는 값을 입력해주세요 (여러개는 , 이어지는 여러숫자들은 - 을 이용해주세요")

print("당신이 선택하신 list는 " ,input_xx ," 입니다. ")

if sys.argv[1]=='1':
    if input_xx=="all":
        for i in list_hdf5:
            IVCurve(i)
    else:
        for i in input_xx:
            IVCurve(list_hdf5[int(i)-1])

elif sys.argv[1] == '2':
    if input_xx=="all":
        for i in list_hdf5:
            transmission_2D(i)
    else:
        for i in input_xx:
            transmission_2D(list_hdf5[int(i)-1])

elif sys.argv[1] == '3':
    if input_xx=="all":
        for i in list_hdf5:
            transmission_3D(i)
    else:
        for i in input_xx:
            transmission_3D(list_hdf5[int(i)-1])







