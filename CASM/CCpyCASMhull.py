#!/bin/env python

import os
import matplotlib.pyplot as plt
import pandas as pd

class CASMhull():
    def __init__(self):
        if "FEhull.dat" not in os.listdir("./"):
            print("'FEhull.dat' is not in this directory.")
            quit()
        pwd = os.getcwd()
        pwd = pwd.split("/")[-1]
        self.pwd = pwd

    def getData(self):
        hull_dat = open("FEhull.dat", "r").readlines()

        cons = []
        fe = []
        hull_point = {'x': [], 'y': []}
        for l in hull_dat:
            splts = l.split()
            for i in range(len(splts)):
                splts[i] = float(splts[i])
            if len(splts) == 4:
                cons.append(splts[0])
                fe.append(splts[1])
                hull_point['x'].append(splts[2])
                hull_point['y'].append(splts[3])
            elif len(splts) == 2:
                cons.append(splts[0])
                fe.append(splts[1])

        self.cons, self.fe, self.hull_point = cons, fe, hull_point

    def plotHull(self):
        cons, fe, hull_point = self.cons, self.fe, self.hull_point

        plt.scatter(cons, fe, marker="D", color='b', s=10)
        plt.plot(hull_point['x'], hull_point['y'], marker='o', color='r', alpha=0.7, ms=8)

        plt.xlim(0.0, 1.0)
        plt.xlabel("Concentration", fontsize=20)
        plt.ylabel("Formation energy", fontsize=20)

        figname = "01_" + self.pwd + "_convexhull.png"
        plt.savefig(figname)
        print("\nFigure saved : " + figname)
        plt.show()

    def mainFlow(self):
        self.getData()
        self.plotHull()

if __name__ == "__main__":
    ch = CASMhull()
    ch.mainFlow()

