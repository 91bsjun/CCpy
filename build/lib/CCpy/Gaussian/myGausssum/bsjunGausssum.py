import webbrowser

import traceback
import copy             # For deepcopy...until I find a better way of doing this
#import configparser     # For writing the settings to an .ini file
import logging
import glob

from CCpy.Gaussian.myGausssum.gausssum.cclib.parser import ADF, GAMESS, Gaussian, ccopen, data
#from gausssum.preferencesbox import PreferencesPopupBox
#from gausssum.aboutbox import AboutPopupBox
from CCpy.Gaussian.myGausssum.gausssum.mypopanalysis import Popanalysis
from CCpy.Gaussian.myGausssum.gausssum.myelectrontrans import ET
#from gausssum.geoopt import GeoOpt
#from gausssum.search import Search
#from gausssum.vibfreq import Vibfreq
from CCpy.Gaussian.myGausssum.gausssum.myscf import SCF
from CCpy.Gaussian.myGausssum.gausssum.utils import *
from CCpy.Gaussian.myGausssum.gausssum.folder import folder

import os, sys

class App:
    def __init__(self,inputfile):
        self.inputfile = inputfile
        self.logfile=ccopen(self.inputfile)
    def getresult(self):
        self.data = self.logfile.parse()
        s=self.data
        self.outlist = SCF(self.data)
        result = self.outlist
        x=[]
        y=result
        for i in range(len(result)):
            x.append(i+1)
        return x, y
    def etresult(self):
        self.data = self.logfile.parse()
        filename = str(self.inputfile)
        mpl = ET(self.data,filename,300,800,500,3000)
        return mpl

    def moresult(self):
        self.data = self.logfile.parse()
        filename = str(self.inputfile)
        mpld3object = Popanalysis(self.data,filename,float(-20),float(10),float(0.3))
        return mpld3object
