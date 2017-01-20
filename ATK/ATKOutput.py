#!/usr/local/bin/python2.7
import os,sys
from subprocess import call

import numpy as np
import pandas as pd
from math import acos, radians, degrees

from matplotlib import colors, ticker, cm, rc, style
import matplotlib.pyplot as plt

from netCDF4 import Dataset

class ATKOutputAnal():
    def __init__(self, ncfile):
        self.ncfile = ncfile
        self.nc = Dataset(ncfile)
        self.variables = self.nc.variables
        self.name = ncfile(".nc","")

    def get3dTransmission(self, trans_max=None):
        nc = self.nc
        ncfile = self.ncfile
        variables = self.variables        
        name = self.name

        
