#!/usr/local/bin/python2.7

import os, sys
from bsjunCODE.bsjunATKOutput import ATKOutputAnal as AO

ncfile = sys.argv[1]

aa = AO(ncfile)

aa.get3dTransmission()
