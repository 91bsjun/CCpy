#!/home/shared/anaconda3/envs/CCpy_tmp/bin/python
import os, sys

from CCpy.ATATio.ATAT import ATATOut as AO

try:
    chk = sys.argv[1]
except:
    print("\nHow to use : " + sys.argv[0].split("/")[-1] + " [option]")
    print('''--------------------------------------
[1] : Analyze fit.out file
[2] : Analyze binary system
[3] : Analyze multi components system'''
          )
    quit()

if sys.argv[1] == "1":
    myAO = AO()
    myAO.fit_out()

elif sys.argv[1] == "2":
    myAO = AO()
    myAO.plotter()

elif sys.argv[1] == "3":
    myAO = AO()
    try:
        c1, c2, c3 = sys.argv[2], sys.argv[3], sys.argv[4]
    except:
        columns = raw_input("cloumns? ")
        c1, c2, c3 = columns.split(",")
        
    myAO.multi_plotter(c1, c2, c3)

