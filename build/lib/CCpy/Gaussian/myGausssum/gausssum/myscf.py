import os
import sys
import math



def SCF(logfile):
    #print("mySCF START")
    scfvalues = logfile.scfvalues[-1] # The most recent in the logfile
    scftargets = logfile.scftargets[-1] # Ditto

    deviation = []
    for i in range(len(scfvalues)): # Which SCF cycle
        dev = 0
        for j in range(len(scftargets)): # Which target
            if abs(scfvalues[i][j]) > scftargets[j]:
                dev += math.log(abs(scfvalues[i][j]) / scftargets[j])
        deviation.append(dev)

    return deviation
    '''
    if len(deviation)>=numpoints+2: # If there are two points to plot

        h = MPLPlot()
        h.setlabels("SCF convergence step", "Deviation from targets")
        data = list(zip(range(len(deviation)-numpoints),deviation[numpoints:]))
        h.data(data)
        h.data(data, lines=False)
        h.subplot.set_ylim(bottom=0)
        
        DisplayPlot(root,h,"Plot of SCF deviation vs Iteration")
    else:
        print("I need at least two points to plot\n")
    '''
