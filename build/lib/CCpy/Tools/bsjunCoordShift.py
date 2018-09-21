#!/usr/local/bin/python2.7

import os, sys
import numpy as np

from bsjunCODE.bsjunStructure import NonPeriodicCoordinates as npc
from bsjunCODE.bsjunStructure import coord_shift

shift = np.linspace(0.2, 1.2, 6)

cars = [car for car in os.listdir("./") if ".car" in car]
cars.sort()

# coord_shift(filename, molecule numbers, direction, shift range, )
for filename in cars:
    coord_shift(filename, range(0,36), "z", shift, negative=True)
