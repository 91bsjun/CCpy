import sys
import numpy as np

"""
Generate custom colormap from
http://jdherman.github.io/colormap/

Copy generated text in the website and make a file.

Usage: get_cmap.py [filename]
Return: pickled numpy array of colormap

Use in matplotlib

import matplotlib
with open("custom_cmap.pkl", "rb") as mydata:
    own_cmap = pickle.load(mydata)

plt.imshow(... , cmap=own_cmap)

NOTICE
pickle is highly up to python version. BE WARE!
"""

filename = sys.argv[1]
lines = open(filename, 'r').readlines()


# remove empty line in the last of file
if len(lines[-1]) < 3:
    del lines[-1]
final_index = len(lines) - 1

cmap = []
for i, l in enumerate(lines):
    l = l.replace("\n", "").replace(";", "").replace("[", "").replace("]", "").replace(" ","")
    rgb = l.split(",")
    rgb = [int(val) for val in rgb]
    cmap.append(rgb)

cmap = np.array(cmap, dtype='float32')

import pickle
with open("custom_cmap.pkl", "wb") as mydata:
    pickle.dump(cmap, mydata)
