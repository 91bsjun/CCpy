### Python scripts for Computational Chemistry.

# 1. How to set up CCpy
## 1.1. Clone to your PC or Cluster
<pre>
[user@localhost ~]$ cd /opt/shared
[user@localhost shared]$ git clone https://github.com/91bsjun/CCpy.git
</pre>

## 1.2. Build setup file
<pre>
[user@localhost shared]$ cd CCpy
[user@localhost CCpy]$ python setup.py install
</pre>
#### Test
<pre>
[user@localhost ~]$ python
Python 2.7.11 (default, Aug  6 2016, 10:12:58) 
[GCC 4.4.7 20120313 (Red Hat 4.4.7-16)] on linux2
Type "help", "copyright", "credits" or "license" for more information.
>>> import CCpy
>>> 
</pre>
## 1.3. Download python libraries
- Numpy
- Pandas
- Matplotlib
- Pymatgen
<pre>
[user@localhost ~]$ pip install numpy pandas matplotlib pymatgen
</pre>

