# CCpy
Python scripts for Computational Chemistry.

# 1. How to set up CCpy
## 1.1. Clone to your PC or Cluster
<pre>
[user@localhost ~]$ cd /opt/shared
[user@localhost shared]$ git clone https://github.com/91bsjun/CCpy.git
</pre>

## 1.2. Add the path of CCpy to environment
#### Linux
<pre>
[user@localhost shared]$ vi ~/.bashrc              # for an individual user
[user@localhost shared]$ sudo vi /etc/bashrc       # for all users.

....
....
export PYTHONPATH=/opt/shared:$PYTHONPATH          # for using CCpy in python
export PATH=/opt/shared/bin:$PATH                  # enable excutable files of CCpy

[user@localhost shared]$ source ~/.bashrc
[user@localhost shared]$ sudo source /etc/bashrc   # update edited configurations
</pre>
#### Windows
`

# 2. VASP modules
## 2.1. VASP Input Generation
<pre>
asd
</pre>
