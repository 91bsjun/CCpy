#!/usr/local/bin/python2.7
from bsjunCODE.bsjunTools import linux_command as lc
import os,sys

try:
    command = sys.argv[1]
except:
    print("You can set command by 'bsjunUpperControllerGen.py [command]'")
    command = "Command"
string="""#!/usr/local/bin/python2.7
import os, sys
from bsjunCODE.bsjunTools import linux_command as lc

## find dirs
exceptions = ["structures"]
dirs = [d for d in os.listdir("./") if os.path.isdir(d) if d not in exceptions]
dirs.sort()

## setting command
command = "%s"


## check
print("* All founded directories")
for d in dirs:
    print(d)
print("* Your excution command : "+command)
chk = raw_input("* Continue (y/n) ? ")
if chk != "y":
    quit()

pwd = os.getcwd()
## action   
for d in dirs:
    os.chdir(d)
    print("Current directory : "+d)
    print("Excution command  : "+command)
    lc(command)
    os.chdir(pwd)
"""%(command)

f = open("UpperController.py","w")
f.write(string)
f.close()

lc("chmod +x UpperController.py")
