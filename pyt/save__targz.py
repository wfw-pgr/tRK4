import datetime
import subprocess

dlist   = [ "src", "pyt", "dat", "go.py", "README" ]
name    = "tRk4"

name    = name + str( datetime.date.today() ).replace( "-", "" )
com     = "tar zcvf " + name + ".tar.gz -X pyt/tar_ex.conf"
for d in dlist:
    com =  com + " " + d
print( com )
subprocess.call( com.split(" ") )
