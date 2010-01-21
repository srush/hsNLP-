import os,sys
from commands import *
import glob
import re
for l in sys.stdin: 
  t = l.split("|")
  print "G"+ t[1]
  print "T"+ t[2]

