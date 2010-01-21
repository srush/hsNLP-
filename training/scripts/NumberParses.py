import os,sys
from commands import *
import glob
import re
files = glob.glob("results/result*")
for f in files: 
  matches = re.search("results/result(\d+)\.data", f)
  
  gold = getoutput("cat "+ f +" | scripts/GetGold G | scripts/Canonicalize ").strip().split("\n")
  test = getoutput("cat "+ f +" | scripts/GetGold T | scripts/Canonicalize ").strip().split("\n")
  if len(gold) <> 25 : continue
  g = int(matches.group(1))
  if g != 64:
    for i in range(0,len(gold)):
      print " | ".join([str((i+1)+(g*25)), gold[i], test[i]])


