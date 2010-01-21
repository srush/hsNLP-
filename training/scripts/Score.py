import os,sys


os.system("rm /tmp/test /tmp/gold")

for i in range(int(sys.argv[2]), int(sys.argv[3])):
  os.system("scripts/GetGold T < %s/result%s.data | scripts/Canonicalize test2/test2.%s.data >> /tmp/test"%(sys.argv[1],i,i))
  os.system("scripts/GetGold G < %s/result%s.data | scripts/Canonicalize test2/test2.%s.data >> /tmp/gold"%(sys.argv[1],i,i))
  #os.system("scripts/GetGold G < %s/result%s.data >> /tmp/gold"%(sys.argv[1],i))
  #os.system("scripts/GetGold T < %s/result%s.data >> /tmp/test"%(sys.argv[1],i))
os.system("~/EVALB/evalb /tmp/gold /tmp/test ~/EVALB/sample/sample.prm")
