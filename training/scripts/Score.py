import os,sys


os.system("rm /tmp/test /tmp/gold")


#for i in range(int(sys.argv[2]), int(sys.argv[3])):
if sys.argv[2] == "C":
  os.system("scripts/GetGold T < %s/result%s.data | scripts/Canonicalize test2.bak/test2.%s.data +RTS -K1G >> /tmp/test"%("results",sys.argv[1],sys.argv[1]))
  os.system("scripts/GetGold G < %s/result%s.data | scripts/Canonicalize test2.bak/test2.%s.data +RTS -K1G >> /tmp/gold"%("results",sys.argv[1],sys.argv[1]))
else:
  #os.system("scripts/GetGold G < %s >> /tmp/gold"%(sys.argv[1]))
  #os.system("scripts/GetGold T < %s >> /tmp/test"%(sys.argv[1]))
#os.system("scripts/GetGold T < %s | scripts/Canonicalize %s >> /tmp/test"%(sys.argv[1],sys.argv[2]))
#os.system("scripts/GetGold G < %s | scripts/Canonicalize %s >> /tmp/gold"%(sys.argv[1],sys.argv[2]))

  os.system("scripts/GetGold G < %s >> /tmp/gold"%(sys.argv[1]))
  os.system("scripts/GetGold T < %s >> /tmp/test"%(sys.argv[1]))
os.system("~/EVALB/evalb /tmp/gold /tmp/test ~/EVALB/sample/sample.prm")
