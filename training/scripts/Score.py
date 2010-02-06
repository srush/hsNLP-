import os,sys


os.system("rm /tmp/test /tmp/gold")


#for i in range(int(sys.argv[2]), int(sys.argv[3])):
if sys.argv[2] == "C":
  for i in range(int(sys.argv[3]), int(sys.argv[4])):
    os.system("scripts/GetGold T < %s/result%s.data | scripts/Canonicalize test2/test2.%s.data +RTS -K1G >> /tmp/test"%("results",i,i))
    os.system("scripts/GetGold G < %s/result%s.data | scripts/Canonicalize test2/test2.%s.data +RTS -K1G >> /tmp/gold"%("results",i,i))
elif sys.argv[2] == "c":
  os.system("scripts/GetGold T < %s | scripts/Canonicalize %s >> /tmp/test"%(sys.argv[3],sys.argv[4]))
  os.system("scripts/GetGold G < %s | scripts/Canonicalize %s >> /tmp/gold"%(sys.argv[3],sys.argv[4]))
else:
  #os.system("scripts/GetGold G < %s >> /tmp/gold"%(sys.argv[1]))
  #os.system("scripts/GetGold T < %s >> /tmp/test"%(sys.argv[1]))
#os.system("scripts/GetGold T < %s | scripts/Canonicalize %s >> /tmp/test"%(sys.argv[1],sys.argv[2]))
#os.system("scripts/GetGold G < %s | scripts/Canonicalize %s >> /tmp/gold"%(sys.argv[1],sys.argv[2]))

  os.system("scripts/GetGold G < %s >> /tmp/gold"%(sys.argv[1]))
  os.system("scripts/GetGold T < %s >> /tmp/test"%(sys.argv[1]))
os.system("~/EVALB/evalb /tmp/gold /tmp/test ~/EVALB/sample/sample.prm")
