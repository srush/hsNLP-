import os,sys


os.system("rm /tmp/test /tmp/gold")


#for i in range(int(sys.argv[2]), int(sys.argv[3])):
if sys.argv[2] == "C":
  for i in range(int(sys.argv[3]), int(sys.argv[4])):
    #if i == 13 : continue
    os.system("scripts/GetGold T < %s/result%s.data | scripts/Canonicalize %s/val2.%s.data +RTS -K1G >> /tmp/test"%(sys.argv[1],i,sys.argv[5],i))
    os.system("scripts/GetGold G < %s/result%s.data | scripts/Canonicalize %s/val2.%s.data +RTS -K1G >> /tmp/gold"%(sys.argv[1],i, sys.argv[5], i ))
elif sys.argv[2] == "C2":
  for i in range(int(sys.argv[3]), int(sys.argv[4])):
    os.system("scripts/GetGold T < %s/test.%s | scripts/Canonicalize val2/val2.%s.data +RTS -K1G >> /tmp/test"%(sys.argv[1],i,i))
    os.system("scripts/GetGold G < %s/test.%s | scripts/Canonicalize val2/val2.%s.data +RTS -K1G >> /tmp/gold"%(sys.argv[1],i,i))
elif sys.argv[2] == "C3":
  for i in range(int(sys.argv[3]), int(sys.argv[4])):
    os.system("scripts/GetGold T < %s/test.0207.%s.%s | scripts/Canonicalize val2/val2.%s.data +RTS -K1G >> /tmp/test"%(sys.argv[1],i,sys.argv[5],i))
    os.system("scripts/GetGold G < %s/test.0207.%s.%s | scripts/Canonicalize val2/val2.%s.data +RTS -K1G >> /tmp/gold"%(sys.argv[1],i,sys.argv[5],i))
elif sys.argv[2] == "C4":
  for i in range(int(sys.argv[3]), int(sys.argv[4])):
    os.system("scripts/GetGold T < %s/test.%s | scripts/Canonicalize sec23.2/sec23.2.%s.data +RTS -K1G >> /tmp/test"%(sys.argv[1],i,i))
    os.system("scripts/GetGold G < %s/test.%s | scripts/Canonicalize sec23.2/sec23.2.%s.data +RTS -K1G >> /tmp/gold"%(sys.argv[1],i,i))

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

os.system("cat /tmp/gold | python scripts/Unquote.py | python scripts/Unline.py > /tmp/gold2 ")
os.system("cat /tmp/test | python scripts/Unquote.py | python scripts/Unline.py > /tmp/test2 ")
os.system("~/EVALB/evalb /tmp/gold2 /tmp/test2 -p ~/EVALB/COLLINS.prm")
