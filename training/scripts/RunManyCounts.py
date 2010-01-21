import os,sys
s = int(sys.argv[1])
e = int(sys.argv[2])
for i in range(s,e):
  os.system("scripts/CollectCounts training/train.%s.data /tmp/dwords.%s +RTS -N4 -K1G -H1G"%(i,i))
os.system("scripts/CombineCounts /tmp/counts1001.2 " + "".join([("/tmp/dwords.%s "%i) for i in range(s,e)]))
os.system("mv /tmp/counts1001.2 counts/%s"%(sys.argv[3]))
