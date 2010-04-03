import os,sys

for i in range(int(sys.argv[1]), int(sys.argv[2])):
  # os.system("scripts/ShowCounts %s /tmp/cspines /tmp/pspines test.prune/test.prune.%s.data test/test.%s.data %s | tee results/result%s.data"%(sys.argv[3], i,i,i,i))
  os.system("scripts/TestParse %s /tmp/cspines /tmp/pspines test.prune/test.prune.%s.data test/test.%s.data %s | tee results/result%s.data"%(sys.argv[3], i,i,i,i))
