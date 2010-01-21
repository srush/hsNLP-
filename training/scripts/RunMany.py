import os,sys

for i in range(int(sys.argv[1]), int(sys.argv[2])):

  os.system("scripts/ShowCounts %s counts/spinecounts counts/pspines test/testsplit.%s.data %s | tee results/result%s.data"%(sys.argv[3], i,i, i))
