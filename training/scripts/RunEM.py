import os,sys

# os.system("scripts/Inside counts/counts1021.1 counts/spinecounts counts/pspines training/train10000.data r /tmp/rem1 +RTS -K1G -H1G")
for i in range(1,20):
  os.system("scripts/Inside counts/counts1021.1 counts/spinecounts counts/pspines training/train10000.data /tmp/rem%s /tmp/rem%s +RTS -K1G -H1G"%(i,i+1))
