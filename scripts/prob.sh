set -e

f=$1 

date

stack run -- --logformat=dcdt --eventlog var/$f.dcdt --pnetfile var/$f.pnml --ptreefile var/$f.ptree --impl=mnode --traceprobfile var/${f}_prob.txt

date

cat var/$f.ptree

cat var/${f}_prob.txt

