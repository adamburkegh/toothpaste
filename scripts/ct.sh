set -e

f=$1 

date

stack run -- --logformat=dcdt --modeltype=cflow --eventlog var/$f.dcdt --pnetfile var/$f.pnml --ptreefile var/$f.ptree 

date

cat var/$f.ptree

