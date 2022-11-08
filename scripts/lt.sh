set -e

f=$1 

date

stack run -- --logformat=dcdt --eventlog var/$f.dcdt --pnetfile var/$f.pnml --ptreefile var/$f.ptree --impl=mnode --ptreeformat=latex

date

cat var/$f.ptree

