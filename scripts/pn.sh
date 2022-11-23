set -e

f=$1 
n=$2
if [ x$2 == "x" ]; then
    n=0.1
fi

date

stack run -- --logformat=dcdt --eventlog var/$f.dcdt --pnetfile var/$f.pnml --ptreefile var/$f.ptree --impl=mnode --noise=$n

date

cat var/$f.ptree

