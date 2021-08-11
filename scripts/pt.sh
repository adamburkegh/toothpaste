set -e

f=$1 

g=$2
if [ x$2 == "x" ]; then
    g=batch
fi

date

# stack run -- --logformat=dcdt --eventlog var/$f.dcdt --pnetfile var/$f.pnml --ptreefile var/$f.ptree 
stack run -- --logformat=dcdt --eventlog var/$f.dcdt --pnetfile var/$f.pnml --ptreefile var/$f.ptree --impl=$g

date

cat var/$f.ptree

