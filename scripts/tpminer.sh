set -e

verbose=1

log(){
    if [[ $verbose -gt 0 ]] ; then
        echo $*
        date
    fi
}

lg1=$1

xes=`basename $lg1`
lg=${xes%%.xes}

log "Preparing event log ..."

python scripts/xesdcdt.py --outfile var/$lg.dcdt $lg1 

log "Mining ..."

stack run -- --logformat=dcdt --eventlog var/$lg.dcdt --pnetfile var/$lg.pnml --ptreefile var/$lg.ptree 

log "ToothpasteMiner done."

