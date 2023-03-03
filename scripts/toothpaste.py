"""
Simple Python wrapper for the toothpaste miner Haskell implementation.
"""

import os.path
import subprocess
import sys

import pm4py
from pm4py.objects.log.importer.xes import importer as xes_importer
from pm4py.objects.log.exporter.xes import exporter as xes_exporter

TBIN="toothpaste"
CLASSIFIER="concept:name"
DELIM=",,"


"""
Mine the logfile using the toothpaste miner. The resulting stochastic labelled
Petri net is returned as a pm4py PetriNet object, and written to outfile. This
convenience method is implemented as an external call to a toothpaste binary
executable, which must be on the PATH. Note that three other files are 
written to the directory of the outfile: a PNML stochastic Petri net model, a 
PTREE Probabilistic Process Tree, and an intermediate log. If outfile is None, 
the files are written to the logfile directory with the prefix from the 
logfile. 

For richer parameters, such as noise reduction, see the command line help
for Toothpaste miner.
"""
def mine(logfile,outfile=None):
    ofile = outfile
    if ofile is None:
        bname =  os.path.basename(logfile) 
        outdir =  os.path.dirname(logfile) 
    else:
        bname = os.path.basename(outfile) 
        outdir =  os.path.dirname(logfile) 
    pref = bname.split(".")[0]
    dfile = os.path.join( outdir, pref + ".dcdt")
    pnfile = os.path.join( outdir, pref + ".pnml")
    ptfile = os.path.join( outdir, pref + ".ptree")
    xestodcdt(logfile,dfile)
    subprocess.run([TBIN,
                    "--logformat=dcdt",
                    "--eventlog", dfile,
                    "--pnetfile", pnfile,
                    "--ptreefile", ptfile] )
    pn = pm4py.read_pnml(pnfile)
    return (pn,pnfile)

def minerVersion():
    subprocess.run([TBIN, "--version"])

def xestodcdt(xesfile,outfile):
    log = xes_importer.apply(xesfile)
    with open(outfile,'w') as outf:
        for line in log:
            outline = line[0][CLASSIFIER]
            for entry in line[1:]:
                outline += DELIM + entry[CLASSIFIER]
            outline += "\n"
            outf.write(outline)


 
def cmdline(args):
    logfile = sys.argv[1]
    outfile = None
    if len(sys.argv) > 2:
        outfile = sys.argv[2]
    print("Loading XES ...")
    (pn,pnfile) = mine(logfile,outfile)
    print("Model output to " + pnfile)
    

if __name__ == "__main__":
    cmdline(sys.argv)


