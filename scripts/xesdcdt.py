"""
Double Comma Delimited Text (DCDT) separates each field with two commas, and each row with a newline. i.e., like Comma Separated Value (CSV), but with two commas.
"""

from pm4py.objects.log.importer.xes import importer as xes_importer
from pm4py.objects.log.exporter.xes import exporter as xes_exporter
from pm4py.objects.log.obj import EventLog
import argparse
import sys
import random

delim = ",,"
nameClassifier = "concept:name"
classifier = nameClassifier

class Formatter:
    def __init__(self,classifier):
        self.classifier = classifier

    def format(self,log):
        None


class DCDTFormatter(Formatter):
    def __init__(self,classifier):
        Formatter.__init__(self,classifier)

    def format(self,log,outFileName):
        with open(outFileName,'w') as outf:
            for line in log:
                outline = line[0][self.classifier]
                for entry in line[1:]:
                    outline += delim + entry[self.classifier]
                outline += "\n"
                outf.write(outline)


class XESFormatter(Formatter):
    def __init__(self,classifier):
        Formatter.__init__(self,classifier)

    def format(self,log,outFileName):
        xes_exporter.apply(log,outFileName)


def logprocess(formatter,fname,log):
    formatter.format(log,fname)

def writeKLog(basename,logIndex,ext,formatter,ctLog):
    fnk = basename + "_k" + str(logIndex) + "." + ext
    logprocess(formatter,fnk,ctLog)

def writeNKLog(basename,ext,formatter,complogs):
    logIndex = 1
    for ctLog in complogs:
        fnk = basename + "_nk" + str(logIndex) + "." + ext
        logprocess(formatter,fnk,ctLog)
        logIndex+=1

def logk(formatter,fname,kfold,log):
    chunk = len(log) / kfold
    basename , ext = fname.split('.')
    random.shuffle(log)
    ctLog = EventLog()
    complogs = [ EventLog() for x in range(kfold) ]
    ct = 0
    logIndex = 1
    compIndexes = list(range(2,kfold+1))
    for line in log:
        ctLog.append(line)
        for ci in compIndexes:
            complogs[ci-1].append(line)
        if ct > chunk:
            writeKLog(basename,logIndex,ext,formatter,ctLog)
            ct = 0
            logIndex += 1
            compIndexes = list( range(1,kfold+1) ) 
            compIndexes.remove(logIndex)
            ctLog = EventLog()
        ct += 1
    writeKLog(basename,logIndex,ext,formatter,ctLog)
    writeNKLog(basename,ext,formatter,complogs)
 

def logkfold(kfold):
    return lambda formatter,fname,log: logk(formatter,fname,kfold,log) 

def xesprocess(lp,formatter,fname,log):
    lp(formatter,fname,log) 



def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("xesfile")
    parser.add_argument("--format",nargs=1, default=["dcdt"] )
    parser.add_argument("--outfile",nargs=1 )
    parser.add_argument("--kfold",nargs=1, type=int)
    args = parser.parse_args()
    formatter = DCDTFormatter(classifier)
    if (args.format[0] == "xes"):
        formatter = XESFormatter(classifier)
    outfile = ""
    if (args.outfile is None):
        outfile = "out." + args.format[0]
    else:
        outfile = args.outfile[0]
    lp = logprocess
    if (not args.kfold is None):
        lp = logkfold(args.kfold[0])
    log = xes_importer.apply(args.xesfile)
    xesprocess(lp,formatter,outfile,log)


if __name__ == "__main__":
    main()


