#!/bin/bash -l

## Job submission script on QUT HPC

#PBS -N tpc2
#PBS -l ncpus=3
#PBS -l mem=55gb
#PBS -l walltime=09:00:00

export JMEM=50g

module load java/1.8.0_231

if [[ ! -z ${PBS_O_WORKDIR+epsilon} ]]; then
    cd $PBS_O_WORKDIR
fi

tpcmrunner.sh

