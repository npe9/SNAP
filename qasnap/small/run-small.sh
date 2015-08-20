#!/bin/sh
#PBS -q regular
#PBS -l mppwidth=96
#PBS -l walltime=00:30:00
#PBS -N SNAP.small-src_opt0-16x16x16
#PBS -V
cd $PBS_O_WORKDIR

# Small problem, all MPI
# The script below is based on a 4 node, 96 MPI rank run on NERSC's Hopper
#set up the IPM env vars
export IPM_REPORT=full
export IPM_LOGDIR=$PWD
export IPM_LOG=full
export IPM_HPM=PAPI_FP_OPS

echo "running small problem on 4 nodes, 96 cores"
aprun -cc cpu -n 96 ./snap ./small-4nodes.input ./small-4nodes.output
