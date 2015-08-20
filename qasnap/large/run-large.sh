#!/bin/sh
#PBS -q regular
#PBS -l mppwidth=49152
#PBS -l walltime=00:55:00
#PBS -N SNAP.large
#PBS -V
cd $PBS_O_WORKDIR

# Large problem, all MPI

#set up the IPM env vars
export IPM_REPORT=full
export IPM_LOGDIR=$PWD
export IPM_LOG=full
export IPM_HPM=PAPI_FP_OPS

# The script below is based on a 2048 node, 49152 MPI rank run on NERSC's Hopper
echo "running large problem on 2048 nodes, 49152 cores"
aprun -cc cpu -n 49152 ./snap ./large-2048nodes.input ./large-2048nodes.output
