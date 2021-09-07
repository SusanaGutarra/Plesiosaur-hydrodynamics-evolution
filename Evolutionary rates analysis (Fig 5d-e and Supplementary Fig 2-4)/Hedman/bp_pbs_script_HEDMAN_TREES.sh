#!/bin/bash
# request resources:
#PBS -l select=2:ncpus=24:mem=32GB+1:ncpus=2:mem=32GB
#PBS -l walltime=20:00:00
# #Join stdout and stderr to the same file
#PBS -j oe
module add lang/r/3.6.0-gcc
# module add lang/python/anaconda/3.7-2019.03
# Loading the installed version of BayesTrait on BluePebble also loads PPPostProcess
module load apps/bayestraits/3.0.1
# Curently we have to create a function when using BayesTraits V3 (since the old alias is not default in BluePebble + it is 
# not recommended to create aliases in scripts)
# BayesTraitsV3 () {
#    command bayestraits "$@"
# }
#export -f BayesTraitsV3
#module load apps/bayestraits/2.0.2
# module load apps/PPPostProcess
#export PATH=~/apps/pppostprocess/phylcor_version:$PATH # #export path to PPPostProcess (phylcor version)
export PATH=~/apps/bayestraits/BayesTraitsV2.0.2:$PATH # #export path to BayesTraitsV2.0.2
export PATH=~/apps/bayestraits/BayesTraitsV3.0.1-Linux:$PATH # #export path to BayesTraitsV3.0.1-Linux
export PBS_O_WORKDIR="/work/ts0438/SG_RATES/Hedman"
# on compute node, change directory to 'submission directory':
cd $PBS_O_WORKDIR

# record some potentially useful details about the job:
echo Running on host `hostname`
echo Time is `date`
echo Directory is `pwd`
echo PBS job ID is $PBS_JOBID
echo This jobs runs on the following machines:
echo `cat $PBS_NODEFILE | uniq`
# count the number of processors available:
numprocs=`wc $PBS_NODEFILE | awk '{print $1}'`

# run your program, timing it for good measure:
time Rscript ./Hedman_steps.R