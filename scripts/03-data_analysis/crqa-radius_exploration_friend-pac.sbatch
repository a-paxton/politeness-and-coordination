#!/bin/bash

#SBATCH --job-name=parallel_test-radius_exploration-friend    # Name of the job.
#SBATCH --output=slurm_output/parallel_test-radius_exploration-friend.out
#SBATCH --partition general_requeue
#SBATCH --ntasks=192    # Since we're parallelizing, set to maximum permitted cores
#SBATCH --dependency=singleton
#-SBATCH --time=1:00    # Don't let it run forever.

# overwrite output file
echo -n > slurm_output/parallel_test-radius_exploration-friend.out

# create per-job scratch directory
OUTPUT_DIR=$HOME/politeness_and_coordination/data/crqa_parameters/radius_exploration/$SLURM_JOB_NAME
mkdir -vp $OUTPUT_DIR
export OUTPUT_DIR

# load modules
source /etc/profile.d/modules.sh
module load X11/7.7 gcc/9.2.0 r/3.6.1

# initialize parallelization
parallel_opts=$(~/parallel-slurm/parallel_opts.sh)
module load parallel

# print to output for confirmation that it's started
echo $SLURM_ARRAY_TASK_ID ": Running SLURM task"

# identify the number of tests to run
lines=$(wc -l scripts/03-data_analysis/*.csv | cut -f1 -d ' ')
lines=$(( lines - 1 ))
job_numbers=slurm_output/job_numbers.txt
seq 1 $lines > $job_numbers

# run "friend" script
parallel \
    $parallel_opts \
    --joblog slurm_output/$SLURM_JOB_NAME.joblog \
    --resume \
    --retries 3 \
    --line-buffer \
    Rscript $HOME/politeness_and_coordination/scripts/03-data_analysis/crqa-test_one_radius_calculation-friend-pac.R \
    :::: $job_numbers

# run the "prof" R script
#Rscript $HOME/politeness_and_coordination/scripts/03-data_analysis/crqa-test_one_radius_calculation-prof-pac.R

# print to output for confirmation that it's ended
echo $SLURM_ARRAY_TASK_ID ": Job done"
