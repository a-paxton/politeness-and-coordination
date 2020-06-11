#!/bin/bash

#SBATCH --job-name=parallel_test-radius_exploration-friend    # Name of the job.
#SBATCH --output=slurm_output/parallel_test-radius_exploration.out
#-SBATCH --output=slurm_output/pac_20200608e/pac_20200608e-radius_exploration-%A_%a.out   # Provide unique output files for each array task.
#-SBATCH --error=slurm_output/pac_20200608e/pac_20200608e-radius_exploration-%A_%a.err    # Provide unique error files for each array task.
#-SBATCH --array=1-980    # Provide values that we'll translate to unique gridsearch parameters.
#SBATCH --partition general_requeue
#SBATCH --ntasks=192    # We've only got one script we're running.
#SBATCH --dependency=singleton
#-SBATCH --time=1:00    # Don't let it run forever.

# Overwrite output file.
echo -n > slurm_output/parallel_test-radius_exploration.out

# Create per-job scratch directory
SCRATCH_DIR=~/scratch/$SLURM_JOB_NAME
mkdir -vp $SCRATCH_DIR
export SCRATCH_DIR

# load modules
source /etc/profile.d/modules.sh
module load X11/7.7 gcc/9.2.0 r/3.6.1
parallel_opts=$(~/parallel-slurm/parallel_opts.sh)
module load parallel

# print to output for confirmation that it's started
echo $SLURM_ARRAY_TASK_ID ": Running SLURM task"

# run the "friend" R script
lines=$(wc -l scripts/03-data_analysis/*.csv | cut -f1 -d ' ')
lines=$(( lines - 1 ))
job_numbers=slurm_output/job_numbers.txt
seq 1 $lines > $job_numbers
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
