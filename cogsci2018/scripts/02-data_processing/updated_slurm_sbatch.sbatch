#!/bin/bash

#SBATCH --job-name=pac_fdm_0713         # Name of the job.
#SBATCH --output=slurm_output/pac_fdm-0713-%A_%a.out       # Provide unique output files for each array task.
#SBATCH --error=slurm_output/pac_fdm-0713-%A_%a.err        # Provide unique error files for each array task.
#SBATCH --array=1-112
#SBATCH --ntasks=1                    # We've only got one script we're running.
#SBATCH -p RM-shared                  # Make sure we're using the "cheaper" nodes.
#SBATCH --time=01:00:00               # Don't let it run forever

# save our task ID as an environmental variable
export SLURM_ARRAY_TASK_ID

# print to output for confirmation that it's started
echo "Running SLURM task: " $SLURM_ARRAY_TASK_ID

# run the p
python politeness_and_coordination/scripts/02-data_processing/process_one_movie.py $SLURM_ARRAY_TASK_ID

# print to output for confirmation that it's ended
echo "Job done"
