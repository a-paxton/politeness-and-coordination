#!/bin/bash

#SBATCH --job-name=pac_20200608e-radius_exploration    # Name of the job.
#SBATCH --output=slurm_output/pac_20200608e/pac_20200608e-radius_exploration-%A_%a.out   # Provide unique output files for each array task.
#SBATCH --error=slurm_output/pac_20200608e/pac_20200608e-radius_exploration-%A_%a.err    # Provide unique error files for each array task.
#SBATCH --array=1-980    # Provide values that we'll translate to unique gridsearch parameters.
#SBATCH --ntasks=1    # We've only got one script we're running.
#SBATCH --time=00:10:00    # Don't let it run forever.

# load modules
source /etc/profile.d/modules.sh
module load X11/7.7 gcc/9.2.0 r/3.6.1

# save our task ID as an environmental variable
export SLURM_ARRAY_TASK_ID

# save variables
search_start_value=.5
search_end_value=1.8
export search_start_value
export search_end_value

# print to output for confirmation that it's started
echo $SLURM_ARRAY_TASK_ID ": Running SLURM task"

# run the "friend" R script
Rscript $HOME/politeness_and_coordination/scripts/03-data_analysis/crqa-test_one_radius_calculation-friend-pac.R

# run the "prof" R script
Rscript $HOME/politeness_and_coordination/scripts/03-data_analysis/crqa-test_one_radius_calculation-prof-pac.R

# print to output for confirmation that it's ended
echo $SLURM_ARRAY_TASK_ID ": Job done"
