#!/bin/bash

find politeness_and_coordination/slurm_output/*.err -type f -size +0 | grep -Po "(?<=_)\d{1,3}" | awk -vORS=, '{ print $1 }' | sed 's/,$/\n/' > politeness_and_coordination/problem_files-DATE.txt
