#!/bin/bash

#SBATCH -J gamma_pie_c
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=8G
#SBATCH --cpus-per-task=4

# Output files should ideally go to /work instead of /home
#SBATCH -o /work/%u/%x-%j.out

module load foss/2019b R/3.6.2-2
export OFILE=/work/$USER/NutNet/$SLURM_JOB_NAME-$SLURM_JOB_ID.Rdata
export LANG=en_US.UTF-8
Rscript --vanilla /home/ladouceu/projects/CCRScale/cluster/gamma_pie_c.R


