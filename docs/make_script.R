
for(i in 1:100){
  my_script <- paste0("#!/bin/bash
#SBATCH -N 1
#SBATCH -c 5
#SBATCH --mem-per-cpu=20G
#SBATCH --time=0-22:30:00     
#SBATCH --output=my.stdout
                      
module load R
R CMD BATCH '--args ", i, "' parallel.R")
  write(my_script, paste0("submitscript", i,".txt") )
  }
