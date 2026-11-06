#!/usr/bin/env bash

###################################
## Definitions for batch system
#SBATCH -A rkm@cpu                    # Accounting information
#SBATCH --job-name=LMDZ_BENCH_DJ08    # Job name
#SBATCH --partition=cpu_p1            # Partition Name
#SBATCH --ntasks=2                    # Total number of MPI processes
#SBATCH --hint=nomultithread          # 1  MPI process per node  (no hyperthreading)
#SBATCH --time=00:10:00                # Maximum execution time (HH:MM:SS)
#SBATCH --output=LMDZ_BENCH_DJ08.out    # Name of output listing file
#SBATCH --error=LMDZ_BECNH_DJ08.err     # Name of error listing file (the same)
###################################

# Process distribution
NPROC_LMDZ=1
NPROC_PYTHON=1

## -------------------------------------------------------
##   End of user-defined section - modify with knowledge
## -------------------------------------------------------
set -x
ulimit -s unlimited

## Load environment
source ${HOME}/.bash_profile

# job information 
cat << EOF
------------------------------------------------------------------
Job submit on $SLURM_SUBMIT_HOST by $SLURM_JOB_USER
JobID=$SLURM_JOBID Running_Node=$SLURM_NODELIST 
Node=$SLURM_JOB_NUM_NODES Task=$SLURM_NTASKS
------------------------------------------------------------------
EOF

## Move to config directory
CONFIG_DIR=${SLURM_SUBMIT_DIR:-$(pwd)}
cd ${CONFIG_DIR}


## Clean working directory
touch namcouple
touch eophis_nml
rm namcouple*
rm eophis_nml

## Get executable and execution material
cp ../../bin/gcm_32x32x39_phylmd_oldrad_para_mem_couple.e  lmdz_cpl.e
cp -r ../../modeles/PYTHON/*  .
cp XML/*  .
cp PARAM/*  .

set -e
ls -l

## Run Eophis preproduction
python3 ./main.py --exec preprod
mv eophis.out eophis_preprod.out
mv eophis.err eophis_preprod.err

# write multi-prog file
touch run_file
echo 0-$((NPROC_LMDZ - 1)) ./lmdz_cpl.e >> run_file
echo ${NPROC_LMDZ}-$((NPROC_LMDZ + NPROC_PYTHON - 1)) python3 ./eophis_main.py >> run_file

# run coupled LMDZ-DJ08
time srun --multi-prog ./run_file

