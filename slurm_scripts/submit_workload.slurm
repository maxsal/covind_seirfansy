#!/bin/bash

#SBATCH --mail-type=FAIL
#SBATCH --time=25:00:00
#SBATCH --job-name=seirfansy_submit_workload
#SBATCH --mem-per-cpu=1G
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

export http_proxy=http://proxy.arc-ts.umich.edu:3128/
export FTP_PROXY=http://proxy.arc-ts.umich.edu:3128/
export ftp_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTPS_PROXY=http://proxy.arc-ts.umich.edu:3128/
export https_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTP_PROXY=http://proxy.arc-ts.umich.edu:3128/

export today=$(date +%Y-%m-%d)
export production=TRUE
export code_repo="$HOME/covind_seirfansy/"
export data_repo="$HOME/covind_seirfansy_data/"

module load Rtidyverse/4.0.3

gitpull=$(sbatch $code_repo/slurm_scripts/git_pull_JHU.slurm | awk '{print $NF}')
preprocess=$(sbatch --dependency=afterok:$gitpull $code_repo/slurm_scripts/preprocess_data.slurm | awk '{print $NF}')
sleep 120
states=$(Rscript $code_repo/r_scripts/get_states.R)
state_jobs=""
for i in $states; do
	export state=$i
	state1=$(sbatch --job-name=$i $code_repo/slurm_scripts/state_gen_seirfansy.slurm | awk '{print $NF}')
	if [[ $state_jobs != "" ]]; then
		state_jobs=$state_jobs:$state1
	else
		state_jobs=$state1
	fi
done
echo $state_jobs
plots=$(sbatch --dependency=afterok:$state_jobs $code_repo/slurm_scripts/plot_state.slurm | awk '{print $NF}')
sbatch --dependency=afterok:$plots $code_repo/slurm_scripts/gitpush.slurm

sbatch --begin=now+24hour $code_repo/slurm_scripts/submit_workload.slurm
