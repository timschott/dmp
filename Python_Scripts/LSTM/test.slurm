#SBATCH -A lyricalnovels
#SBATCH -p gpu #i need a gpu
#SBATCH --gres=gpu
#SBATCH -N 1 # 1 node
#SBATCH -n 1 # 1 task # only doing one thing
#SBATCH -t 00-01:00:00 # time limit dd-hh:mm:ss
#SBATCH --cpus-per-task=20 #max is 28 per node in standard partition (change if multi-threaded)
#SBATCH --mem=120000
#SBATCH --mem-per-cpu=6000 # 6000MB of total memory per cpu, max 120000 in standard partition 

module purge #remove any loaded modules as a precaution
module load anaconda2
export PYTHONPATH=~/bin:$PYTHONPATH
OUTPUTDIR=/results/$USER/test-$SLURM_JOBID      
#  create unique path using job id. $USER avoids hardcoding your computing id 
mkdir -p $OUTPUTDIR     # create the output directory

LSTM.py padded_list.npy > $OUTPUTDIR/test-out