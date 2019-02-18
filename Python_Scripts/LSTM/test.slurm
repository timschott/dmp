#SBATCH -A lyricalnovels
#SBATCH -p gpu #i need a gpu
#SBATCH --gres=gpu
#SBATCH -N 1 # 1 node
#SBATCH -n 1 # 1 task # only doing one thing
#SBATCH -t 00-01:00:00 # time limit dd-hh:mm:ss
#SBATCH --cpus-per-task=4 #max is 8 cores
#SBATCH --mem=120G # max is 240GB
#SBATCH --mem-per-cpu=6G # 6GB of total memory per cpu (max 12GB for gpu)

module purge #remove any loaded modules as a precaution
module load anaconda2
export PYTHONPATH=~/bin:$PYTHONPATH
OUTPUTDIR=/results/$USER/test-$SLURM_JOBID      
#  create unique path using job id. $USER avoids hardcoding your computing id 
mkdir -p $OUTPUTDIR     # create the output directory

LSTM.py padded_list.npy > $OUTPUTDIR/test-out