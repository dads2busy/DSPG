#!/bin/bash

#SBATCH -p bii
#SBATCH -A biocomplexity
#SBATCH --job-name=rnd_embedding_score
#SBATCH --time=1-24:00:00
#SBATCH -c 32
#SBATCH --mem=200G

module load anaconda3

python 07_embedding_score_final.py