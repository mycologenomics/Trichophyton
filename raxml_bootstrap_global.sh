#!/bin/sh

#PBS -lwalltime=72:00:00
#PBS -lselect=1:ncpus=32:mem=124gb:avx2=true

module load anaconda3/personal
source activate raxml_env

raxmlHPC-PTHREADS-AVX2 -T 20 -s /rds/general/project/trichophyton/live/results/Jo_WGSpaper/Phylogeny/indotineae_only/indotineae_only.fa -m GTRGAMMA -p 12345 -f a -x 12345 -N 500 -n indotineae_only -w /rds/general/project/trichophyton/live/results/Jo_WGSpaper/Phylogeny/indotineae_only/
