#!/bin/bash

if [ $# -lt 1 ]; then # $# number of arguments that were passed onto the command line (less than 1)
	echo "usage: $0 [log_file_prefix] [output_file_prefix] [observed_tvalue_file]"
	exit 1;	
fi;

args=$@

log_prefix=$1
output_prefix=$2
tobs=$3

PWD=$(pwd)

echo "log file prefix:" $log_prefix

grep t-value: logs/${log_prefix}* > ${output_prefix}_tmp.txt

python null_distribution_R.py ${PWD}/${output_prefix}_tmp.txt ${PWD}/${output_prefix}.txt

sed -i 's/"//g' ${output_prefix}.txt

~/R-3.2.3/bin/R --slave --args $tobs ${output_prefix}.txt < find_num_of_sig_voxels.R
