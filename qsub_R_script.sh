#!/bin/bash
#$ -S /bin/bash
#$ -cwd
#$ -N NA_test2
#$ -o logs
#$ -j y
#$ -V
#$ -l h_vmem=10G

export R_HOME=/ifshome/yeunkim/R-3.2.3/lib64/R

if [ -z "$SGE_TASK_ID" ] || [ "$SGE_TASK_ID" == "undefined" ]; then
perm=$1
echo perm="$perm"
else

perm=`sed "${SGE_TASK_ID}p;d" $1`
fi;

/ifshome/yeunkim/R-3.2.3/bin/R --slave --args ~/BrainSuite16a/svreg/BrainSuiteAtlas1/ Sex*Score Sex:Score simulated_data_with_NA.csv $perm < permutation_test_R_test.R
