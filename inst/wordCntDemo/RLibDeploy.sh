#!/bin/env bash  

# How you might conveniently send files to each machine in the cluster and install R Libraries there

# Rsync out to other computers
for srv in $(cat $HADOOP_HOME/conf/slaves); do  
  echo "Sending command to $srv...";   
#  rsync -av ~/sourceDir $srv:destDir
#  ssh $srv "R CMD INSTALL --no-docs -l ~/RLibrary ~/RPackages/HadoopStreaming"
#  ssh $srv "R CMD INSTALL --no-docs -l ~/RLibrary ~/RPackages/CRAN/*"
done  
  
echo "done." D
