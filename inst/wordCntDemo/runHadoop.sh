#!/bin/env bash
HADOOP="hadoop"   # Hadoop command (either change, or put hadoop in the path)
HADOOPSTREAMING="$HADOOP jar $HADOOP_HOME/contrib/streaming/hadoop-0.19.1-streaming.jar" # change version number as appropriate
RLIBPATH=$HOME/RLibrary  # can specify additional R Library paths here

INPUTFILE="anna.txt"
HFSINPUTDIR="input"
OUTDIR="annaWordCnt"
RFILE="hsWordCnt.R"
LOCALOUT="annaWordCnts.out"
# Put the file into the Hadoop file system
$HADOOP fs -put $INPUTFILE $HFSINPUTDIR

# Remove the directory if already exists (otherwise, won't run)
$HADOOP fs -rmr $OUTDIR

MAPARGS="--mapper"  
REDARGS="--reducer"
JOBARGS="-cmdenv R_LIBS=$RLIBPATH" # numReduceTasks 0
echo $HADOOPSTREAMING -cmdenv R_LIBS=$RLIBPATH  -input $HFSINPUTDIR/$INPUTFILE -output $OUTDIR -mapper "$RFILE $MAPARGS" -reducer "$RFILE $REDARGS" -file $RFILE 
$HADOOPSTREAMING $JOBARGS   -input $HFSINPUTDIR/$INPUTFILE -output $OUTDIR -mapper "$RFILE $MAPARGS" -reducer "$RFILE $REDARGS" -file $RFILE 

# Extract output
./$RFILE --reducecols > $LOCALOUT
$HADOOP fs -cat $OUTDIR/part* >> $LOCALOUT


# Look at output directory
# $HADOOP fs -ls $OUTDIR

# Look at output
#$HADOOP fs -cat $OUTDIR/part*

# Look at error log 
# $HADOOP fs -cat $OUTDIR/_logs/history/*.jar


