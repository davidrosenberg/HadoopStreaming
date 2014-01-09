\name{HadoopStreaming-package}
\alias{HadoopStreaming-package}
\alias{HadoopStreaming}
\docType{package}
\title{
Functions facilitating Hadoop streaming with R.
}
\description{
Provides a framework for writing map/reduce scripts for use in Hadoop
Streaming. Also facilitates operating on data in a streaming fashion,
without Hadoop.
}
\details{
\tabular{ll}{
Package: \tab HadoopStreaming\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2009-09-28\cr
License: \tab GNU \cr
LazyLoad: \tab yes\cr
}
The functions in this package read data in chunks from a file connection
(stdin when used with Hadoop streaming), package up the chunks in
various ways, and pass the packaged versions to user-supplied
functions.

There are 3 functions for reading data:
\enumerate{
\item hsTableReader is for reading data in table format (i.e. columns
separated by a separator character)
\item hsKeyValReader is for reading key/value pairs, where each is a string
\item hsLineReader is for reading entire lines as strings, without any
data parsing.
}
Only hsTableReader will break the data into chunks comprising all rows
of the same key.  This \emph{assumes} that all rows with the same key are
stored consecutively in the input file.  This is always the case if
the input file is taken to be the stdin provided by Hadoop in a
Hadoop streaming job,
since Hadoop guarantees that the rows given to the reducer are sorted
by key.  When running from the command line (not in Hadoop), we can
use the sort utility to sort the keys ourselves.

In addition to the data reading functions, the function
\code{hsCmdLineArgs} offers several default command line arguments for
doing things such as specifying an input file, the number of lines of
input to read, the input and output column separators, etc.
The \code{hsCmdLineArgs} function also facilitates packaging both the mapper
and reducer scripts into a single R script by accepting arguments
--mapper and --reducer to specify whether the call to the script
should execute the mapper branch or the reducer.

The examples below give a bit of support code for using the functions in
this package. Details on using the functions themselves can be found in
the documentation for those functions.

For a full demo of running a map/reduce script from the command line and
in Hadoop, see the directory <RLibraryPath>/HadoopStreaming/wordCntDemo/
and the README file there.
}
\author{ David S. Rosenberg <drosen@sensenetworks.com> }
\keyword{ package }
\examples{
## STEP 1: MAKE A CONNECTION

## To read from STDIN (used for deployment in Hadoop streaming and for command line testing)
con = file(description="stdin",open="r")

## Reading from a text string: useful for very small test examples
str <- "Key1\tVal1\nKey2\tVal2\nKey3\tVal3\n"
cat(str)
con <- textConnection(str, open = "r")

## Reading from a file: useful for testing purposes during development
cat(str,file="datafile.txt")            # write datafile.txt data in str 
con <- file("datafile.txt",open="r")

## To get the first few lines of a file (also very useful for testing)
numlines = 2
con <- pipe(paste("head -n",numlines,'datafile.txt'), "r")

## STEP 2: Write map and reduce scripts, call them mapper.R and
## reducer.R. Alternatively, write a single script taking command line
## flags specifying whether it should run as a mapper or reducer.  The
## hsCmdLineArgs function can assist with this.
## Writing #!/usr/bin/env Rscript can make an R file executable from the command line.

## STEP 3a: Running  on command line with separate mappers and reducers
## cat inputFile | Rscript mapper.R | sort | Rscript reducer.R
## OR
## cat inputFile | R --vanilla --slave -f mapper.R | sort | R --vanilla --slave -f reducer.R

## STEP 3b: Running on command line with the recommended single file
## approach using Rscript and the hsCmdLineArgs for argument parsing.
## cat inputFile | ./mapReduce.R --mapper | sort | ./mapReduce.R --reducer

## STEP 3c: Running in Hadoop -- Assuming mapper.R and reducer.R can
## run on each computer in the cluster:
## $HADOOP_HOME/bin/hadoop $HADOOP_HOME/contrib/streaming/hadoop-0.19.0-streaming.jar \
##   -input inpath -output outpath -mapper \
##   "R --vanilla --slave -f mapper.R" -reducer "R --vanilla --slave -f reducer.R" \
##    -file ./mapper.R -file ./reducer.R

## STEP 3d: Running in Hadoop, with the recommended single file method:
## $HADOOP_HOME/bin/hadoop $HADOOP_HOME/contrib/streaming/hadoop-0.19.0-streaming.jar \
##   -input inpath -output outpath -mapper \
##   "mapReduce.R --mapper" -reducer "mapReduce.R --reducer" \
##    -file ./mapReduce.R
}
