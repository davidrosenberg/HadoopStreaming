\name{hsCmdLineArgs}
\alias{hsCmdLineArgs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Handles command line arguments for Hadoop streaming tasks}
\description{ Offers several command line arguments useful for Hadoop
streaming. Allows specifying input and output files, column separators,
and much more. Optionally opens the I/O connections. 
}
\usage{
hsCmdLineArgs(spec=c(),openConnections=TRUE,args=commandArgs(TRUE))
}
\arguments{
  \item{spec}{A vector specifying the command line args to support.}
  \item{openConnections}{A boolean specifying whether to open the I/O connections.}
  \item{args}{Character vector of arguments. Defaults to command line args.}
}
\details{
  The \code{spec} vector has length \code{6*n}, where \code{n} is the number of command line
  arguments specified.  The \code{spec} has the same format as the \code{spec}
  parameter in the getopt function of the getopt package, though we have
  one additional entry specifying a defaut value. The six entries per
  argument are the following:
\enumerate{
\item long flag name (a multi-character string)
\item short flag name (a single character)
\item Argument specification: 0=no arg, 1=required arg, 2=optional arg
 \item Data type ('logical', 'integer', 'double', 'complex', or 'character')
  \item A string describing the option
  \item The default value to be assigned to this parameter
  }
  
  See getopt in getopt.package for details.

  The following vector defines the default command line
  args.  The vector is appended to the user-supplied \code{spec} vector in the
  call to getopt.
\preformatted{  
basespec = c(
  'mapper',     'm',0, "logical","Runs the mapper.",F,
  'reducer',    'r',0, "logical","Runs the reducer, unless already running mapper.",F,
  'mapcols',    'a',0, "logical","Prints column headers for mapper output.",F,
  'reducecols', 'b',0, "logical","Prints column headers for reducer output.",F,
  'infile'   ,  'i',1, "character","Specifies an input file, otherwise use stdin.",NA,
  'outfile',    'o',1, "character","Specifies an output file, otherwise use stdout.",NA,
  'skip',       's',1,"numeric","Number of lines of input to skip at the beginning.",0,
  'chunksize',  'C',1,"numeric","Number of lines to read at once, a la scan.",-1,
  'numlines',   'n',1,"numeric","Max num lines to read per mapper or reducer job.",0,
  'sepr',       'e',1,"character","Separator character, as used by scan.",'\t',
  'insep',      'f',1,"character","Separator character for input, defaults to sepr.",NA,
  'outsep',     'g',1,"character","Separator character output, defaults to sepr.",NA,
  'help',       'h',0,"logical","Get a help message.",F
  )
  
}
    
  }
\value{
  Returns a list.  The names of the entries in the list are the long
  flag names.  Their values are either those specified on the command
  line, or the default values.

  If openConnections=TRUE, then the returned list has two additional
  entries: incon and outcon.  incon is a readable connection to the
  input source specified, and outcon is a writable connection to the
  appropriate output destination.
  
  An additional entry in the returned list is named \code{'set'}.
  When this list entry is FALSE, none of the options were set
  (generally because -h or --help was requested).  The calling
  procedure should probably stop execution when the \code{'set'} is
  returned as FALSE.
}
\author{David S. Rosenberg \email{drosen@sensenetworks.com} }
\seealso{This package relies heavily on package \pkg{getopt}}
\examples{
spec = c('myChunkSize','C',1,"numeric","Number of lines to read at once, a la scan.",-1)
## Displays the help string
hsCmdLineArgs(spec, args=c('-h'))
## Call with the mapper flag, and request that connections be opened
opts = hsCmdLineArgs(spec, openConnections=TRUE,args=c('-m'))
opts  #   a list of argument values
opts$incon # an input connection
opts$outcon # an output connection
}
