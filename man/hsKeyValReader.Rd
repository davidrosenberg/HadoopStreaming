\name{hsKeyValReader}
\alias{hsKeyValReader}
 %- Also NEED an '\alias' for EACH other topic documented here.
\title{Reads key value pairs}
\description{
  Uses \code{scan} to read in chunkSize lines at a time, where each line
  consists of a key string and a value string.  The first \code{skip}
  lines of input are skipped.  Each group of key/value pairs are passed
  to FUN as a character vector of keys and character vector of values.
}
\usage{
hsKeyValReader(con = "", chunkSize = -1, skip = 0, sep = "\t",FUN = function(k, v) cat(paste(k, v, sep = ": "), sep = "\n"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{con}{A connection object or a character string, as in scan.}
  \item{chunkSize}{The (maximal) number of lines to read at a
  time. The default is -1, which specifies that the whole file should be read at once.}
  \item{skip}{Number of lines to ignore at the beginning of the file}
  \item{FUN}{A function that takes a character vector as input}
  \item{sep}{The character separating the key and the value strings.}
}
\value{
  No return value.
}
\author{David S. Rosenberg.  <\email{drosen@sensenetworks.com}>}
\examples{
  printFn <- function(k,v) {
    cat('A chunk:\n')
    cat(paste(k,v,sep=': '),sep='\n')
  }
  str <- "key1\tval1\nkey2\tval2\nkey3\tval3\n"
  cat(str)
  con <- textConnection(str, open = "r")
  hsKeyValReader(con,chunkSize=2,FUN=printFn)
  close(con)
  con <- textConnection(str, open = "r")
  hsKeyValReader(con,FUN=printFn)
  close(con)
}
