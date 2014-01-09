\name{hsLineReader}
\alias{hsLineReader}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A wrapper for readLines }
\description{
This function repeatedly reads \code{chunkSize} lines of data from
\code{con} and passes a character vector of these strings to
\code{FUN}. The first \code{skip} lines of input are ignored.
}
\usage{
hsLineReader(con = "", chunkSize = -1, skip = 0, FUN = function(x) cat(x, sep = "\n"))
}
\arguments{
  \item{con}{A connection object or a character string, as in readLines.}
  \item{chunkSize}{The (maximal) number of lines to read at a
  time. The default is -1, which specifies that the whole file should be read at once.}
  \item{skip}{Number of lines to ignore at the beginning of the file}
  \item{FUN}{A function that takes a character vector as input}
}
\details{
Warning: A feature(?) of readLines is that if there is a newline before the EOF, an extra empty string is returned.
}
\value{
  No return value.
}
\author{David S. Rosenberg.  <\email{drosen@sensenetworks.com}>}
\examples{
  str <- "Hello here are some\nlines of text\nto read in, chunkSize\nlines at a time.\nHow interesting.\nhuh?"
  cat(str)
  con <- textConnection(str, open = "r")
  hsLineReader(con,chunkSize=-1,FUN=print)
  close(con)
  con <- textConnection(str, open = "r")
  hsLineReader(con,chunkSize=3,skip=1,FUN=print)
  close(con)
}
