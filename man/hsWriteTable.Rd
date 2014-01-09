\name{hsWriteTable}
\alias{hsWriteTable}
\title{Calls write.table with defaults sensible for Hadoop streaming.}
\description{
Calls write.table without row names or column names, without string
quotes, and with tab as the default separator.
 }
\usage{
hsWriteTable(d, file = "", sep = "\t")
}
\arguments{
  \item{d}{A data frame}
  \item{file}{A connection, as taken by write.table()}
  \item{sep}{The column separator, defaults to a tab character}
}
\value{
  No return value.
}
\author{David S. Rosenberg <drosen@sensenetworks.com> }
\seealso{\code{\link{write.table}} }
\examples{
d=data.frame(a=c('hi','yes','no'),b=c(1,2,3))
rownames(d) <- c('row1','row2','row3')
write.table(d)
hsWriteTable(d)
}
