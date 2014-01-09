`hsLineReader` <-
function(con="",chunkSize=-1L, skip=0L, FUN=function(x) cat(x,sep='\n')) {

  if (is.character(con)) {
    con <- file(con, "r")
    on.exit(close(con))
  }

  if (skip>0) {
    junk = readLines(con, n=skip)
  }
  repeat {
    a = readLines(con, n=chunkSize,warn=TRUE)
    if (length(a) ==0) break
    FUN(a)
  }
}

