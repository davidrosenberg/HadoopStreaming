`hsKeyValReader` <-
  function(con=file(description="stdin",open="r"),chunkSize=-1,skip=0, sep='\t',FUN=function(k,v) cat(paste(k,v,sep=': '),"\nEND-OF-BLOCK\n",sep='\n'), ignoreKey=FALSE,carryMemLimit=512e6,carryMaxRows=Inf,stringsAsFactors=FALSE,debug=FALSE) {

    if (is.character(con)) {
      con <- file(con, "r")
      on.exit(close(con))
    }

    ## flush=TRUE  (allows comment, but precludes more than one record per line, which is good)
    if (skip > 0) {
      junk = readLines(con, n=skip)
    }
    
    if (ignoreKey) {
      repeat {
        a = readLines(con, n=chunkSize,warn=FALSE)
        if (length(a) ==0) break
        
        keyBoundary = regexpr(sep,a)
        keys = substr(a,start=1,stop=(keyBoundary-1))
        vals = substr(a,start=(keyBoundary+1),stop=nchar(a))
        
        FUN(keys,vals)
      }
      return(invisible())
    }

    ## Carryover list
    aCarryKeys = c()
    aCarryVals = c()
    fileEmpty = TRUE
    repeat {
      if (object.size(aCarryVals)>carryMemLimit || length(aCarryVals) > carryMaxRows) {
        cat('In hsTableReader, aCarryVals has exceeded defined limits on memory and/or rows\n',file=stderr())
        cat('Key=',aCarryKeys[1],' MemoryUsed=',object.size(aCarryVals)/(2^20),'MB; NumEntries=',length(aCarryVals),'\n',sep='',file=stderr())
        cat('Consider using higher values for carryMemLimit and carryMaxRows,\nOR use PFUN to handle incomplete keys.\n',file=stderr())
        ## Throw out the carryover data because getting too big
        aCarryVals=list()
      }
      a = readLines(con, n=chunkSize,warn=TRUE)
      ## Memory Report
      if (debug) {
        cat('In hsKeyValReader, we have just scanned ',object.size(a)/(2^20),'MB. Current carry size is ',object.size(aCarryVals)/(2^20),'\n',file=stderr())
      }
      ## Done processing, because scan returned nothing
      if ( length(a) == 0 ) break
      fileEmpty = FALSE
      ## Prepend last carry to new data and convert scanned stuff to data.frame
      if (length(a) ==0) break

      keyBoundary = regexpr(sep,a)
      keys = substr(a,start=1,stop=(keyBoundary-1))
      vals = substr(a,start=(keyBoundary+1),stop=nchar(a))

      keys = c(aCarryKeys, keys)
      vals = c(aCarryVals, vals)


      r = rle(keys)
      numDistinctKeys = length(r$values)
      if (numDistinctKeys == 1) {
        aCarryKeys = keys
        aCarryVals = vals
        next
      }
      firstEntryOfLastKey = length(keys) - r$lengths[numDistinctKeys] + 1
      if (firstEntryOfLastKey <=1 || firstEntryOfLastKey > length(keys)) stop("Problem with firstEntryOfLastKey")
      aCarryKeys = keys[firstEntryOfLastKey:length(keys)]
      aCarryVals = vals[firstEntryOfLastKey:length(keys)]

      ## Process all complete keys, one at a time
      startPos = 1
      for (keyNum in 1:(numDistinctKeys-1)) {
        endPos = startPos+r$lengths[keyNum]-1
        FUN(keys[startPos:endPos],vals[startPos:endPos])
        startPos = endPos+1
      }
      if (startPos != firstEntryOfLastKey) stop("startPos != firstEntryOfLastKey")
    }
    if (!ignoreKey && !fileEmpty && length(vals)==0) stop ("empty aCarryVals at end -- this should never happen!!!")
    if (length(aCarryVals)>0) {
      FUN(aCarryKeys, aCarryVals)
    }
    return(invisible())
  }
