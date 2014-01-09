hsWriteTable <- function(d, file="",sep='\t',quote=FALSE,row.names=FALSE,
                         col.names=FALSE) {
  write.table(x=d,file=file,sep=sep,quote=quote,row.names=row.names,col.names=col.names)
}
