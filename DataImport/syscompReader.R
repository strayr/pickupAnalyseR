if(!exists("libfolder")) {libfolder<-'.'}
source(paste(libfolder,"columnNames.R", sep = "/"))
syscompReader <- function(tableSource){
  bode<-read.table(file = tableSource, sep=',', header = TRUE)
  names(bode)<-stdNames(names(bode))
  return(bode)
}
