
source ('columnNames.R')
syscompReader <- function(tableSource){
  bode<-read.table(file = tableSource, sep=',', header = TRUE)
  names(bode)<-stdNames(names(bode))
  return(bode)
}