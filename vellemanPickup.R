#vellemanPickup
if(!exists("libfolder")) {libfolder<-'.'}
source (paste(libfolder, 'pickup.R', sep="/"))
source (paste(libfolder, 'DataImport/vellemanReader.R', sep="/"))
#source (paste(libfolder, 'plotData.R', sep="/"))
source (paste(libfolder, 'columnNames.R', sep="/"))

VellemanPickup <- setRefClass(
  "VellemanPickup",
  contains="Pickup",
  fields=list(
    tableBase="character",
    unloadedIndex="numeric",
    loadedIndex="numeric",
    indIndex="numeric"
  )
)

VellemanPickup$methods(
  initialize = function(tableBase, unloadedIndex=0, loadedIndex=0, indIndex=0, ...){
   
    if(loadedIndex != 0){
      # print("vellemanPickup 23")
      # print(loadedIndex)
      loadedTable=vellemanReader(tableBase, loadedIndex)
      print(head(loadedTable))
      setLoaded(loadedTable)
    }
    if(unloadedIndex != 0){
      # print("vellemanPickup 30")
      # print(loadedIndex)
      unloadedTable=vellemanReader(tableBase, unloadedIndex)
      print(head(unloadedTable))
      setUnloaded(unloadedTable)
    }
    if(indIndex != 0){
      # print("vellemanPickup 37")
      # print(indIndex)
      indTable=vellemanReader(tableBase, indIndex)
      print(head(indTable))
      setInduction(indTable)
    }
    
    
    
    
    callSuper(...)
  }
)

##
# vellemanReader() Static function I only need here
# 
# vellemanReader <- function(filename, index = 1) {
#   padding = 1
#   
#   myData = read.delim(filename)
#   print(length(myData[, 1]))
#   startpoints = row.names (myData[which(myData$Hz == myData[1, ]$Hz),])
#   
#   #if leng
#   tables = list()
#   
#   
#   start = as.numeric(startpoints[index])
#   
#   end = length(myData[, 1])
#   if (index != length(startpoints)) {
#     end =  as.numeric(startpoints[(index + 1)]) - padding - 1
#   }
#   
#   
#   
#   print(c(start, end))
#   
#   tableSlice = myData[start:end, ]
#   names(tableSlice) = stdNames(names(tableSlice))
#   tableSlice$Freq <- as.numeric(as.character(tableSlice$Freq))
#   tableSlice$Volts <- as.numeric(as.character(tableSlice$Volts))
#   tableSlice$Mag <- as.numeric(as.character(tableSlice$Mag))
#   tableSlice$Phase <- as.numeric(as.character(tableSlice$Phase))
#   aTable = tableSlice
#   
#   
#   
#   
#   
#   return(aTable)
# }
