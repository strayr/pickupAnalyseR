#vellemanReader.R



if (!exists("libfolder")) {
  libfolder <- '.'
}
source(paste(libfolder, 'columnNames.R', sep="/"))
source(paste(libfolder, 'plotData.R', sep="/"))

# returns the plot between 
vellemanReader <- function(filename, index=1){
  padding = 1
  
  myData = read.delim(filename)
  if(exists("DEBUG")){print(length(myData[,1]))}
  startpoints = row.names (myData[which(myData$Hz == myData[1,]$Hz ), ])
  
  #if leng
  tables = list()
  
  
    start = as.numeric(startpoints[index])
    
    end=length(myData[,1])
    if(length(startpoints) > index){
      end =  as.numeric(startpoints[(index+1)]) - padding -1
    }
   
    
    if(exists("DEBUG")){
    print(c("start",start))
    print(c("end",end))
    }
    
    tableSlice=myData[start:end,]
    names(tableSlice)=stdNames(names(tableSlice))
    tableSlice$Freq <- as.numeric(as.character(tableSlice$Freq))
    tableSlice$Volts <- as.numeric(as.character(tableSlice$Volts))
    tableSlice$Mag <- as.numeric(as.character(tableSlice$Mag))
    tableSlice$Phase <- as.numeric(as.character(tableSlice$Phase))
    aTable<-(tableSlice)
    #tables[i]= aTable
    
  
  
  
  return(aTable)
}

# Test Driver
#vellmanTables <- vellemanReader("./GuitarAnalysisData/Velleman/dimarzio_super_dist.txt")