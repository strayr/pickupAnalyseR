source('columnNames.R')
source('plotData.R')

vellemanReader <- function(filename){
  padding = 1
  
  myData = read.delim(filename)
  print(length(myData[,1]))
  startpoints = row.names (myData[which(myData$Hz == myData[1,]$Hz ), ])
  
  #if leng
  tables = list()
  
  for (i in 1:(length(startpoints) ) ){
    start = as.numeric(startpoints[i])
    
    end=length(myData[,1])
    if(i != length(startpoints)){
      end =  as.numeric(startpoints[(i+1)]) - padding -1
    }
   
    
    
    print(c(start, end))
    
    tableSlice=myData[start:end,]
    names(tableSlice)=stdNames(names(tableSlice))
    tableSlice$Freq <- as.numeric(as.character(tableSlice$Freq))
    tableSlice$mVrms <- as.numeric(as.character(tableSlice$mVrms))
    tableSlice$Mag <- as.numeric(as.character(tableSlice$Mag))
    tableSlice$Phase <- as.numeric(as.character(tableSlice$Phase))
    aTable=PlotData(table=tableSlice)
    tables[i]= aTable
    
  } 
  
  
  return(tables)
}

# Test Driver
vellmanTables <- vellemanReader("./GuitarAnalysisData/Velleman/dimarzio_super_dist.txt")