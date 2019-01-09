source('columnNames.R')
source('plotData.R')

vellmanReader <- function(filename){
  padding = 1
  
  test = read.delim(filename)
  startpoints = row.names (test[which(test$Hz == test[1,]$Hz ), ])
  
  #if leng
  tables = list()
  
  for (i in 1:(length(startpoints)-1 ) ){
    start = as.numeric(startpoints[i])
    end =  as.numeric(startpoints[(i+1)]) - padding -1
    tableSlice=test[start:end,]
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
vellmanTables <- vellmanReader("./GuitarAnalysisData/Velleman/longdata.txt")