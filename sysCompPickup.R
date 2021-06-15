#SysCompPickup

#if(!exists("libfolder")) {libfolder<-'.'}
source ('pickup.R')
source ('columnNames.R')
source ('DataImport/syscompReader.R')


SysCompPickup <- setRefClass(
  "SysCompPickup",
  contains="Pickup"
)

SysCompPickup$methods(
  initialize = function(tableBase, ...){
    callSuper(...)
    autoSetFromFiles(tableBase)
    
  }
)

SysCompPickup$methods(
  autoSetFromFiles = function(fileStem) {
    ul <- paste0(fileStem, "-UL.csv")
    ld <- paste0(fileStem, "-LD.csv")
    it <- paste0(fileStem, "-IT.csv")
    #print(ld)
    ldTable=syscompReader(ld)
    itTable=syscompReader(it)
    ulTable=syscompReader(ul)
    
    setLoaded(ldTable)
    setInduction(itTable)
    setUnloaded(ulTable)
  
    smoothing<<-defaultSmoothing
    
  }
)