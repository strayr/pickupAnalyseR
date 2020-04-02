#SysCompPickup

if(!exists("libfolder")) {libfolder<-'.'}
source (paste(libfolder, 'pickup.R', sep="/"))
source (paste(libfolder, 'columnNames.R', sep="/"))
source (paste(libfolder, 'DataImport/syscompReader.R', sep="/"))


SysCompPickup <- setRefClass(
  "SyscompPickup",
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