#SysCompPickup

if(!exists("libfolder")) {libfolder<-'.'}
source (paste(libfolder, 'pickup.R', sep="/"))
source (paste(libfolder, 'columnNames.R', sep="/"))


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
    ldTable=read.table(file = ld, sep=',', header = TRUE)
    itTable=read.table(file = it, sep=',', header = TRUE)
    ulTable=read.table(file = ul, sep=',', header = TRUE)
    
    names(ldTable)=stdNames(names(ldTable))
    names(itTable)=stdNames(names(itTable))
    names(ulTable)=stdNames(names(ulTable))
    
    setLoaded(ldTable)
    setInduction(itTable)
    setUnloaded(ulTable)
  
    smoothing<<-defaultSmoothing
    
  }
)