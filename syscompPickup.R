#SysCompPickup
source('pickup.R')
SysCompPickup <- setRefClass(
  "SyscompPickup",
  contains="Pickup"
)

SysCompPickup$methods(
  initialize = function(tableBase, ...){
    autoSetFromFiles(tableBase)
    callSuper(...)
  }
)

SysCompPickup$methods(
  autoSetFromFiles = function(fileStem) {
    ul <- paste0(fileStem, "-UL.csv")
    ld <- paste0(fileStem, "-LD.csv")
    it <- paste0(fileStem, "-IT.csv")
 
    setLoaded(read.table(file = ld, sep=',', header = TRUE))
    setInduction(read.table(file = it, sep=',', header = TRUE))
    setUnloaded(read.table(file = ul, sep=',', header = TRUE))
  
    smoothing<<-defaultSmoothing
    
  }
)