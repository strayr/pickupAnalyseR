#vellemanPickup
source('pickup.R')
source('vellemanReader.R')
source('plotData.R')
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
    tables=vellemanReader(tableBase)
    if(loadedIndex != 0){
      print(loadedIndex)
      setLoaded(tables[[loadedIndex]]$table)
    }
    if(unloadedIndex != 0){
      print(loadedIndex)
      setUnloaded(tables[[unloadedIndex]]$table)
    }
    
    
    
    
    callSuper(...)
  }
)