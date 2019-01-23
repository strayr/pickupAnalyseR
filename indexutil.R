## Functions for returning the index of a vector matching a criteria


##Returns the index of the largest value, if several are identical then returns the middle index
maxIndex <- function(aList) {
  peak=-100.0
  peakIndex=0
  peakStart=0
  peakEnd=0
  onPeak=FALSE
  spreadCount=0
  for (index in 1:length(aList)){
    if (aList[index] > peak) {
      peak <- aList[index]
      peakStart=index
      peakEnd=index
      if (onPeak) {
      }else{
        onPeak=TRUE
      }
    } 
    if (aList[index] == peak) {
      peakEnd <- index
      onPeak<-TRUE
    }
    
    if (aList[index] < peak){
      if(onPeak){
        onPeak <- FALSE
        peakEnd <- index - 1
      }
      
    } 
  }
  if (peakEnd > peakStart){
    peakIndex <- peakStart + floor(0.5 * (peakEnd-peakStart))
  } else {
    peakIndex <- peakStart
  }
  return (peakIndex)
}