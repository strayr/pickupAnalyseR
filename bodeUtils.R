## Does Munching of bode plots



shortNames <- function (bodeData) {
  names.o<-names(bodeData)
  names(bodeData)[names.o == "Frequency..Hz."]<-"Freq"
  names(bodeData)[names.o == "Magnitude..dB."]<-"Mag"
  names(bodeData)[names.o == "Phase..deg."]<-"Phase" 
  return(names(bodeData))
}

removeExcess <- function(bodeData){
  drop<- c("Quality", "Phase..deg.")
  return (bodeData[,!(names(bodeData) %in% drop)])
}

processBode <- function (bodeData, fMin=100, fMax=500, gain=0.0) {
  #Apply -20dB/decade slope
  print(head(bodeData))
  bodeData$IntMag <- (bodeData$Mag - 20 * log10(bodeData$Freq) )
  
  
  #chop out the section that should be flatish
  fRange <- subset(bodeData, Freq > fMin & Freq < fMax, IntMag)
  
  #TODO check to see if a gain offeset is supplied, and apply this instead
  #of calculating 0db line. Only use to compare relative responses of pickups
  #on the same test circut.
  
  #calculate gain offset
  gain <- 0 - mean(fRange$IntMag)
  #and apply
  bodeData$IntMag <- (bodeData$IntMag + gain)
  loessMod <- loess(IntMag~Freq, bodeData, span=0.07)
  smoothed <- predict(loessMod)
  bodeData$smIntMag <-smoothed
  return (bodeData)
}

log10_minor_break = function (...){
  function(x) {
    minx         = floor(min(log10(x), na.rm=T))-1;
    maxx         = ceiling(max(log10(x), na.rm=T))+1;
    n_major      = maxx-minx+1;
    major_breaks = seq(minx, maxx, by=1)
    minor_breaks = 
      rep(log10(seq(1, 9, by=1)), times = n_major)+
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }
}