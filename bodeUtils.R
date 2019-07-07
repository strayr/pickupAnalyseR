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

processBode <- function (bodeData, fMin=250, fMax=1001, gain=0.0, smoothing=0.07) {
  #Apply -20dB/decade slope
  
  bodeData$IntMag <- (bodeData$Mag - 20 * log10(bodeData$Freq) )
  
  
  
  
  #TODO check to see if a gain offeset is supplied, and apply this instead
  #of calculating 0db line. Only use to compare relative responses of pickups
  #on the same test circut.
  # 
  # loessMod <- loess(IntMag~Freq, bodeData, span=smoothing) #span 0.07 here
  # smoothed <- predict(loessMod)
  bodeData$smIntMag <- tryCatch({
    loessMod <- loess(IntMag~Freq, bodeData, span=smoothing) #span 0.07 here
    smoothed <- predict(loessMod)
    smoothed
  },
  error = function(err){
    bodeData$IntMag
  }) #End tryCatch
  
  #chop out the section that should be flatish
  fRange <- subset(bodeData, Freq > fMin & Freq < fMax, smIntMag)
  
  #calculate gain offset
  gain <- 0 - mean(fRange$smIntMag)
  #and apply
  
  bodeData$IntRelMag <- (bodeData$IntMag + gain)
  bodeData$smRelMag <- (bodeData$smIntMag + gain)
  #bodedata$smRelMag
  #print(head(bodeData))
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