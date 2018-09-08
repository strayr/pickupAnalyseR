#pickup.R


## Ok let's make a formal model of af a pickup as a reference class. I want
#class methods available so I can process with class methods and have reference
#behaviour so a method causes the the object to do a BIG computation and store
#the result rather than risk repeated recalculation or copy-on-write with large
#data.

#External packages
#For Graphing
library(ggplot2)
library(scales)
library(ggpmisc)

#For melting data
library(purrr)

#library(peakPick)
library(quantmod)

#I have some packages of useful functions that I should probably internalise here
source ('indexutil.R')
source('bodeUtils.R')


#TODO handle this
defaultSmoothing <- 0.07
defaultProbeCap <- 20 #in pF

# I'm just defining fields here, methods can go in one at a time for code clarity
Pickup <- setRefClass(
  "Pickup",
  fields = list(
    name = "character",
    manuf = "character",
    
    tableBase = "character",
    smoothing="numeric",
    
    #The big unweildy data from datalogger or SPICE
    loaded = "data.frame",
    unloaded = "data.frame",
    induction = "data.frame",
    
    # #Analysis hints
    # ldHasEddy="logical",
    # ldHasResonance="logical",
    # ulHasEddy="logical",
    # ulHasResonance="logical",
    
    #Measured Values
    mDCR = "numeric",
    
    #caluclatable values
    #Set to -1 if not calculated or able to be calculated 
    #Setting data tables and get* methods attempt calculation
    cCap = "numeric", #use getCap()
    cInd = "numeric"  #use getInd()
    
    
  )
) #End pickup def


# Pickup$methods (
#   initialize= function() {
#    }
# )
# 
# Pickup$methods (
#   initialize= function(...) {
#     print (tableBase)
#     tableBase <<- tableBase
#   }
# )


# A class method for internal use to identify peaks
Pickup$methods(
  priSimplePeak = function(bodeData) {
    loessMod = loess(Mag ~ Freq, bodeData, span = defaultSmoothing)
    smoothed = predict(loessMod)
    index = maxIndex(smoothed)
    peak = list()
    peak$freq = bodeData$Freq[index]
    peak$mag = smoothed[index]
    return(peak)
  }
)

Pickup$methods(
  setLoaded = function(x) {
    x<-removeExcess(x)
    names(x)<-shortNames(x)
    x<-processBode(x)
    loaded <<- x
  }
)



Pickup$methods(
  setUnloaded = function(x) {
    x<-removeExcess(x)
    names(x)<-shortNames(x)
    x<-processBode(x)
    unloaded <<- x
    #We've just invalidated our calculation and can't garantee having Induction data so reset
    cCap <<- (-1)
    # getCap()
  }
)

Pickup$methods(
  setInduction = function(x) {
    x<-removeExcess(x)
    names(x)<-shortNames(x)
    x<-processBode(x)
    induction <<- x
    #We've just invalidated our calculation and can't garantee having capacitance data so reset
    cInd <<- (-1)
    cCap <<- (-1)
    #now we can attempt to recalculate Induction
    # getInd()
    # getCap()
  }
)

##
# This assumes the bode plots are in CSV files with systemic prefixes

Pickup$methods(
  setFromFiles = function(fileStem=tableBase, schema="SYSCOMP") {
    ul <- paste0(fileStem, "-UL.csv")
    ld <- paste0(fileStem, "-LD.csv")
    it <- paste0(fileStem, "-IT.csv")
    setLoaded(read.table(file = ld, sep=',', header = TRUE))
    setInduction(read.table(file = it, sep=',', header = TRUE))
    setUnloaded(read.table(file = ul, sep=',', header = TRUE))
    smoothing<<-defaultSmoothing
   
  }
)


# Pickup$methods(
#   assumeTrue = function() {
#     ldHasEddy <<- TRUE
#     ldHasResonance <<- TRUE
#     ulHasEddy <<- TRUE
#     ulHasResonance <<- TRUE
#   }
# )

## Acessor getInd for variable cIND
Pickup$methods(
  getInd = function() {
    if (cInd>0) {
      return (cInd)
    } else {
      inF=priSimplePeak(induction)
      cInd <<- 1 / (((2 * pi * inF$freq) ^ 2) * (10^-8))
      return (cInd)
    }
    
  }
)

## Accessor getCap for variable cCap
Pickup$methods(
  getCap = function() {
    if (cCap>0) {
      return (cCap)
    } else {
      ulF=priSimplePeak(unloaded)
      cCap <<- (1/(((2 * pi * ulF$freq) ^ 2) * getInd())) * (10^12) - defaultProbeCap
      return (cCap)
    }
    
  }
)

##
# Data massaging to make uniqe columns when we merge and melt
#TODO just melt the data here so we can do a simple concatination

Pickup$methods(
  preMergeLoaded = function() {
    lOut <- loaded[,(names(loaded) %in% c("Freq", "IntMag"))]
    names(lOut)<-c("Freq", name)
    return(lOut)
  }

)

Pickup$methods(
  preMergeUL = function() {
    lOut <- unloaded[,(names(unloaded) %in% c("Freq", "IntMag"))]
    names(lOut)<-c("Freq", name)
    return(lOut)
  }
  
)

Pickup$methods(
  getLDCutoff = function(){
    cutoff=loaded[head(which(loaded$smIntMag < -3)[1],n=1),"Freq"]
    return(cutoff)
  }
)

Pickup$methods(
  getULCutoff = function(){
    cutoff=unloaded[head(which(unloaded$smIntMag < -3)[1],n=1),"Freq"]
    return(cutoff)
  }
)

Pickup$methods(
  getLDPeaks = function(){
    peakIndex <- (findPeaks(as.matrix(loaded)[,4]))
    return(loaded[peakIndex,] )
  }
)

Pickup$methods(
  getULPeaks = function(){
    peakIndex <- (findPeaks(as.matrix(unloaded)[,4]))
    return(unloaded[peakIndex,] )
  }
)


###
# We can build in some ggplot defaults

Pickup$methods(
  getPlot = function(){
    myPlot = ggplot (data = unloaded) +
      scale_x_log10(minor_breaks = log10_minor_break()) +
      theme(
        panel.grid.major.x = element_line(size = 0.1),
        panel.grid.minor.x = element_line(size = 0.2)
      ) +
      
      geom_line(mapping = aes(x = Freq , y = IntMag)) +
      geom_smooth(mapping = aes(x = Freq , y = IntMag) , span = smoothing) +
      geom_line(data = unloaded, mapping = aes(x = Freq , y = IntMag)) +
      geom_smooth(
        data = aPickup$loaded,
        mapping = aes(x = Freq , y = IntMag) ,
        span = smoothing,
        colour = "red"
      ) +
      
      geom_vline(xintercept = tail(getLDPeaks()[,"Freq"], n=1), colour = "red")+
      geom_vline(xintercept = getLDCutoff(), colour = "red")+
      geom_vline(xintercept = tail(getULPeaks()[,"Freq"], n=1), colour = "blue")+
      geom_vline(xintercept = getULCutoff(), colour = "blue")+
      
      
      
      ylim(-10, 6) +
      ggtitle(paste(aPickup$manuf, aPickup$name)) +
      xlab("Frequency /Hz") +
      ylab("Magnetude /dB (-20db/Decade)")
    return(myPlot)
  }
)

##
# This really DOES NOT WORK
# Pickup$methods (
#   getDatasheet = function() {
#     data=paste(
#       "Calculated Inductance:",
#       prettyNum(getInd(), format = "fg", digits = 3),
#       "H\n",
#       "Calculated Capacitance:", prettyNum(getCap(), format = "fg", digits = 3),"pF\n",
#       #LDCutoff #ULCutoff #LDPeaks #ULPeaks
#       "Loaded:\nResonant Peak:", getLDPeaks()
#     )
#     return(data)
#   }
# )
# 
