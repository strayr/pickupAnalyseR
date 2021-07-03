#pickup.R


## Ok let's make a model of af a pickup as a reference class. I want
#class methods available so I can process with class methods and have reference
#behaviour so a method causes the the object to do a BIG computation and store
#the result rather than risk repeated recalculation or copy-on-write with large
#data.

#TODO internalise peak tabulation
#TODO Make bias gain available


#External packages
#For Graphing
library(ggplot2)
library(scales)
#library(ggpmisc)

#For melting data
library(purrr)
library(reshape2)

#Using a finance library for peak detecton
library(quantmod)

#I have some packages of useful functions that I should probably internalise here
# if (!exists("libfolder")) {
#   libfolder <- '.'
# }
if(!exists("libfolder")) {libfolder<-'.'}
source(paste(libfolder,"indexutil.R", sep = "/"))
source(paste(libfolder,"bodeUtils.R", sep = "/"))



#TODO handle this
defaultSmoothing <- 0.05
defaultProbeCap <- 20 #in pF

# I'm just defining fields here, methods can go in one at a time for code clarity
Pickup <- setRefClass(
  "Pickup",
  fields = list(
    name = "character",
    manuf = "character",
    #hasLoaded = "logical",     #Not using
    #hasUnloaded = "logical",   #Not Using
    #hasInd = "logical",        #Not Using
    #tableBase = "character",
    smoothing = "numeric",

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
    cCap = "numeric",
    #use getCap()
    cInd = "numeric",
    #use getInd()


    #Store the peaks from the raw plots, this is calculated and cached data so use the accessors
    indRawPeak = "list",
    #use getIndRawPeak()
    ldRawPeak = "list",
    #use getLdRawPeak()
    ulRawPeak = "list", #use getULRawPeak()

    #Count this many peaks to the left
    LDOffset ="numeric",
    ULOffset = "numeric"
    #
  )
) #End pickup def


# Pickup$methods(
#   initialize=function(smoothing=0, ...) {
#     if (smoothing==0) {
#       smoothing <<- defaultSmoothing
#     } else {
#       smoothing <<- smoothing
#     }
#     manuf <<- manuf
#     name <<- name
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
    #print(peak) #debug
    return(peak)
  }
)

Pickup$methods(
  setLoaded = function(x) {
    #x<-removeExcess(x)
    #names(x)<-shortNames(x)
    # print(head(x))
    loaded <<- processBode(x)
    # print(head(loaded))
  }
)



Pickup$methods(
  setUnloaded = function(ulTable) {
    # x<-removeExcess(x)
    #names(x)<-shortNames(x)
    # print("pickup.R 109")
    # print(head(ulTable))
    # print("pickup.R 111")
    unloaded <<- processBode(ulTable, smoothing = smoothing)
    #We've just invalidated our calculation and can't garantee having Induction data so reset
    cCap <<- (-1)
    # getCap()
  }
)

Pickup$methods(
  setInduction = function(x) {
    induction <<- processBode(x, smoothing = smoothing)
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
# this has been shifted to data-specific classes derived from this

# Pickup$methods(
#   setFromFiles = function(fileStem=tableBase, schema="SYSCOMP") {
#     ul <- paste0(fileStem, "-UL.csv")
#     ld <- paste0(fileStem, "-LD.csv")
#     it <- paste0(fileStem, "-IT.csv")
#     if(hasLoaded){
#       setLoaded(read.table(file = ld, sep=',', header = TRUE))
#     }
#     if(hasInd){
#       setInduction(read.table(file = it, sep=',', header = TRUE))
#     }
#     if(hasUnloaded){
#       setUnloaded(read.table(file = ul, sep=',', header = TRUE))
#     }
#     smoothing<<-defaultSmoothing
#
#   }
# )


## Acessor getInd for variable cIND
Pickup$methods(
  getInd = function() {
    if (is.null(induction)) {
      return (NA)
    }
    if (cInd > 0) {
      return (cInd)
    } else {
      inF = priSimplePeak(induction)
      #print(induction)
      #print(inF)
      cInd <<- 1 / (((2 * pi * inF$freq) ^ 2) * (10 ^ -8))
      indRawPeak <<- inF
      return (cInd)
    }

  }
)

## Accessor getCap for variable cCap
Pickup$methods(
  getCap = function() {
    if (cCap > 0) {
      return (cCap)
    } else {
      ulF = priSimplePeak(unloaded)
      cCap <<-
        (1 / (((2 * pi * ulF$freq) ^ 2) * getInd())) * (10 ^ 12) - defaultProbeCap
      ulRawPeak <<- ulF
      return (cCap)
    }

  }
)

## Accessor getLdRawPeak
Pickup$methods(
  getLdRawPeak = function() {
    if (length(ldRawPeak) > 0) {
      return (ldRawPeak)
    } else {
      ldRawPeak <<- priSimplePeak(loaded)
      return (ldRawPeak)
    }

  }
)

## Accessor getULRawPeak
Pickup$methods(
  getULRawPeak = function() {
    if (length(ulRawPeak) > 0) {
      return (ulRawPeak)
    } else {
      getCap() #Yes bad design, needs refactoring
      return (ulRawPeak)
    }

  }
)
## Accessor getULRawPeak
Pickup$methods(
  getIndRawPeak = function() {
    if (length(indRawPeak) > 0) {
      return (indRawPeak)
    } else {
      getInd() #Yes bad design, needs refactoring
      return (indRawPeak)
    }

  }
)


##
# Data massaging to make uniqe columns when we merge and melt
#TODO just melt the data here so we can do a simple concatination

Pickup$methods(
  getLoadedPlotData = function(column) {
    lOut <- loaded[, (names(loaded) %in% c("Freq", column))]
    names(lOut) <- c("Freq", name)
    # print(head (lOut)) #debug
    return(lOut)
  }

)

Pickup$methods(
  getUnLoadedPlotData = function(column) {
    lOut <- unloaded[, (names(unloaded) %in% c("Freq", column))]
    names(lOut) <- c("Freq", name)
    # print(head (lOut)) #debug
    return(lOut)
  }

)

Pickup$methods(
  getLDCutoff = function() {
    cutoff = loaded[loaded$smRelMag < -3,]
    #print(cutoff)
    trimCutoff = cutoff[cutoff$Freq > getLdRawPeak()$freq,] #only want the peaks after the raw data spikes
    #print(trimCutoff)
    return(head(trimCutoff, n = 1)$Freq)
  }
)

Pickup$methods(
  getULCutoff = function() {
    cutoff = loaded[unloaded$smRelMag < -3,]
    #print(cutoff)
    trimCutoff = cutoff[cutoff$Freq > getULRawPeak()$freq,] #only want the peaks after the raw data spikes
    #print(trimCutoff)
    return(head(trimCutoff, n = 1)$Freq)
  }
)

Pickup$methods(
  getLDPeaks = function() {
    peakIndex <- (findPeaks(as.matrix(loaded$smIntMag)))
    #print(tail(loaded[peakIndex,], n=1 )) #debug
    return(loaded[peakIndex, ])
  }
)

Pickup$methods(
  getLDPeak = function(){
    offset=0
    if(length(LDOffset)>0) {
      offset=LDOffset
    }
    peaks = getLDPeaks()
    trimPeaks = peaks[peaks$Freq < getLdRawPeak()$freq, ] #only want the peaks before the raw data spikes
    return(head( tail(trimPeaks, n = 1+offset), n=1 ) ) # we want the last but offseth one
  }
)

Pickup$methods(
  getULPeaks = function() {
    peakIndex <- (findPeaks(as.matrix(unloaded$smIntMag)))
    return(unloaded[peakIndex, ])
  }
)

Pickup$methods(
  getULPeak = function() {
    offset=0
    if(length(ULOffset)>0) {
      offset=ULOffset
    }
    peaks = getULPeaks()
    trimPeaks = peaks[peaks$Freq < getULRawPeak()$freq, ] #only want the peaks before the raw data spikes
    return(head( tail(trimPeaks, n = 1+offset), n=1 ) )  # we want the last one
  }
)


###
# We can build in some ggplot defaults



Pickup$methods(
  getIntPlot = function(min= -70, max = -50, smoothing=defaultSmoothing) {
    myPlot = ggplot (data = unloaded) +
      scale_x_log10(minor_breaks = log10_minor_break()) +
      theme(
        panel.grid.major.x = element_line(size = 0.1),
        panel.grid.minor.x = element_line(size = 0.2)
      ) +

      geom_line(mapping = aes(x = Freq , y = IntMag)) +
      geom_smooth(mapping = aes(x = Freq , y = IntMag),
                  span = smoothing) +
      geom_line(data = loaded, mapping = aes(x = Freq , y = IntMag)) +
      geom_smooth(
        data = loaded,
        mapping = aes(x = Freq , y = IntMag) ,
        span = smoothing,
        colour = "red"
      ) +

      geom_vline(xintercept = getLDPeak()$Freq, colour = "red") +
      geom_vline(xintercept = getLDCutoff(), colour = "red") +
      geom_vline(xintercept = getULPeak()$Freq, colour = "blue") +
      geom_vline(xintercept = getULCutoff(), colour = "blue") +

      #Add "raw" data
      geom_vline(xintercept = getLdRawPeak()$freq,
                 colour = "red",
                 linetype = "dashed") +
      geom_vline(xintercept = getULRawPeak()$freq,
                 colour = "blue",
                 linetype = "dashed") +



      ylim(min, max) +
      ggtitle(paste(manuf, name), "Integrated") +
      xlab("Frequency /Hz") +
      ylab("Magnetude /dB (-20db/Decade)")
    return(myPlot)
  }
)

Pickup$methods(
  getRelPlot = function(min= -10, max = 5, smoothing=defaultSmoothing) {
    myPlot = ggplot (data = unloaded) +
      scale_x_log10(minor_breaks = log10_minor_break()) +
      theme(
        panel.grid.major.x = element_line(size = 0.1),
        panel.grid.minor.x = element_line(size = 0.2)
      ) +

      geom_line(mapping = aes(x = Freq , y = IntRelMag)) +
      geom_smooth(mapping = aes(x = Freq , y = IntRelMag),
                  span = smoothing) +
      geom_line(data = loaded, mapping = aes(x = Freq , y = IntRelMag)) +
      geom_smooth(
        data = loaded,
        mapping = aes(x = Freq , y = IntRelMag) ,
        span = smoothing,
        colour = "red"
      ) +

      geom_vline(xintercept = getLDPeak()$Freq, colour = "red") +
      geom_vline(xintercept = getLDCutoff(), colour = "red") +
      geom_vline(xintercept = getULPeak()$Freq, colour = "blue") +
      geom_vline(xintercept = getULCutoff(), colour = "blue") +

      #Add "raw" data
      geom_vline(xintercept = getLdRawPeak()$freq,
                 colour = "red",
                 linetype = "dashed") +
      geom_vline(xintercept = getULRawPeak()$freq,
                 colour = "blue",
                 linetype = "dashed") +



      ylim(min, max) +
      ggtitle(paste(manuf, name, "Integrated")) +
      xlab("Frequency /Hz") +
      ylab("Magnetude /dB (-20db/Decade)")
    return(myPlot)
  }
)

# getRawPlot
# plots unintegrated data for verification of peak detection
#
Pickup$methods(
  getRawPlot = function(min= -1, max = -1, smoothing=defaultSmoothing) {
    #print(manuf)
    myPlot = ggplot (data = unloaded) +
      scale_x_log10(minor_breaks = log10_minor_break()) +
      theme(
        panel.grid.major.x = element_line(size = 0.1),
        panel.grid.minor.x = element_line(size = 0.2)
      ) +

      geom_line(mapping = aes(x = Freq , y = Mag)) +
      geom_smooth(mapping = aes(x = Freq , y = Mag) , span = smoothing) +

      geom_line(data = induction, mapping = aes(x = Freq , y = Mag)) +
      geom_smooth(
        data = induction,
        mapping = aes(x = Freq , y = Mag) ,
        span = smoothing,
        colour = "green"
      ) +

      geom_line(data = loaded, mapping = aes(x = Freq , y = Mag)) +
      geom_smooth(
        data = loaded,
        mapping = aes(x = Freq , y = Mag) ,
        span = smoothing,
        colour = "red"
      ) +

      #Dashed data from integrated plot
      geom_vline(xintercept = getLDPeak()$Freq,
                 colour = "red",
                 linetype = "dashed") +
      geom_vline(xintercept = getLDCutoff(),
                 colour = "red",
                 linetype = "dashed") +
      geom_vline(xintercept = getULPeak()$Freq,
                 colour = "blue",
                 linetype = "dashed") +
      geom_vline(xintercept = getULCutoff(),
                 colour = "blue",
                 linetype = "dashed") +

      geom_vline(xintercept = getLdRawPeak()$freq, colour = "red") +
      geom_vline(xintercept = getULRawPeak()$freq, colour = "blue") +
      geom_vline(xintercept = getIndRawPeak()$freq, colour = "green") +



      #ylim(min, max) +
      ggtitle(paste(manuf, name, "Raw")) +
      xlab("Frequency /Hz") +
      ylab("Magnetude /dB")

    if((min!=-1) | (max!=-1)) {
      myPlot = myPlot + ylim(min, max)
    }
    return(myPlot)
  }
)
#End

Pickup$methods(
  printData = function() {
    print(paste(manuf, name))
    print(paste(
      "Calculated Inductance:",
      prettyNum(getInd(), format = "fg", digits = 3),
      "H"
    ))
    print(paste(
      "Calculated Capacitance:",
      prettyNum(getCap(), format = "fg", digits = 3),
      "pF"
    ))
    print(paste(
      "Loaded Cutoff:",
      prettyNum(getLDCutoff(), format = "fg", digits = 3),
      "Hz"
    ))
    print(paste(
      "Loaded Resonant Peak:",
      prettyNum(getLDPeak()$Freq, format = "fg", digits = 3),
      "Hz",
      prettyNum(
        getLDPeak()$smIntMag,
        format = "fg",
        digits = 3
      ),
      "dB"
    ))
    print(paste(
      "Raw Loaded Peak:",
      prettyNum(
        getLdRawPeak()$freq,
        format = "fg",
        digits = 3
      ),
      "Hz",
      prettyNum(getLdRawPeak()$mag, format = "fg", digits = 3),
      "dB"
    ))
    print(paste(
      "Unloaded Cutoff:",
      prettyNum(getULCutoff(), format = "fg", digits = 3),
      "Hz"
    ))
    print(paste(
      "Unloaded Resonant Peak:",
      prettyNum(getULPeak()$Freq, format = "fg", digits = 3),
      "Hz",
      prettyNum(
        getULPeak()$smIntMag,
        format = "fg",
        digits = 3
      ),
      "dB"
    ))
    print(paste(
      "Raw Unloaded Peak:",
      prettyNum(
        getULRawPeak()$freq,
        format = "fg",
        digits = 3
      ),
      "Hz",
      prettyNum(getULRawPeak()$mag, format = "fg", digits = 3),
      "dB"
    ))
    print(paste(
      "Raw Induction Peak:",
      prettyNum(
        getIndRawPeak()$freq,
        format = "fg",
        digits = 3
      ),
      "Hz",
      prettyNum(
        getIndRawPeak()$mag,
        format = "fg",
        digits = 3
      ),
      "dB"
    ))

  }
)

