# Standard column names
# 
# Every source uses different column names, i need to standardise.
# 
# "Freq"      : Frequency of sample in Hz
# "Mag"       : Magnetude of sample in dB
# "Phase"     : Hopefully self explanetory, ignored column
# "Quality"   : Ignored.
# "IntMag"    : Integrated Magnetude, used for samples with an inherrent 20dB/decade bias. 
# "NmIntMag"  : Normalised IntMag, either over 100 to 500 Hz range or at 440H
# "mVrms"     : raw millivolts, not used
#

#We can be really lazy and run the same function on all sources, it's not too expensive and it saves 
#trying to detect or specify 

stdNames <- function (bodeDataNames) {
  names.o<-bodeDataNames
  
  #Convert Velleman Names
  bodeDataNames[names.o == "Hz"]<-"Freq"
  bodeDataNames[names.o == "dBV"]<-"Mag"
  bodeDataNames[names.o == "deg"]<-"Phase"
  
  #Convert CGM names
  bodeDataNames[names.o == "Frequency..Hz."]<-"Freq"
  bodeDataNames[names.o == "Magnitude..dB."]<-"Mag"
  bodeDataNames[names.o == "Phase..deg."]<-"Phase" 
  
  return(bodeDataNames)
}