#Test-Driver for pickup
libfolder<-"pickupAnalyseR"
datafolder <- "GuitarAnalysisData/CGM/Bass"

source(paste(libfolder, 'sysCompPickup.R', sep="/"))
source(paste(libfolder, 'vellemanPickup.R', sep="/"))
source(paste(libfolder, 'pickupComparisonCharts.R', sep="/"))


datafolder <- "GuitarAnalysisData/CGM/Bass"

smoothing <- 0.07

chartHeading = "DiMarzio Super Family Pickups"

##
# Load some pickup data. Expected are three .csv files from the SYSCOMP scope
# with the -LD.csv -UL.csv and -IT.csv suffixes
pickupList = c(
  SysCompPickup(name = "Super 2",
         manuf = "Dimarzio",
         mDCR = 8.44,
         tableBase='GuitarAnalysisData/CGM/DiMarzio/Super2/Super2'),
  SysCompPickup(name = "EVO",
                manuf = "Dimarzio",
                mDCR = 8.44,
                tableBase='GuitarAnalysisData/CGM/DiMarzio/EVO/EVO'),
  SysCompPickup(name = "V2",
                manuf = "Ibanez",
                mDCR = 8.44,
                tableBase='GuitarAnalysisData/CGM/Ibanez/RocketRoll/V2'),
  
  VellemanPickup(name = "Super Distortion",
                 manuf = "Dimarzio",
                 mDCR = 16.0, 
                 tableBase='GuitarAnalysisData/Velleman/dimarzio_super_dist-driver.txt',
                 unloadedIndex=1,
                 loadedIndex=2,
                 indIndex=1, # Blatant LIE
                 smoothing=0.07
  )

)



for (p in pickupList) {
  p$printData()
}



print(loadedAbsPlot(
  pickupList,
  chartHeading = chartHeading,
  min = -90,
  max = -55
))
print(loadedRelPlot(
  pickupList,
  chartHeading = chartHeading,
   min = -10,
   max = 5
))

print(unLoadedPlot(
  pickupList,
  chartHeading = chartHeading,
  min = -90,
  max = -50
))

