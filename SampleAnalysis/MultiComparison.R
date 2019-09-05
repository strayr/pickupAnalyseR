#Test-Driver for pickup
libfolder<-"."

source(paste(libfolder, 'sysCompPickup.R', sep="/"))
source(paste(libfolder, 'vellemanPickup.R', sep="/"))
source(paste(libfolder, 'pickupComparisonCharts.R', sep="/"))


datafolder <- "SampleData"

smoothing <- 0.07

chartHeading = "Test Pickups"

##
# Load some pickup data. Expected are three .csv files from the SYSCOMP scope
# with the -LD.csv -UL.csv and -IT.csv suffixes
pickupList = c(
  SysCompPickup(name = "Test 1",
         manuf = "testmaker",
         mDCR = 8.44,
         tableBase='SampleData/P22-A2'),
  SysCompPickup(name = "Test 2",
                manuf = "testnmaker",
                mDCR = 8.44,
                tableBase='SampleData/P22-A2'),
  SysCompPickup(name = "Test 3",
                manuf = "rival testmaker",
                mDCR = 8.44,
                tableBase='SampleData/P22-A2'),
  
  VellemanPickup(name = "velleman test",
                 manuf = "testmaker",
                 mDCR = 16.0, 
                 tableBase='SampleData/vell-2pass.txt',
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

