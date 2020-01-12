###
# Each pickup needs one of these to define a report. This should be as short as possible and 
# contain no logic beyond defining a piuckup and printing some data
#


##
# Import needed libraries
source('sysCompPickup.R')
source('pickupComparisonCharts.R')



##
# Load some pickup data. Expected are three .csv files from the SYSCOMP scope
# with the -LD.csv -UL.csv and -IT.csv suffixes

chartHeading = "Multi-pickup Example"

pickupList = c(
  SysCompPickup(
    name = "Humbucker 1", #a modern flat-response pickup
    manuf = "Test",
    mDCR = 8.00,
    tableBase = 'SampleData/Humbucker1/Humbucker1',
    LDOffset=0, # is a gain adjustment for arbitrary purposes
    ULOffset=0  # is a gain adjustment for arbitrary purposes
  ),
  SysCompPickup(
    name = "Humbucker 2", #a vintage pickup with strong eddy current loss
    manuf = "OtherBuilder",
    mDCR = 9.00,
    tableBase = 'SampleData/Humbucker2/P22-A2',
    LDOffset=0, # is a gain adjustment for arbitrary purposes
    ULOffset=0  # is a gain adjustment for arbitrary purposes
  ),
  SysCompPickup(
    name = "HB1-Slugs", #Slug coil of HB1
    manuf = "Test",
    mDCR = 4.00,
    tableBase = 'SampleData/Humbucker1/Humbucker1-Slug'
  ),
  SysCompPickup(
    name = "SingleCoil1", #Slightly hotter and darker than vintage.
    manuf = "Test",
    mDCR = 6.00,
    tableBase = 'SampleData/SingleCoil1/SC1'
  )
  
)

#Select plots from a single item
#q=pickupList[[1]]
#print (q$getRawPlot())

# Do this for individual pickups to check data is good before relying 
# on comparison charts and calculated readings.
#Generates many individual charts, code in comments may be unmaintained
#check HumbuckerExample for the latest
# for (p in pickupList) {
#   p$printData()
#   
#   # mostly for confirming peak measurements 
#   print (p$getRawPlot()) 
#   
#   #Shows response with -20db/decade filter compensating for driver coil
#   # arrangement, measurments made here shown in solid line, dotted lines are 
#   # raw peaks
#   print (p$getIntPlot(min=-80, max=-50)) 
#   
#   # shows frequency response normalised to 0dB for comparison of frequency 
#   # response with other pickups, cutoff frequency should be at -3dB on plot
#     print (p$getRelPlot()) 
#   
# }

#From pickup comparison charts
#Compare output of all pickups in list
print(loadedAbsPlot(
  pickupList,
  chartHeading = chartHeading,
  smoothing=0.05,#keep small, 0.05 often good
  min = -70,
  max = -50
))
# compare frequency responses, the is chart we really want
print(loadedRelPlot(
  pickupList,
  chartHeading = chartHeading,
  min = -4,
  smoothing=0.08,#keep small, 0.05 often good
  max = 4
))


