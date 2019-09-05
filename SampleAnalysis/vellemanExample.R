###
# Each pickup needs one of these to define a report. This should be as short as possible and 
# contain no logic beyond defining a piuckup and printing some data
#

##
# Import needed libraries
libfolder<-"."
source(paste(libfolder, 'vellemanPickup.R', sep="/"))



##
# Load some pickup data. Expected are three .csv files from the SYSCOMP scope
# with the -LD.csv -UL.csv and -IT.csv suffixes

#I'm using my own pickup as the default for this template.

aPickup<-VellemanPickup(name = "Test Pickup",
               manuf = "TestMaker",
               mDCR = 16.0, 
               tableBase='SampleData/vell-2pass.txt',
               unloadedIndex=1,
               loadedIndex=2,
               indIndex=1, # Blatant LIE
               smoothing=0.07
               )


print (aPickup$getRawPlot())
print (aPickup$getIntPlot(min=-120, max=-80))
print (aPickup$getRelPlot())

aPickup$printData()