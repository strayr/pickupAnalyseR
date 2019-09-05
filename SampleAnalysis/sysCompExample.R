###
# Each pickup needs one of these to define a report. This should be as short as possible and 
# contain no logic beyond defining a piuckup and printing some data
#
 
datafolder<-"SampleData"

##
# Import needed libraries

libfolder<-"."
source(paste(libfolder, 'sysCompPickup.R', sep="/"))
#source(sysCompPickup.R)



##
# Load some pickup data. Expected are three .csv files from the SYSCOMP scope
# with the -LD.csv -UL.csv and -IT.csv suffixes

#I'm using my own pickup as the default for this template.

aPickup=SysCompPickup(name = "P22",
               manuf = "Straylight",
               mDCR = 8.08,
               tableBase=paste(datafolder,'P22-A2', sep="/")
               )

print (aPickup$getRawPlot())
print (aPickup$getIntPlot())
print (aPickup$getRelPlot())

aPickup$printData()