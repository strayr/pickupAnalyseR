#velleman reader test
# Import needed libraries
libfolder<-"pickupAnalyseR"
source(paste(libfolder, 'vellemanPickup.R', sep="/"))

vellmanTables <- vellemanReader("GuitarAnalysisData/Velleman/dimarzio_super_dist.txt", index =0)
print (head(vellmanTables))