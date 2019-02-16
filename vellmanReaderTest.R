#velleman reader test
# Import needed libraries
libfolder<-"pickupAnalyseR"
source(paste(libfolder, 'vellemanReader.R', sep="/"))

vellmanTables1 <- vellemanReader("GuitarAnalysisData/Velleman/dimarzio_super_dist.txt")
print (head(vellmanTables1))

vellmanTables2 <- vellemanReader("GuitarAnalysisData/Velleman/dimarzio_super_dist-driver.txt")
print (head(vellmanTables2))