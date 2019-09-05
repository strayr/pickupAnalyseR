#velleman reader test
# Import needed libraries
libfolder<-"."
source(paste(libfolder, 'DataImport/vellemanReader.R', sep="/"))

vellmanTables1 <- vellemanReader("SampleData/vell-2pass.txt")
print (head(vellmanTables1))
vellmanTables2 <- vellemanReader("SampleData/vell-2pass.txt", index=2)
print (head(vellmanTables2))

#vellmanTables2 <- vellemanReader("GuitarAnalysisData/Velleman/dimarzio_super_dist-driver.txt")
#print (head(vellmanTables2))