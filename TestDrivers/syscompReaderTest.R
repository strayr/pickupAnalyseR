#basePlotDriver.R

## Reads sample plots into R, prints some summary data

source('DataImport/syscompReader.R')
CGMPlot = syscompReader("SampleData/CGM-filter.csv")
print (head(CGMPlot))
      