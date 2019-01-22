# plotData.R
# a class to handle some bodePlot data

PlotData <- setRefClass("PlotData",
                        fields = list(table = "data.frame"))