#pickupComparisonCharts.R
# 
# A collection of comparison charts that operate on a LIST of Pickup (or subclass) objects
#

library(purrr)

# Returns an integrated plot of pickups tested under standard load.
# Use for comparasons of audible frequency response
loadedRelPlot = function(pickupList,chartHeading="Unnamed", min= -70, max = 5, smoothing=0.05) {
  meltedLoad=melt(map(pickupList, ~ .x$getLoadedPlotData("IntRelMag") ), id.vars="Freq", variable.name="pickupName")
  myPlot = ggplot(meltedLoad, aes(Freq,value)) +
    geom_smooth(mapping = aes(colour=pickupName) , span = smoothing) +
    #geom_line(mapping = aes(colour=pickupName)) +
    scale_x_log10(minor_breaks = log10_minor_break()) +
    theme(
      panel.grid.major.x = element_line(size = 0.1),
      panel.grid.minor.x = element_line(size = 0.2)
    ) +
    ylim(min, max) +
    labs(title=paste(chartHeading,"- loaded 200k 470pf"), x="Frequency /Hz", y="Relative Magnetude (500-1000Hz) /dB (-20db/Decade)", color="Pickup")
  return(myPlot)
  
}


# Returns an integrated plot of pickups tested under standard load.
# Use for comparasons of pickup OUTPUT
loadedAbsPlot = function(pickupList,chartHeading="Unnamed", min= -70, max = -50, smoothing=0.07) {
  meltedLoad=melt(map(pickupList, ~ .x$getLoadedPlotData("IntMag") ), id.vars="Freq", variable.name="pickupName")
  myPlot = ggplot(meltedLoad, aes(Freq,value)) +
    geom_smooth(mapping = aes(colour=pickupName) , span = smoothing) +
    scale_x_log10(minor_breaks = log10_minor_break()) +
    theme(
      panel.grid.major.x = element_line(size = 0.1),
      panel.grid.minor.x = element_line(size = 0.2)
    ) +
    ylim(min, max) +
    labs(title=paste(chartHeading,"- loaded 200k 470pf"), x="Frequency /Hz", y="Magnetude /dB (-20db/Decade)", color="Pickup")
  return(myPlot)
  
}



# Returns an integrated plot of pickups tested without load
# Not really sure its useful
unLoadedPlot = function(pickupList,chartHeading="Unnamed", min= -70, max = -50, smoothing=0.07) {
  meltedUL=melt(map(pickupList, ~ .x$getUnLoadedPlotData("IntMag") ), id.vars="Freq", variable.name="pickupName")
  ULPlot = ggplot(meltedUL, aes(Freq,value)) +
    geom_smooth(mapping = aes(colour=pickupName) , span = smoothing) +
    scale_x_log10(minor_breaks = log10_minor_break()) +
    theme(
      panel.grid.major.x = element_line(size = 0.1),
      panel.grid.minor.x = element_line(size = 0.2)
    ) +
    ylim(min, max) +
    labs(title=paste(chartHeading,"- unloaded, absolute"), x="Frequency /Hz", y="Magnetude /dB (-20db/Decade)", color="Pickup")
  return(ULPlot)
}


