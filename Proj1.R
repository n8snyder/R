load(url("http://www.stat.berkeley.edu/users/nolan/data/weather2011.rda"))
makePlotRegion = function(xlim, ylim, bgcolor, ylabels,
                          margins, xtop = TRUE){
  par(bg = bgcolor, adj=0, mar=margins)
  plot(1:2, 1:2, type ="n", xlim = xlim, ylim = ylim, 
       xaxt="n", ylab='', xlab="", yaxt="n", bty="n")
  axis(2, las=2, at=ylabels, col.ticks=bgcolor, pos=xlim[1])
  axis(4, las=2, at=ylabels, col.ticks=bgcolor, pos=xlim[2])
  if(xtop){
    for(i in 1:12){
     mtext(at = (cumDays[i] + cumDays[i+1])/2, text = monthNames[i], side = 3)
     }
   }else{
     for(i in 1:12){
       mtext(at = (cumDays[i] + cumDays[i+1])/2, text = monthNames[i], side = 1)
     }
   }
}


drawTempRegion = function(day, high, low, col){
  # This plot will produce 365 rectangles, one for each day
  # It will be used for the record temps, normal temps, and 
  # observed temps
  
  # day - a numeric vector of 365 dates
  # high - a numeric vector of 365 high temperatures
  # low - a numeric vector of 365 low temperatures
  # col - color to fill the rectangles
  rect(day-.5, low, day+.5, high, col = col, border = col)
  
}

addGrid = function(location, col, ltype, vertical = TRUE) {
  # This function adds a set of parallel grid lines
  # It will be used to place vertical and horizontal lines
  # on both temp and precip plots
  
  # location is a numeric vector of locations for the lines
  # col - the color to make the lines
  # ltype - the type of line to make
  # vertical - indicates whether the lines are vertical or horizontal
  
  if(vertical){
    for(i in location){
      abline(v = i, col = col, lty = ltype)
      }
    }else{
      for(i in location){
        abline(h = i, col = col, lty = ltype)
        }
    }
}

monthPrecip = function(day, dailyprecip, normal){
  # This function adds one month's precipitation to the 
  #   precipitation plot.
  # It will be called 12 times, once for each month
  # It creates the cumulative precipitation curve,
  # fills the area below with color, add the total
  # precipitation for the month, and adds a reference
  # line and text for the normal value for the month
  
  # day a numeric vector of dates for the month
  # dailyprecip a numeric vector of precipitation recorded
  # for the month (any NAs can be set to 0)
  # normal a single value, which is the normal total precip
  #  for the month
  polygon(c(day,rev(day)), c(cumsum(dailyprecip),rep(0,length(dailyprecip))), col = "#E5D8BD")
  lines(day, cumsum(dailyprecip), col = "#174c64", lwd = 4)
  lines(day, rep(normal,length(day)), col = "#8ca8ac", lwd = 3)
  text(day[1],normal+.2, as.character(normal), cex = 1)
  text(day[length(day)], max(cumsum(dailyprecip))+.2, as.character(max(cumsum(dailyprecip))), 
       cex = 1, pos = 1)
}
monthNames = c("January", "February", "March", "April",
               "May", "June", "July", "August", "September",
               "October", "November", "December")
daysInMonth = c(31, 28, 31, 30, 31, 30, 31, 
                31, 30, 31, 30, 31)
cumDays = cumsum(c(0, daysInMonth))

finalPlot = function(temp, precip){
  # The purpose of this function is to create the whole plot
  # Include here all of the set up that you need for
  # calling each of the above functions.
  # temp is the data frame sfoWeather or laxWeather
  # precip is the data frame sfoMonthlyPrecip or laxMonthlyPrecip

  
  # Here are some vectors that you might find handy

  normPrecip = as.numeric(as.character(precip$normal))
  ### Fill in the various stages with your code
 

  ### Add any additional variables that you will need here
  precipPrecip = as.numeric(as.character(precip$precip))
  recordedPrecip = temp$Precip
  recordedPrecip[is.na(recordedPrecip)]=0
  tiedHighs = temp$High[which(temp$RecordHigh == temp$High)]
  tiedHighsDays = which(temp$RecordHigh == temp$High)
  tiedLows = temp$Low[which(temp$RecordLow == temp$Low)]
  tiedLowsDays = which(temp$RecordLow == temp$Low)
  ### Set up the graphics device to plot to pdf and layout
  ### the two plots on one canvas
  ### pdf("", width = , height = )
  ### layout(  )
  pdf("Weather Graph", width = 13, height = 7)
  layout(c(1,1,1,1,2,2))
  ### Call makePlotRegion to create the plotting region
  ### for the temperature plot
  makePlotRegion(c(1,365), c(floor(min(temp$RecordLow)/10)*10,ceiling(max(temp$RecordHigh/10))*10), "#EBEBE0", 
                 (floor(min(temp$RecordLow)/10):ceiling(max(temp$RecordHigh/10)))*10, c(5,2,5,2))
  ### Call drawTempRegion 3 times to add the rectangles for
  ### the record, normal, and observed temps
  drawTempRegion(1:365, temp$RecordHigh, temp$RecordLow, "#E5D8BD")
  drawTempRegion(1:365, temp$NormalHigh, temp$NormalLow, "#99907e")
  drawTempRegion(1:365, temp$High, temp$Low, "#6F1A49")
  ### Call addGrid to add the grid lines to the plot
  addGrid(c(31,59,90,120,151,181,212,243,273,304,334), "black", 3)
  addGrid((floor(min(temp$RecordLow)/10):ceiling(max(temp$RecordHigh/10)))*10, "#EBEBE0", 1, F)
  ### Add the markers for the record breaking days
  text(tiedHighsDays[which(tiedHighs == max(tiedHighs))] + 3, 
       max(tiedHighs) + 10, 
       labels = paste("TIED RECORD HIGH:", max(tiedHighs)))
  segments(tiedHighsDays[which(tiedHighs == max(tiedHighs))],
           max(tiedHighs),
           y1 = max(tiedHighs) + 10)
  
  text(tiedLowsDays[which(tiedLows == min(tiedLows))]+3, 
       min(tiedLows) - 10, 
       labels = paste("TIED RECORD LOW:", min(tiedLows)))
  segments(tiedLowsDays[which(tiedLows == min(tiedLows))], 
           min(tiedLows),
           y1 = min(tiedLows) - 10)
  ### Add the titles 
  title(main = list("San Francisco's Weather in 2011", cex = 1.8))
  text(8, ceiling(max(temp$RecordHigh/10))*10, labels = "Temperature", cex = 1.5, font = 2)
  text(8, ceiling(max(temp$RecordHigh/10))*10 - 5, 
       labels = "Bars represent range between", cex = 1.5)
  text(8, ceiling(max(temp$RecordHigh/10))*10 - 9, 
       labels = "the daily high and low.", cex = 1.5)
  
  segments(190, floor(min(temp$RecordLow)/10)*10+3, y1 = floor(min(temp$RecordLow)/10)*10+18,
           col = "#E5D8BD", lwd = 7)
  text(188, floor(min(temp$RecordLow)/10)*10+3, labels = "RECORD LOW", adj = 1)
  text(188, floor(min(temp$RecordLow)/10)*10+18, labels = "RECORD HIGH", adj = 1)
  segments(190, floor(min(temp$RecordLow)/10)*10+8, y1 = floor(min(temp$RecordLow)/10)*10+13,
           col = "#99907e", lwd = 7)
  segments(c(190, 187, 187), floor(min(temp$RecordLow)/10)*10 + c(13,13,8),
           x1 = c(187,187,190), y1 = floor(min(temp$RecordLow)/10)*10+ c(13,8,8))
  
  segments(190, floor(min(temp$RecordLow)/10)*10+10, y1 = floor(min(temp$RecordLow)/10)*10+15,
           col = "#6F1A49", lwd = 4)
  text(186, floor(min(temp$RecordLow)/10)*10+10, labels="NORMAL RANGE", adj = 1)
  segments(190, floor(min(temp$RecordLow)/10)*10+10, x1 = 194)
  text(195, floor(min(temp$RecordLow)/10)*10+10, labels = "ACTUAL LOW")
  segments(190, floor(min(temp$RecordLow)/10)*10+15, x1 = 194)
  text(195, floor(min(temp$RecordLow)/10)*10+15, labels = "ACTUAL HIGH")
  ### Call makePlotRegion to create the plotting region
  ### for the precipitation plot
  makePlotRegion(c(1,365), c(0,ceiling(max(precipPrecip))), "#EBEBE0", c(0:ceiling(max(precipPrecip))),
                 c(5,2,5,2), F)
  ### Call monthPrecip 12 times to create each months 
  ### cumulative precipitation plot. To do this use 
  ### sapply(1:12, function(m) {
  ###             code
  ###             monthPrecip(XXXX)
  ###             }) 
  ### the anonymous function calls monthPrecip with the 
  ### appropriate arguments
  for(i in 1:12){
    monthPrecip(cumDays[i]:(cumDays[i+1]-1) + 1, recordedPrecip[cumDays[i]:(cumDays[i+1]-1) + 1], normPrecip[i])
  }
  ### Call addGrid to add the grid lines to the plot
  addGrid(c(31,59,90,120,151,181,212,243,273,304,334), "black", 3)
  addGrid(0:5, "#EBEBE0", 1, F)
  ### Add the titles
  title(main = list("Precipitation", cex = 1.8))
  mtext(paste("Cumulative monthly precipitation in inches compared with normal monthly precipitation. Total precipitation in 2011 was", sum(recordedPrecip), "inches."),
        side = 3, line = 2)
  ### Close the pdf device dev.off()
  dev.off()
}

### Call: finalPlot(temp = sfoWeather, precip = sfoMonthlyPrecip)
finalPlot(temp = sfoWeather, precip = sfoMonthlyPrecip)
