#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source('timeseries.R',local = TRUE)
source("diurnal.R",local = TRUE)
source("weekdays.R",local = TRUE)
source("boxplot.R",local = TRUE)

# print(str(data))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # call boxplot module server function
  callModule(boxPlot,"boxPlott1")

  # call weekday plot module server function
  callModule(weekdayPlot,"weekday")
  
  # call timeseries module server function
  callModule(timeSeriesPlot,"timeseries")
  
  # call diurnal plot module server function
  callModule(diurnalPlot,"diurnal")
  

})
