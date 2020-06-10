#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

source("timeseries.R",local = TRUE)
source("diurnal.R",local = TRUE)
source("weekdays.R",local = TRUE)
source("boxplot.R",local = TRUE)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  title = "Riseboro Air Sensing Visualization",
  
  # Application title
  titlePanel("Visualization: Riseboro Air Sensing Data"),
  
  #tab panel to switch between visualizations
  tabsetPanel(
    tabPanel("All Sensors",
      boxPlotUI("boxPlott1")
    ),
    tabPanel("Weekdays Plot",
      weekdayPlotUI("weekday")
    ),
    tabPanel("Time Series",
      timeSeriesPlotUI("timeseries")
    ),
    tabPanel("Diurnal Plot",
      diurnalPlotUI("diurnal")       
    )

  )
  
))
