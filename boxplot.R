library(shiny)
library(tidyverse)
library(rlang)

boxPlotUI <- function(id) {
  ns <- NS(id)
  
  list(
    fluidRow(
      column(3,dateRangeInput(ns("daterangebox"),"Date range:",start = "2020-02-13",end = "2020-03-07",min = datadatemin,max = datadatemax)),
      column(3,selectInput(ns("airattrT1"),"Pollutant/Attribute",varnames))
    ),
    hr(),
    fluidRow(
      plotOutput(ns("boxplot1"))
    ),
    hr(),
    fluidRow(
      column(3,dateRangeInput(ns("daterangebox_2"),"Date range:",start = "2020-02-13",end = "2020-03-07",min = datadatemin,max = datadatemax)),
      column(3,selectInput(ns("airattrT1_2"),"Pollutant/Attribute",varnames))
    ),
    fluidRow(
      plotOutput(ns("boxplot2"))
    )
  )
  
}

boxPlot <- function(input,output,session) {
  
  boxplotdata <- reactive({
    startdateT1 = as.POSIXct(input$daterangebox[1],tz='EST')
    enddateT1 = as.POSIXct(input$daterangebox[2],tz='EST')
    startdateT1_2 = as.POSIXct(input$daterangebox_2[1],tz='EST')
    enddateT1_2 = as.POSIXct(input$daterangebox_2[2],tz='EST')
    cat(file=stderr(),"date read \n")
    
    attributeSelected = lookupattr[[input$airattrT1]]
    attributeSelected_2 = lookupattr[[input$airattrT1_2]]
    cat(file=stderr(),"attr read \n")
    
    data_boxplot1 = 
      data %>% 
        filter(timestamp_local>=startdateT1,timestamp_local<=enddateT1) %>% 
        select(timestamp_local,attributeSelected,sensorname)
    
    data_boxplot2 = 
      data %>% 
      filter(timestamp_local>=startdateT1_2,timestamp_local<=enddateT1_2) %>% 
      select(timestamp_local,attributeSelected_2,sensorname)
 
    
    return(list("data"=data_boxplot1,"attr"=attributeSelected,"start"=startdateT1,"end"=enddateT1,"data_2"=data_boxplot2,"attr_2"=attributeSelected_2,"start_2"=startdateT1_2,"end_2"=enddateT1_2))
  })
  
  
  
  output$boxplot1 <- renderPlot({
    box(boxplotdata()$data,boxplotdata()$attr,boxplotdata()$start,boxplotdata()$end)
  })
  
  output$boxplot2 <- renderPlot({
    box(boxplotdata()$data_2,boxplotdata()$attr_2,boxplotdata()$start_2,boxplotdata()$end_2)
  })
  
}

box <- function(data,attr,startdate,enddate) {
  ggplot(data,aes_string("sensorname",attr)) +
    geom_boxplot(aes(fill=sensorname),outlier.size = 1) +
    theme_bw() + theme(axis.title.x = element_blank()) +
    labs(y= lookupattrname[[attr]],title = lookupattrname[[attr]],fill = "Sensor") +
    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)))
}
