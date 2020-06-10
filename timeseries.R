# library(tidyverse) #no need as libraries are imported in global.R
library(shiny)
library(tidyverse)
library(rlang)

timeSeriesPlotUI <- function(id) {
  ns <- NS(id)
  
  list(
    fluidRow(
      column(3,dateRangeInput(ns("daterange4"),"Date range:",start = "2020-02-13", end="2020-03-07", min = datadatemin,max = datadatemax)),
      column(3,selectInput(ns("sensorT4"),"Sensor 1",sensorlist)),
      column(3,selectInput(ns("attrT4"),"Sensor 1 Pollutant/Attribute",varnames)),
      column(3,selectInput(ns("attrdenomT4"),"Compare to Pollutant/Attribute",varnames))
      
    ),
    hr(),
    fluidRow(
      plotOutput(ns("timeseries1"),brush = ns("brush"))
    ),
    hr(),
    fluidRow(
      column(3,dateRangeInput(ns("daterange4_2"),"Date range:",start = "2020-02-13", end="2020-03-07", min = datadatemin,max = datadatemax)),
      column(3,selectInput(ns("sensorT4_2"),"Sensor 2",sensorlist)),
      column(3,selectInput(ns("attrT4_2"),"Sensor 2 Pollutant/Attribute",varnames)),
      column(3,selectInput(ns("attrdenomT4_2"),"Compare to Pollutant/Attribute",varnames))
      
    ),
    fluidRow(
      plotOutput(ns("timeseries2"),brush = ns("brush"))
    )
  )

}

timeSeriesPlot <- function(input,output,session){
  # colnames(data)
  observeEvent(input$attrT4,{
    updateSelectInput(session,"attrdenomT4",choices = c("No Comparison",varnames[varnames!=input$attrT4]),selected = "No Comparison")
  })
  
  observeEvent(input$attrT4_2,{
    updateSelectInput(session,"attrdenomT4_2",choices = c("No Comparison",varnames[varnames!=input$attrT4_2]),selected = "No Comparison")
  })
  
  timeseriesdata <- reactive({
    
    # cat(stderr(),"class:",class(input$sensorT4),"\n")
    startdateT4 = as.POSIXct(input$daterange4[1],tz='EST')
    enddateT4 = as.POSIXct(input$daterange4[2],tz='EST')
    

    sensorSelected = lookupsensor[[input$sensorT4]]

    attributeSelected = lookupattr[[input$attrT4]]
    attributeSelected_denom = lookupattr[[input$attrdenomT4]]
    cat(file=stderr(),"inputdenom:",attributeSelected_denom,"\n") #for debug
    
    if(attributeSelected_denom=="No Comparison"){
      #convert String to quosure
      attr_s1 = parse_quo(attributeSelected,env = caller_env())      
      
      #data computation and subsetting
      data_timeseries = 
        data %>%
        drop_na(!! attr_s1) %>%
        mutate(targetVariable = !! attr_s1) %>%
        filter(timestamp_local>=startdateT4,timestamp_local<=enddateT4) %>%
        filter(sensorname == sensorSelected) %>%
        select(timestamp_local,targetVariable)
      
    }else{
      #convert String to quosure
      attr_s1 = parse_quo(attributeSelected,env = caller_env())
      attr_s1_denom = parse_quo(attributeSelected_denom,env = caller_env())
      
      #data computation and subsetting
      data_timeseries = 
        data %>%
        drop_na(c(!! attr_s1, !! attr_s1_denom)) %>%
        mutate(targetVariable = ifelse(!!attr_s1_denom==0,NA,!!attr_s1/!!attr_s1_denom)) %>%
        filter(timestamp_local>=startdateT4,timestamp_local<=enddateT4) %>%
        filter(sensorname == sensorSelected) %>%
        select(timestamp_local,targetVariable) 
    }

    
    
    return(list("data"=data_timeseries,"attr"=attributeSelected,"sensor"=sensorSelected,"start"=startdateT4,"end"=enddateT4,"attrdenom"=attributeSelected_denom))
  })
  
  timeseriesdata_2 <- reactive({
    
    startdateT4_2 = as.POSIXct(input$daterange4_2[1],tz='EST')
    enddateT4_2 = as.POSIXct(input$daterange4_2[2],tz='EST')
    
    sensorSelected_2 = lookupsensor[[input$sensorT4_2]]
    
    attributeSelected_2 = lookupattr[[input$attrT4_2]]
    attributeSelected_2_denom = lookupattr[[input$attrdenomT4_2]]
    cat(file=stderr(),"inputdenom2:",attributeSelected_2_denom,"\n") #for debug
    
    if (attributeSelected_2_denom =="No Comparison"){
      #convert String to quosure
      attr_s2 = parse_quo(attributeSelected_2,env = caller_env())
      
      data_timeseries_2 = 
        data %>%
        drop_na(!!attr_s2) %>%
        mutate(targetVariable = !! attr_s2) %>%
        filter(timestamp_local>=startdateT4_2,timestamp_local<=enddateT4_2) %>%
        filter(sensorname == sensorSelected_2) %>%
        select(timestamp_local,targetVariable) 
    }else{
      #convert String to quosure
      attr_s2 = parse_quo(attributeSelected_2,env = caller_env())
      attr_s2_denom = parse_quo(attributeSelected_2_denom,env = caller_env())
      
      data_timeseries_2 = 
        data %>%
        drop_na(c(!!attr_s2,!!attr_s2_denom)) %>%
        mutate(targetVariable = ifelse(!!attr_s2_denom==0,NA,!!attr_s2/!!attr_s2_denom)) %>%
        filter(timestamp_local>=startdateT4_2,timestamp_local<=enddateT4_2) %>%
        filter(sensorname == sensorSelected_2) %>%
        select(timestamp_local,targetVariable) 
      
    }
    
    # data_timeseries_2 = 
    #   data %>%
    #   filter(timestamp_local>=startdateT4_2,timestamp_local<=enddateT4_2) %>%
    #   filter(sensorname == sensorSelected_2) %>%
    #   select(timestamp_local,attributeSelected_2)
    
    return(list("start_2"=startdateT4_2,"end_2"=enddateT4_2,"data_2"=data_timeseries_2,"attr_2"=attributeSelected_2,"sensor_2"=sensorSelected_2,"attrdenom_2"=attributeSelected_2_denom))
  })

  output$timeseries1 <- renderPlot({
    timeplot(timeseriesdata()$data,timeseriesdata()$attr,timeseriesdata()$sensor,timeseriesdata()$start,timeseriesdata()$end,timeseriesdata()$attrdenom)
  })
  
  output$timeseries2 <- renderPlot({
    timeplot(timeseriesdata_2()$data_2,timeseriesdata_2()$attr_2,timeseriesdata_2()$sensor_2,timeseriesdata_2()$start_2,timeseriesdata_2()$end_2,timeseriesdata_2()$attrdenom_2)
  })
    # output$timeseries1 <- renderPlot({
    #   timeplot(data,attr,sensor,startdateT4,enddateT4)
    # })
    # 
  # cat(file = stderr(),"finished plotting","\n")
}

timeplot <- function(data,attr,sensor,startdate,enddate,attrdenom){
  ggplot(data,aes(timestamp_local,targetVariable)) +
    geom_line(color = "orange") + 
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
      # axis.text = element_text(size = 48)
    ) + 
    labs(y = "Concentration",title = paste(sensor,lookupattrname[[attr]],sep =" : "),subtitle = paste(startdate,enddate,sep = " to "),caption = paste("Comparison(ratio between): ",paste(attr,attrdenom,sep = ",")))
  # p
}
