library(shiny)
library(tidyverse)
library(rlang)

diurnalPlotUI <- function(id) {
  ns <- NS(id)
  
  list(
    fluidRow(
      column(3,dateRangeInput(ns("daterangediurnal"),"Date range:",start = "2020-02-13", end="2020-03-07", min = datadatemin,max = datadatemax)),
      column(3,selectInput(ns("sensorT5"),"Sensor 1",sensorlist)),
      column(3,selectInput(ns("attrT5"),"Sensor 1 Pollutant/Attribute",varnames)),
      column(3,selectInput(ns("attrdenomT5"),"Compare to Pollutant/Attribute",varnames))

    ),
    hr(),
    fluidRow(
      plotOutput(ns("diurnal1"),brush = ns("brush"))
    ),
    hr(),
    fluidRow(
      column(3,dateRangeInput(ns("daterangediurnal_2"),"Date range:",start = "2020-02-13", end="2020-03-07", min = datadatemin,max = datadatemax)),
      column(3,selectInput(ns("sensorT5_2"),"Sensor 2",sensorlist)),
      column(3,selectInput(ns("attrT5_2"),"Sensor 2 Pollutant/Attribute",varnames)),
      column(3,selectInput(ns("attrdenomT5_2"),"Compare to Pollutant/Attribute",varnames))
    ),
    fluidRow(
      plotOutput(ns("diurnal2"),brush = ns("brush"))
    )
  )
  
}

diurnalPlot <- function(input,output,session) {
  
  observeEvent(input$attrT5,{
    updateSelectInput(session,"attrdenomT5",choices = c("No Comparison",varnames[varnames!=input$attrT5]),selected = "No Comparison")
  })
  
  observeEvent(input$attrT5_2,{
    updateSelectInput(session,"attrdenomT5_2",choices = c("No Comparison",varnames[varnames!=input$attrT5_2]),selected = "No Comparison")
  })
  
  diurnaldata <- reactive({

    startdateT5 = as.POSIXct(input$daterangediurnal[1],tz='EST')
    enddateT5 = as.POSIXct(input$daterangediurnal[2],tz='EST')
    startdateT5_2 = as.POSIXct(input$daterangediurnal_2[1],tz='EST')
    enddateT5_2 = as.POSIXct(input$daterangediurnal_2[2],tz='EST')
    
    sensorSelected = lookupsensor[[input$sensorT5]]
    sensorSelected_2 = lookupsensor[[input$sensorT5_2]]
    
    attributeSelected = lookupattr[[input$attrT5]]
    attributeSelected_denom = lookupattr[[input$attrdenomT5]]
    cat(file=stderr(),"inputdenom:",attributeSelected_denom,"\n") #for debug

    attributeSelected_2 = lookupattr[[input$attrT5_2]]
    attributeSelected_2_denom = lookupattr[[input$attrdenomT5_2]]
    cat(file=stderr(),"inputdenom2:",attributeSelected_2_denom,"\n") #for debug
    
    if (attributeSelected_denom=="No Comparison"){
      #convert String to quosure
      attr_s1 = parse_quo(attributeSelected,env = caller_env())
      
      #data computation and subsetting
      data_diurnal = 
        data %>%
        drop_na(!! attr_s1) %>%
        mutate(targetVariable = !! attr_s1) %>%
        filter(timestamp_local>=startdateT5,timestamp_local<=enddateT5) %>%
        filter(sensorname == sensorSelected) %>%
        select(st_time_all_in_oneday,targetVariable)
      
    }else{
      #convert String to quosure
      attr_s1 = parse_quo(attributeSelected,env = caller_env())
      attr_s1_denom = parse_quo(attributeSelected_denom,env = caller_env())
      
      #data computation and subsetting
      data_diurnal = 
        data %>%
        drop_na(c(!! attr_s1, !! attr_s1_denom)) %>%
        mutate(targetVariable = ifelse(!!attr_s1_denom==0,NA,!!attr_s1/!!attr_s1_denom)) %>%
        filter(timestamp_local>=startdateT5,timestamp_local<=enddateT5) %>%
        filter(sensorname == sensorSelected) %>%
        select(st_time_all_in_oneday,targetVariable) 

    }
    
    if (attributeSelected_2_denom =="No Comparison"){
      #convert String to quosure
      attr_s2 = parse_quo(attributeSelected_2,env = caller_env())
      
      data_diurnal_2 = 
        data %>%
        drop_na(!!attr_s2) %>%
        mutate(targetVariable = !! attr_s2) %>%
        filter(timestamp_local>=startdateT5_2,timestamp_local<=enddateT5_2) %>%
        filter(sensorname == sensorSelected_2) %>%
        select(st_time_all_in_oneday,targetVariable) 
    }else{
      #convert String to quosure
      attr_s2 = parse_quo(attributeSelected_2,env = caller_env())
      attr_s2_denom = parse_quo(attributeSelected_2_denom,env = caller_env())
      
      data_diurnal_2 = 
        data %>%
        drop_na(c(!!attr_s2,!!attr_s2_denom)) %>%
        mutate(targetVariable = ifelse(!!attr_s2_denom==0,NA,!!attr_s2/!!attr_s2_denom)) %>%
        filter(timestamp_local>=startdateT5_2,timestamp_local<=enddateT5_2) %>%
        filter(sensorname == sensorSelected_2) %>%
        select(st_time_all_in_oneday,targetVariable) 

    }

    
    return(list("data"=data_diurnal,"attr"=attributeSelected,"sensor"=sensorSelected,"start"=startdateT5,"end"=enddateT5,"data_2"=data_diurnal_2,"attr_2"=attributeSelected_2,"sensor_2"=sensorSelected_2,"start_2"=startdateT5_2,"end_2"=enddateT5_2,"attrdenom"=attributeSelected_denom,"attrdenom_2"=attributeSelected_2_denom))
  })
  
  output$diurnal1 <- renderPlot({
    diurnal(diurnaldata()$data,diurnaldata()$attr,diurnaldata()$sensor,diurnaldata()$start,diurnaldata()$end,diurnaldata()$attrdenom)
  })
  
  output$diurnal2 <- renderPlot({
    diurnal(diurnaldata()$data_2,diurnaldata()$attr_2,diurnaldata()$sensor_2,diurnaldata()$start_2,diurnaldata()$end_2,diurnaldata()$attrdenom_2)
  })
  
}


diurnal <- function(data,attr,sensor,startdate,enddate,attrdenom) {
  ggplot(data,aes(st_time_all_in_oneday,targetVariable)) +
    geom_smooth(stat = 'summary', color = 'blue', fill = 'grey', alpha = 0.4, size = 0.2, 
                fun.data = median_hilow, fun.args = list(conf.int = 0.5))+
    stat_summary(fun = quantile,fun.args = list(probs = 0.25),color = "green",geom = "line", size = 0.2) +
    stat_summary(fun = quantile,fun.args = list(probs = 0.75),color = "orange",geom = "line", size = 0.2) +
    scale_x_datetime(date_labels = "%H",date_breaks = "1 hour", timezone = "EST") +
    # scale_y_continuous(limits = c(0,tab2data()$maxy)) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
      # axis.text = element_text(size = 48)
    ) +
    labs(title = paste(sensor,lookupattrname[[attr]],sep =" : "),subtitle = paste(startdate,enddate,sep = " to "),caption = paste("Comparison(ratio between): ",paste(attr,attrdenom,sep = ",")))
}