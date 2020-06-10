library(shiny)
library(tidyverse)
library(rlang)

weekdayPlotUI <- function(id) {
  ns <- NS(id)
  
  list(
    fluidRow(
      column(3,dateRangeInput(ns("daterangeweekday"),"Date range:",start = "2020-02-13", end="2020-03-07", min = datadatemin,max = datadatemax)),
      column(3,selectInput(ns("sensorT3"),"Sensor 1",sensorlist)),
      column(3,selectInput(ns("attrT3"),"Sensor 1 Pollutant/Attribute",varnames)),
      column(3,selectInput(ns("attrdenomT3"),"Compare to Pollutant/Attribute",varnames))
      
    ),
    hr(),
    fluidRow(
      plotOutput(ns("weekday1"),brush = ns("brush"))
    ),
    hr(),
    fluidRow(
      column(3,dateRangeInput(ns("daterangeweekday_2"),"Date range:",start = "2020-02-13", end="2020-03-07", min = datadatemin,max = datadatemax)),
      column(3,selectInput(ns("sensorT3_2"),"Sensor 2",sensorlist)),
      column(3,selectInput(ns("attrT3_2"),"Sensor 2 Pollutant/Attribute",varnames)),
      column(3,selectInput(ns("attrdenomT3_2"),"Compare to Pollutant/Attribute",varnames))
    ),
    fluidRow(
      plotOutput(ns("weekday2"),brush = ns("brush"))
    )
  )
  
}

weekdayPlot <- function(input,output,session) {
  
  observeEvent(input$attrT3,{
    updateSelectInput(session,"attrdenomT3",choices = c("No Comparison",varnames[varnames!=input$attrT3]),selected = "No Comparison")
  })
  
  observeEvent(input$attrT3_2,{
    updateSelectInput(session,"attrdenomT3_2",choices = c("No Comparison",varnames[varnames!=input$attrT3_2]),selected = "No Comparison")
  })
  
  weekdaydata <- reactive({
    
    startdateT3 = as.POSIXct(input$daterangeweekday[1],tz='EST')
    enddateT3 = as.POSIXct(input$daterangeweekday[2],tz='EST')
    startdateT3_2 = as.POSIXct(input$daterangeweekday_2[1],tz='EST')
    enddateT3_2 = as.POSIXct(input$daterangeweekday_2[2],tz='EST')
    
    sensorSelected = lookupsensor[[input$sensorT3]]
    sensorSelected_2 = lookupsensor[[input$sensorT3_2]]

    attributeSelected = lookupattr[[input$attrT3]]
    attributeSelected_denom = lookupattr[[input$attrdenomT3]]
    cat(file=stderr(),"inputdenom:",attributeSelected_denom,"\n") #for debug
    
    attributeSelected_2 = lookupattr[[input$attrT3_2]]
    attributeSelected_2_denom = lookupattr[[input$attrdenomT3_2]]
    cat(file=stderr(),"inputdenom2:",attributeSelected_2_denom,"\n") #for debug
    
    if (attributeSelected_denom=="No Comparison"){
      #convert String to quosure
      attr_s1 = parse_quo(attributeSelected,env = caller_env())
      
      #data computation and subsetting
      data_weekday = 
        data %>%
        drop_na(!! attr_s1) %>%
        mutate(targetVariable = !! attr_s1) %>%
        filter(timestamp_local>=startdateT3,timestamp_local<=enddateT3) %>%
        filter(sensorname == sensorSelected) %>%
        select(st_time_all_in_oneweek,targetVariable)
      
    }else{
      #convert String to quosure
      attr_s1 = parse_quo(attributeSelected,env = caller_env())
      attr_s1_denom = parse_quo(attributeSelected_denom,env = caller_env())
      
      #data computation and subsetting
      data_weekday = 
        data %>%
        drop_na(c(!! attr_s1, !! attr_s1_denom)) %>%
        mutate(targetVariable = ifelse(!!attr_s1_denom==0,NA,!!attr_s1/!!attr_s1_denom)) %>%
        filter(timestamp_local>=startdateT3,timestamp_local<=enddateT3) %>%
        filter(sensorname == sensorSelected) %>%
        select(st_time_all_in_oneweek,targetVariable) 
      
    }
    
    if (attributeSelected_2_denom =="No Comparison"){
      #convert String to quosure
      attr_s2 = parse_quo(attributeSelected_2,env = caller_env())
      
      data_weekday_2 = 
        data %>%
        drop_na(!!attr_s2) %>%
        mutate(targetVariable = !! attr_s2) %>%
        filter(timestamp_local>=startdateT3_2,timestamp_local<=enddateT3_2) %>%
        filter(sensorname == sensorSelected_2) %>%
        select(st_time_all_in_oneweek,targetVariable) 
    }else{
      #convert String to quosure
      attr_s2 = parse_quo(attributeSelected_2,env = caller_env())
      attr_s2_denom = parse_quo(attributeSelected_2_denom,env = caller_env())
      
      data_weekday_2 = 
        data %>%
        drop_na(c(!!attr_s2,!!attr_s2_denom)) %>%
        mutate(targetVariable = ifelse(!!attr_s2_denom==0,NA,!!attr_s2/!!attr_s2_denom)) %>%
        filter(timestamp_local>=startdateT3_2,timestamp_local<=enddateT3_2) %>%
        filter(sensorname == sensorSelected_2) %>%
        select(st_time_all_in_oneweek,targetVariable) 
      
    }
    
    maxy1 = quantile(data_weekday$targetVariable,probs = 0.99)
    maxy2 = quantile(data_weekday_2$targetVariable,probs = 0.99)
    maxy = max(maxy1,maxy2)
    
    
    return(list("data"=data_weekday,"attr"=attributeSelected,"sensor"=sensorSelected,"start"=startdateT3,"end"=enddateT3,"attrdenom"=attributeSelected_denom,"data_2"=data_weekday_2,"attr_2"=attributeSelected_2,"sensor_2"=sensorSelected_2,"start_2"=startdateT3_2,"end_2"=enddateT3_2,"attrdenom_2"=attributeSelected_2_denom,"maxy"=maxy))
  })
  

  
  output$weekday1 <- renderPlot({
    weekday(weekdaydata()$data,weekdaydata()$attr,weekdaydata()$sensor,weekdaydata()$start,weekdaydata()$end,weekdaydata()$attrdenom,weekdaydata()$maxy)
  })
  
  output$weekday2 <- renderPlot({
    weekday(weekdaydata()$data_2,weekdaydata()$attr_2,weekdaydata()$sensor_2,weekdaydata()$start_2,weekdaydata()$end_2,weekdaydata()$attrdenom_2,weekdaydata()$maxy)
  })
  
}


weekday <- function(data,attr,sensor,startdate,enddate,attrdenom,maxy) {
  ggplot(data,aes(st_time_all_in_oneweek,targetVariable)) +
    geom_smooth(stat = 'summary', color = 'blue', fill = 'grey', alpha = 0.4, size = 0.2, 
                fun.data = median_hilow, fun.args = list(conf.int = 0.5))+
    stat_summary(fun = quantile,fun.args = list(probs = 0.25),color = "green",geom = "line", size = 0.2) +
    stat_summary(fun = quantile,fun.args = list(probs = 0.75),color = "orange",geom = "line", size = 0.2) +
    scale_x_datetime(date_labels = "%a",date_breaks = "1 day",date_minor_breaks = "1 hour", timezone = "EST") + 
    scale_y_continuous(limits = c(0,maxy)) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
      # axis.text = element_text(size = 48)
    ) +
    labs(title = paste(sensor,lookupattrname[[attr]],sep =" : "),subtitle = paste(startdate,enddate,sep = " to "),caption = paste("Comparison(ratio between): ",paste(attr,attrdenom,sep = ",")))

}