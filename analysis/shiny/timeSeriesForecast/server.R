library(shiny)
library(forecast)

shinyServer(function(input, output){
  #file = input$file1
  #if (is.null(inFile))
  #   return NULL
  
  #BEGIN PLACEHOLDER
  #will be changed to support user input
  #it may run slower now at first, but it will be pretty fast after the first time
  homeData = read.csv("/var/www/EVAnalysisCorp.com/website/analysis/shiny/timeSeriesForecast/case.csv", header=TRUE, sep=",")
  homeTimeseries = ts(data=homeData$Index, start=c(1987,1), end=c(2013,3), deltat=1/12)
  profitData = read.csv("/var/www/EVAnalysisCorp.com/website/analysis/shiny/timeSeriesForecast/profit.csv", header=TRUE, sep=",")
  profitTimeseries = ts(data=profitData$Profit, start=c(2001,4), end=c(2013,1), deltat=1/4)
  #END PLACEHOLDER
  homeArima = auto.arima(homeTimeseries)
  homeets = ets(homeTimeseries)
  profitArima=auto.arima(profitTimeseries)
  profitets= ets(profitTimeseries)
  arima = reactive({
    switch(input$choice,
    "Case-Shiller Home Price Index" = homeArima,
    "Corporate Profits After Tax" = profitArima)
    })
  ets = reactive({
    switch(input$choice,
    "Case-Shiller Home Price Index" = homeets,
    "Corporate Profits After Tax" = profitets)
    })
  model = reactive({
     switch(input$model,
     "ARIMA" = arima(),
     "ETS" = ets())
     })
  predict = reactive({
    forecast(model(), input$h)
    })
  ylabel = reactive({
    switch(input$choice,
           "Case-Shiller Home Price Index" = "Value of Index",
           "Corporate Profits After Tax" = "Profit (in billions of USD)")
  })
  output$forecast = renderPlot({
    plot(predict(), xlab="Year", ylab=ylabel())
  })
})
