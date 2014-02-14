library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Time Series Forecasting"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
  	selectInput("choice", "Choose a time series to forecast:",
  		choices = c("Case-Shiller Home Price Index", "Corporate Profits After Tax")),
   	 selectInput("model", "Choose a forecasting model:",  
	   choices = c("ARIMA", "ETS")),
	sliderInput("h", 
                "Forecast Distance", 
                min = 10,
                max = 120, 
                value = 60),
	helpText("Note: Dark grey areas indicate 80% certainty,",
             "while light grey areas indicate 95% certainty.",
			 "The graph may take some time to load."),
			  HTML("<hr>"),
   			h6(a("Return to the demo page", 
        		 href="http://www.evanalysiscorp.com/cases/demos.php"))
                
  ),
   mainPanel(
    plotOutput("forecast")
    )
  )
)
