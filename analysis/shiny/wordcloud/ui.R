library(XML)
library(tm)
library(wordcloud)
library(RColorBrewer)
# library(Snowball)
# library(SnowballC)
library(twitteR)
library(ggplot2)
library(stringr)
library(slam)
library(rjson)
library(lattice)
library(RJSONIO)
library(data.table)
library(igraph)
library(TraMineR) 
library(dtw)
library(ggplot2) 
library(shiny)
library(PerformanceAnalytics)
library(plyr)
library(stringr)
library(lubridate)

shinyUI(pageWithSidebar(

    headerPanel("Twitter Wordcloud"),
    
    sidebarPanel(
        textInput("tag", "Enter query:", "BMW"),
        selectInput("numTweets", "Number of Tweets:", c(50, 100, 150)),
        submitButton("Update"),
        helpText("Don't worry! It takes a while to load."),
         HTML("<hr>"),
   			h6(a("Return to the demo page", 
        		 href="http://www.evanalysiscorp.com/cases/demos.php"))
        
        ),
    mainPanel(
        plotOutput("plot")
  )
))
