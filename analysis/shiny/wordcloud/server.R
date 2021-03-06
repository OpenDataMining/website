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
source("/var/www/EVAnalysisCorp.com/website/analysis/shiny/wordcloud/tagcloud.R")

shinyServer(function(input, output){
  output$plot = renderPlot(prepareWordCloud(query = input$tag, num_tweets = input$numTweets))
})