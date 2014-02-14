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


flatlist <- function(mylist){
    lapply(rapply(mylist, enquote, how="unlist"), eval)
}

records2df <- function(recordlist, columns) {
    if(length(recordlist)==0 && !missing(columns)){
      return(as.data.frame(matrix(ncol=length(columns), nrow=0, dimnames=list(NULL,columns))))
    }
    un <- lapply(recordlist, flatlist)
    if(!missing(columns)){
        ns <- columns;
    } else {
        ns <- unique(unlist(lapply(un, names)))
    }
    un <- lapply(un, function(x) {
        y <- as.list(x)[ns]
        names(y) <- ns
        lapply(y, function(z) if(is.null(z)) NA else z)})
    s <- lapply(ns, function(x) sapply(un, "[[", x))
    names(s) <- ns
    data.frame(s, stringsAsFactors=FALSE)
}

tryTolower = function(x)
{
   # create missing value
   y = NA
   # tryCatch error
   try_error = tryCatch(tolower(x), error=function(e) e)
   # if not an error
   if (!inherits(try_error, "error"))
   y = tolower(x)
   # result
   return(y)
}


yingliTwitterAuth <- function(credFileName,newReg = FALSE)
{
	if (newReg == TRUE) {
		# the TwitterKey and TwitterSecret is specific to Ying Li
		myTwitterKey = "rV5XImdUgEx2eMBQT7ZLQ"
		myTwitterSecret = "1WKFm7x9b4THNyiFMlziKcw25Um6EVlU4rqxiGdR0I"
		requestURL = "https://api.twitter.com/oauth/request_token"
		authURL = "https://api.twitter.com/oauth/authorize"
		accessURL = "https://api.twitter.com/oauth/access_token"
		myTwitterCred <- OAuthFactory$new(consumerKey=myTwitterKey, consumerSecret=myTwitterSecret, requestURL=requestURL, accessURL=accessURL, authURL=authURL)
		myTwitterCred$handshake()  # when this was called, a pin will be displayed on browser, pin # was 3160793
		save(myTwitterCred, file=credFileName)
		# myTwitterCred is the variable name that holds the cred
	}   
	load(credFileName)
	registerTwitterOAuth(myTwitterCred)
}  


### assuming input is a text column taken from a data frame, 
preProcessing <- function(dfText)
{
	# remove retweet entities
	text_noRT = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", dfText)
	# remove at people
	text_noPeople = gsub("@\\w+", "", text_noRT)
	# remove punctuation
	text_noPunc = gsub("[[:punct:]]", "", text_noPeople)
	# remove numbers, I dont think we should remove numbers
	# text_noNum = gsub("[[:digit:]]", "", text_noPunc)
	# remove html links
	text_noHtml = gsub("http\\w+", "", text_noPunc)
	# remove unnecessary spaces
	text_noSpace = gsub("[ \t]{2,}", "", text_noHtml)
	text_noSpace = gsub("^\\s+|\\s+$", "", text_noSpace)
	return(text_noSpace)
}

### those steps below only need to run once to establish credentials, once established, can re-read them back to a new R session
# see funtion yingliTwitterAuth
# myTwitterKey = "rV5XImdUgEx2eMBQT7ZLQ"
# myTwitterSecret = "1WKFm7x9b4THNyiFMlziKcw25Um6EVlU4rqxiGdR0I"
# requestURL = "https://api.twitter.com/oauth/request_token"
# authURL = "https://api.twitter.com/oauth/authorize"
# accessURL = "https://api.twitter.com/oauth/access_token"
# myTwitterCred <- OAuthFactory$new(consumerKey=myTwitterKey, consumerSecret=myTwitterSecret, requestURL=requestURL, accessURL=accessURL, authURL=authURL)
# myTwitterCred$handshake()  # pin was 3160793
# registerTwitterOAuth(myTwitterCred)
# myTwitterCred
# save(myTwitterCred, file="~/myTwitterCred")   

### in new session, do the registration of cred
# load("../myTwitterCred")   
# registerTwitterOAuth(myTwitterCred)
#Server specific path
filePath = "/var/www/EVAnalysisCorp.com/website/analysis/shiny/wordcloud/myTwitterCred"
newReg = TRUE
if (file.exists(filePath))
  newReg = FALSE
yingliTwitterAuth(credFileName = filePath, newReg)


### to get tweets from REST API
#cranTweets <- userTimeline('cranatic')
#tweets=searchTwitter('BMW', n=500,lang="en")
### convert tweets to data frame
#tweets_df = twListToDF(tweets)

### to get tweets from stream API, below is the output file from Tao, twitter-sample_20130726.txt is the best source as of now
#filename = "/Users/yingli/EVAnalysisCorp/irtm/twitter/twitter-sample_20130726.txt"
#obj1 <- fromJSON(filename,  simplifyWithNames=FALSE, simplify=FALSE) 
#df1=records2df(obj1)
#save(df1, file="/Users/yingli/EVAnalysisCorp/irtm/twitter/df1")
#filename = "/Users/yingli/EVAnalysisCorp/irtm/twitter/twitter-sample_20130725.txt"
#obj <- fromJSON(filename,  simplifyWithNames=FALSE, simplify=FALSE) 
#df=records2df(obj)
#save(df, file="/Users/yingli/EVAnalysisCorp/irtm/twitter/df")

#load("/Users/yingli/EVAnalysisCorp/irtm/twitter/df")
#load("/Users/yingli/EVAnalysisCorp/irtm/twitter/df1")

