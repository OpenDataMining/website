source("/var/www/EVAnalysisCorp.com/website/analysis/shiny/wordcloud/preProcessing.R")

prepareWordCloud <- function(query = 'BMW', num_tweets = 50, lang = 'en')
{
	tweets=searchTwitter(query, n=num_tweets,lang)
	tweets_df = twListToDF(tweets)
	text_clean = preProcessing(tweets_df$text)
	# lower case using tryTolower with sapply 
	text_lower = sapply(text_clean, tryTolower)
	# remove NAs in some_txt
	text_noNA = text_lower[!is.na(text_lower)]
	names(text_noNA) = NULL

	mycorpus = Corpus(VectorSource(text_noNA))
	x=tm_map(mycorpus, function(x) removeWords(x, stopwords("english")))
	z.tdm=TermDocumentMatrix(x)
	ap.m <- as.matrix(z.tdm)
	ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
	ap.d <- data.frame(word = names(ap.v),freq=ap.v)
	#table(ap.d$freq)
	pal2 <- brewer.pal(8,"Dark2")
	wordcloud(ap.d$word,ap.d$freq, scale=c(10,.5),min.freq=1,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
}

