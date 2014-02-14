source("~/EVAnalysisCorp/website/analysis/shiny/wordcloud/preProcessing.R")
#5090881

preProcessing <- function(dfText)
{
	# remove "retweet entities RT word"
	text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", dfText)
	# remove "at people"
	text = gsub("@\\w+", " ", text)
	# remove apostroph '
	text = gsub("\'", "", text)
	# remove numbers, I dont think we should remove numbers
	# text_noNum = gsub("[[:digit:]]", " ", text_noPunc)
	# remove punctuation
	text = gsub("[[:punct:]]", "", text)
	# remove html links
	text = gsub("http\\w+", "", text)
	# remove backslash
	text = gsub("\\[:alnum:]*", " ", text)
	#remove backslash n
	text = gsub("\\n|\\v", " ", text)	
	# remove \023
	text = gsub("\023", " ", text)
	# remove unnecessary spaces
	text = gsub("[ \t]{2,}", " ", text)
	text = gsub("^\\s+|\\s+$", " ", text)
	return(text)
}

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

tweets_text = df1$text[1:10000]
	
	
prepareCluster <- function(tweets_text)
{
	tweets_text = df1$text[1:100]
	text_clean = preProcessing(tweets_text)
	# lower case using tryTolower with sapply 
	text_lower = sapply(text_clean, tryTolower)
	# remove NAs in some_txt
	text_noNA = text_lower[!is.na(text_lower)]
	names(text_noNA) = NULL

	# text_noNum = gsub("[[:digit:]]", "", text_noPunc)

	mycorpus = Corpus(VectorSource(text_noNA))
	mycorpus = tm_map(mycorpus, function(x) removeWords(x, stopwords("english")))
	dtm <- DocumentTermMatrix(mycorpus)
	dtm_tfxidf <- weightTfIdf(dtm)

	m <- as.matrix(dtm_tfxidf)

rownames(m) <- 1:nrow(m)



	z.tdm=TermDocumentMatrix(x)
	ap.m <- as.matrix(z.tdm)
	ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
	ap.d <- data.frame(word = names(ap.v),freq=ap.v)
	#table(ap.d$freq)
	pal2 <- brewer.pal(8,"Dark2")
	wordcloud(ap.d$word,ap.d$freq, scale=c(10,.5),min.freq=1,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
}

tweets=searchTwitter("starbucks", n=500, lang="en")
tweets_df = twListToDF(tweets)
text_clean = preProcessing(tweets_df$text)
# lower case using tryTolower with sapply 
text_lower = sapply(text_clean, tryTolower)
# remove NAs in some_txt
text_noNA = text_lower[!is.na(text_lower)]
names(text_noNA) = NULL
mycorpus = Corpus(VectorSource(text_noNA))
x=tm_map(mycorpus, function(x) removeWords(x, stopwords("english")))
x=tm_map(mycorpus, function(x) removeWords(x, c("starbucks")))
z.tdm=TermDocumentMatrix(x)
ap.m <- as.matrix(z.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
dtm=DocumentTermMatrix(x)
findAssocs(dtm, "coffee", 0.3)
findFreqTerms(dtm, lowfreq=20)
dtm.df <- as.data.frame(inspect(dtm))
dtm2 <- removeSparseTerms(dtm, sparse=0.98)
dtm2.df <- as.data.frame(inspect(dtm2))
dtm2.df.scale <- scale(dtm2.df)
d <- dist(dtm2.df.scale, method = "euclidean")
fit <- hclust(d, method="ward")
plot(fit) # display dendogram?
groups <- cutree(fit, k=5) # cut tree into 5 clusters
rect.hclust(fit, k=5, border="red")




