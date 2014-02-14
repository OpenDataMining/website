### to get tweets from stream API, below is the output file from Tao
### twitter-sample_20130726.txt is the best source as of now
filename = "/Users/yingli/EVAnalysisCorp/irtm/twitter/twitter-sample_20130726.txt"
obj1 <- fromJSON(filename,  simplifyWithNames=FALSE, simplify=FALSE) 
df1=records2df(obj1)
save(df1, file="/Users/yingli/EVAnalysisCorp/irtm/twitter/df1")
filename = "/Users/yingli/EVAnalysisCorp/irtm/twitter/twitter-sample_20130725.txt"
obj <- fromJSON(filename,  simplifyWithNames=FALSE, simplify=FALSE) 
df=records2df(obj)
save(df, file="/Users/yingli/EVAnalysisCorp/irtm/twitter/df")

load("/Users/yingli/EVAnalysisCorp/irtm/twitter/df")
load("/Users/yingli/EVAnalysisCorp/irtm/twitter/df1")

dt1 = data.table(df1)
length(df1$user.id_str) ## total number of users = 150325
length(df1$unique(user.id_str)) # total number of unique users = 14101
setkey(dt1, user.id_str)

tweets_by_user=count(dt1, c("user.id_str"))
colnames(tweets_by_user)[2]="num_tweets"
tweets_by_user = data.table(tweets_by_user)
setkey(tweets_by_user, num_tweets)
tweets_hist =count(tweets_by_user,c("num_tweets"))
colnames(tweets_hist)[2]="num_users"
tweets_hist$pct_tweets = tweets_hist$num_users/150325

retweets = dt1[which(!is.na(dt1$retweeted_status.user.screen_name ))]
dt1_retweets = data.table(retweets)
setkey(dt1_retweets, user.id_str)
retweets_by_user=count(dt1_retweets, c("user.id_str"))
colnames(retweets_by_user)[2]="num_retweets"
retweets_by_user = data.table(retweets_by_user)
setkey(retweets_by_user, num_retweets)
retweets_hist =count(retweets_by_user,c("num_retweets"))
colnames(retweets_hist)[2]="num_users"
retweets_hist$pct_retweets = retweets_hist$num_users/41325

get_retweet_graph_new <- function(dt_tweets)
{
	rt_patterns = which(!is.na(dt_tweets$retweeted_status.user.screen_name ))

	who_retweet = as.vector(1:length(rt_patterns))
	who_post = as.vector(1:length(rt_patterns))
 	weight = as.numeric(rep(1,length(rt_patterns)))

	for (i in 1:length(rt_patterns))
	{
		 # get tweet with retweet entity
		 twit = dt_tweets[rt_patterns[i],]
		 # name of retweeted user
		 who_post[i] = twit$retweeted_status.user.screen_name
		 # name of retweeting user
		 who_retweet[i] = twit$user.screen_name
	}
 
	retweeter_poster = data.table(cbind(who_retweet, who_post,weight))
	setkey(retweeter_poster, who_retweet, who_post)
	retweeter_poster_weight=count(retweeter_poster, c("who_retweet", "who_post"))
	colnames(retweeter_poster_weight)[3] <- "weight"

	# generate graph
	rt_graph = graph.data.frame(retweeter_poster_weight, directed = TRUE, vertices = NULL)

 
	# get vertex names
	V(rt_graph)$label<-V(rt_graph)$name
	return(rt_graph)
}

g = get_retweet_graph_new(dt1)
save(g, file = "/Users/yingli/EVAnalysisCorp/website/analysis/shiny/influencer/g1")

dt2=dt1[1:10000,]
g2 = get_retweet_graph_new(dt2)

get_retweet_graph <- function(dt_tweets)
{
	tweet_txt = dt_tweets$text
	rt_patterns = which(!is.na(dt_tweets$retweeted_status.user.screen_name ))

	who_retweet = as.list(1:length(rt_patterns))
	who_post = as.list(1:length(rt_patterns))
 
	for (i in 1:length(rt_patterns))
	{
		 # get tweet with retweet entity
		 twit = dt_tweets[rt_patterns[i],]
		 # name of retweeted user
		 who_post[[i]] = twit$retweeted_status.user.screen_name
		 # name of retweeting user
		 who_retweet[[i]] = twit$user.screen_name
	}
 
	# and we put it off the list
	who_post = unlist(who_post)
	who_retweet = unlist(who_retweet)

	retweeter_poster = cbind(who_retweet, who_post)
 
	# generate graph
	rt_graph = graph.edgelist(retweeter_poster)
 
	# get vertex names
	ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))
	return(rt_graph)
}

g = get_retweet_graph(dt1)
save(g, file="/Users/yingli/EVAnalysisCorp/website/analysis/shiny/influencer/retweet_graph")
# load it back into g, computation took a long time
load(file="/Users/yingli/EVAnalysisCorp/website/analysis/shiny/influencer/retweet_graph")

### this works for Joshua's machine, ie, Windows machine
get_retweet_graph_old <- function(dt_tweets)
{
	tweet_txt = dt_tweets$text
	rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", tweet_txt, ignore.case=TRUE)

	who_retweet = as.list(1:length(rt_patterns))
	who_post = as.list(1:length(rt_patterns))
 
	for (i in 1:length(rt_patterns))
	{
		 # get tweet with retweet entity
		 twit = dt_tweets[rt_patterns[i],]
		 # get retweet source
		 poster = str_extract_all(twit$text, "(RT|via)((?:\\b\\W*@\\w+)+)")
		 #remove ':'
		 poster = gsub(":", "", unlist(poster),perl=TRUE)
		 # name of retweeted user
		 who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE)
		 # name of retweeting user
		 who_retweet[[i]] = rep(twit$user.screen_name, length(poster))
	}
 
	# and we put it off the list
	who_post = unlist(who_post)
	who_retweet = unlist(who_retweet)

	retweeter_poster = cbind(who_retweet, who_post)
 
	# generate graph
	rt_graph = graph.edgelist(retweeter_poster)
 
	# get vertex names
	ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))
	return(rt_graph)
}

# choose layout
glay = layout.fruchterman.reingold(rt_graph)

rt_graph=g2
glay=layout.fruchterman.reingold(g2)

# plot
par(bg="gray15", mar=c(1,1,1,1))
plot(rt_graph, layout=glay,
 vertex.color="gray25",
 vertex.size=10,
 vertex.label=rt_graph$ver_labs,
 vertex.label.family="sans",
 vertex.shape="none",
 vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
 vertex.label.cex=0.85,
 edge.arrow.size=0.2,
 edge.arrow.width=1.2,
 edge.width=1,
 edge.color=hsv(h=.25, s=1, v=.7, alpha=0.5))
# add title
title("\nTweets from the User account: Who retweets whom",
 cex.main=1, col.main="gray95")

plot(rt_graph, layout=glay,
 vertex.color="gray25",
 vertex.size=1,
 vertex.label=NA,
 vertex.label.family="sans",
 vertex.shape="none",
 vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
 vertex.label.cex=0.35,
 edge.arrow.size=0.4,
 edge.arrow.width=0.5,
 edge.width=0.5,
 edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))

plot(rt_graph,layout=glay, vertex.size=5,vertex.label=NA,edge.arrow.size=0.5)

par(bg="gray15", mar=c(1,1,1,1))
plot(rt_graph, layout=glay,
 vertex.color="gray25",
 vertex.size=1,
 vertex.label=NA,
 edge.arrow.size=0.5,
 edge.arrow.width=0.2,
 edge.width=1,
 edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))

### study the structure of the graph

pr=page.rank(rt_graph)$vector
unfold.tree(rt_graph, mode = c("out"), roots)

V(rt_graph)$id <- seq_len(vcount(rt_graph))-1
roots <- sapply(decompose.graph(rt_graph), function(x) {V(x)$id[ topological.sort(x)[1]+1 ] })
tree=unfold.tree(rt_graph, mode = c("out"),roots=roots[23869])

names = colnames(df1)
num_nonNA=sapply(1:231, function(x) as.numeric(sum(!(is.na(df1[,c(names[x])])))))
agg=data.table(names,num_nonNA, pct_nonNA=num_nonNA/150325)

print(agg, nrows=300)
write(t(agg), file = "~/agg.csv", sep=",",ncolumns=3) ## write to a file






