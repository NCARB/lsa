#Looad Libs
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
	'XML', 
	'tm', 
	'wordcloud',
	'RColorBrewer',
  'plyr',
	'ggplot2',
	'httr',
	'slam',
	'mime',
	'R6',
	'twitteR',
	'bit',
	'bit64',
	'rjson',
	'DBI',
	'Rstem',
	'NLP',
	'sentiment',
	'Rcpp',
	'igraph',
	'SnowballC',
	'qdap'
	)

#Size management
dev.new <- function(width = 14, height = 14) { 
  platform <- sessionInfo()$platform 
if (grepl("linux",platform)) 
{ x11(width=width, height=height) } 
else if (grepl("pc",platform)) 
{ windows(width=width, height=height) } 
else if (grepl("apple", platform)) 
{ quartz(width=width, height=height) }
}

#Generic function to cleanup corpus
buildScope=function(data){

    #Create Corpus
  scope <-list()
	scope$corpus <- Corpus(VectorSource(data))
	scope$corpus <- tm_map(scope$corpus , removePunctuation)

	#stemming is too rough
	#scope$corpus <- tm_map(scope$corpus , stemDocument,language="english")
	scope$corpus <- tm_map(scope$corpus , PlainTextDocument)
	scope$tdm <- TermDocumentMatrix(scope$corpus)
	scope$m <- as.matrix(scope$tdm)
	scope$v <- sort(rowSums(scope$m),decreasing=TRUE)
	scope$d <- data.frame(word = names(scope$v),freq=scope$v)
	return (scope)
}

wordCount <- function(scope)
{
	table(scope$d$freq)
	pal2 <- brewer.pal(8,"Dark2")
	dev.new()
	wordcloud(scope$d$word,scope$d$freq, scale=c(8,.2),min.freq=3,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

}

sent<- function(data){
	some_txt<-data

	# classify emotion
	class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)

	# get emotion best fit
	emotion = class_emo[,7]

	# substitute NA's by "unknown"
	emotion[is.na(emotion)] = "unknown"

	# classify polarity
	class_pol = classify_polarity(some_txt, algorithm="bayes")

	# get polarity best fit
	polarity = class_pol[,4]

	# data frame with results
	sent_df = data.frame(text=some_txt, emotion=emotion,
	polarity=polarity, stringsAsFactors=FALSE)

	# sort data frame
	sent_df = within(sent_df, emotion)
	# plot distribution of emotions
	ggplot(sent_df, aes(x=emotion)) +
	geom_bar(aes(y=..count.., fill=emotion)) +
	scale_fill_brewer(palette='Dark2') +
	labs(x='emotion categories', y='number of comments') +
	labs(title = 'Sentiment Analysis', plot.title = element_text(size=12))
}
adj <-function(scope){

	lim = quantile(scope$v , probs=0.2)
	good = scope$m[scope$v > lim,]
	#
	good = good[,colSums(good)!=0]
	# adjacency matrix
	M = good %*% t(good)
	#
	# undirected, symmetric -> zeroes in diagonal
	diag(M) = 0
	#
	# graph
	g = graph.adjacency(M, weighted=TRUE, mode="undirected",
	                    add.rownames=TRUE)
		# layout
	glay = layout.fruchterman.reingold(g)
	# a cluster structure with k-means clustering
	kmg = kmeans(M, centers=8)
	gk = kmg$cluster
	#
	# color management
	gbrew = c("red", brewer.pal(8, "Dark2"))
	gpal = rgb2hsv(col2rgb(gbrew))
	gcols = rep("", length(gk))
	#
	for (k in 1:8) {
	  gcols[gk == k] = hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
	}
	#
	# prepare ingredients for plot
	V(g)$size = 10
	V(g)$label = V(g)$name
	V(g)$degree = degree(g)
	V(g)$label.cex = 1.5 * log10(V(g)$degree)
	V(g)$label.color = hsv(0, 0, 0.2, 0.55)
	V(g)$frame.color = NA
	V(g)$color = gcols
	E(g)$color = hsv(0, 0, 0.7, 0.3)
	#
	# plot
	#plotfile2 <- "krankheit_depression_03042015.png"
	#png(filename=plotfile2, width=12, height=8, units="in", res=300)
	dev.new()  
	plot(g, layout=glay)
	title("\nGraph of IDPC questions ",
	      col.main="gray", family='serif')
	#
	#dev.off()
}

readInput <-function(fileName,removeWords=c('idp','ncarb','experience','question','will','can','also','however','like','just','please','way','since','still','may','get','do','does','record','now','thank','intern','thankyear')){
  #Clean file. I do this instead of stemming. Stemming is to invasiave for this work
  cleanFileName <- paste('Clean_',fileName,sep='')
  content  <- readLines(fileName)
  content<-gsub('[[:digit:]]+', '', content)
  content <- tolower(content)
  content<-removeWords(content, c(removeWords,stopwords("english")))
  word <- c('students','interns','hours', 'settings', 'thanks','degrees','schools','elegibilities','universities','supervisors','architects','coordinators','transcripts','years','credits')
  repl <- c('student','intern','hour', 'setting', 'thank','degree','school','elegibility','university','supervisor','architect','coordinator','transcript','year','credit')
  #content <-mapply(gsub , word, repl, content)
  #this requires rJava you can comment it out if can't get it to install
  content  <- mgsub( word, repl, content) 
  writeLines(content, con=cleanFileName)
  data <-read.csv(text=content, header=TRUE)
  return(data)
}
data<- readInput ('idpc.csv')
bodyScope <- buildScope(data$body)
subjectScope <- buildScope(data$subject)

#Load IDPC questions' subject
# wordCount (data$Subject)
#Load IDPC questions' body
adj(subjectScope)
#adj(bodyScope)
#wordCount (bodyScope)
#wordCount (subjectScope)

