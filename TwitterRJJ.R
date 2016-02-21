# Set up & install library twitterR
#install.packages(c("devtools", "rjson", "httr", "base64enc"))
#install.packages("bit64")

#RESTART R session!
library(devtools)
library(twitteR)
library(wordcloud)
library(tm)

api_key <- "9VJSL3NotMkz5z6l9aExseS4H"
api_secret <- "SQbllMONUKhPKnseRnDI5PKCqRMG6hbtOKlRNBjx0dvEKZ40Qs"
access_token <- "153439125-BdV8GS2ffVFkIsbpnFUKo0jtAHXGS3dvXXoXJCER"
access_token_secret <- "0BTDeeAZl5VAagVR8nWhDiXFCfRopKPacks1aq7n02Wpy"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


################### 
#1. retrieve tweets & convert to data frame......................
###################
tweets <- userTimeline("BarackObama", n=3200)
tweets.df <- twListToDF(tweets)
dim(tweets.df)


#################### 
#2. Data preposessing............................................
####################
# build a Corpus
# myCorpus <- tm_map(myCorpus,
#                    content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
#                    mc.cores=1)
myCorpus <- Corpus(VectorSource(tweets.df$text))
# Convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 
myCorpus <- tm_map(myCorpus, removeURL)
# add two extra stopwords: available, via
myStopwords <- c(stopwords("english"), "available", "via")
# remove "r" & "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords, lazy=TRUE)

# keep a copy of corpus
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument, lazy=TRUE)

# make text fit for paper width
for (i in 1:5) {
  cat(paste("[[", i, "]]", sep = ""))
  writeLines(myCorpus[[i]])
}

# stem completion
myCorpus <- tm_map(myCorpus, stemCompletion, 
                  dictionary = myCorpusCopy, lazy=TRUE)

# # count frequency of "mining"
# miningCases <- tm_map(myCorpusCopy, grep, pattern = "\\<mining")
# sum(unlist(miningCases))
# # count frequency of "miners"
# minerCases <- tm_map(myCorpusCopy, grep, pattern = "\\<miners")
# sum(unlist(minerCases))
# # replace "miner" with "mining"
# myCorpus <- tm_map(myCorpus, gsub, pattern = "miners", replacement = "mining", lazy=TRUE)

#library(SnowballC)


####################################
#3. Building a Term-Document Matrix.............................................
####################################
tdm <- TermDocumentMatrix(myCorpus)
tdm


#####################################
#4. Frequent Words and Associations.............................................
#####################################
idx <- which(dimnames(tdm)$Terms == "r")
inspect(tdm[idx + (0:5), 101:110])
#inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 15))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 15)
df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + 
  geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

#which words are associated with "r"?
findAssocs(tdm, "r", 0.2)
#which words are associated with "mining"?
findAssocs(tdm, "mining", 0.25)


################
#5. Word Count..................................................
################
library(wordcloud)
library(RColorBrewer)
m <- as.matrix(tdm)
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F)
# WordCloud with Color
pal2 <- brewer.pal(8,"Dark2")
wordcloud(myCorpus, min.freq = 3, max.words = 100, random.order = F, colors = pal2)


###############
#6. K-means Clustering...................................................
###############
# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)

# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k=6)

#transpose matrix to cluster documents(tweets)
m3 <- t(m2)
set.seed(1)
k <- 6
kmeansResult <- kmeans(m3, k)
# cluster centers
round(kmeansResult$centers)

# print tweets of every cluster
# print (tweets[which(kmeansResult$cluster==i)])
for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")
}

library(fpc)
# partitioning around medoids with estimation of number of clusters
pamResult <- pamk(m3, metric = "manhattan")
# number of clusters identified
k <- pamResult$nc
pamResult <- pamResult$pamobject
# print cluster medoids
for (i in 1:k) {
  cat("cluster", i, ": ",
      colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
}
# plot clustering result
layout(matrix(c(1, 2), 1, 2)) # two graphs per page
plot(pamResult, col.p = pamResult$clustering)

layout(matrix(1)) # change back to one graph per page


###################
#7. Topic Modeling (LDA)...............................................
####################
dtm <- as.DocumentTermMatrix(tdm)
install.packages("topicmodels")
library(topicmodels)
lda <- LDA(dtm, k = 8) # find 8 topics
term <- terms(lda, 4) # first 4 terms of every topic
term
term <- apply(term, MARGIN = 2, paste, collapse = ", ")

# first topic identified for every document (tweet)
topic <- topics(lda, 1)
library(data.table) # package to run as.IDate
topics <- data.frame(date = as.IDate(tweets.df$created), topic)
library(ggplot2)
qplot(date, ..count.., data = topics, geom = "density", fill = term[topic], position = "stack")
  
