library("twitteR")
library("ROAuth")
library("RCurl")
library("bitops")
library("rjson")
library(tm)
library(SnowballC)
library(googleVis)
library(wordcloud)
dir <- ("C:/Users/amol6/Desktop/Boston University/Spring 18/Web Analytics and Mining")
setwd(dir)
t.api.key <- "Vl9rKbvNO3QrQi4KVJ8b0jzze"
t.api.secret <- "nt82FF5kKsdSuv7gnucLYbzgpPYM6fi0u4mEI3nMOSYLRBqvHM"
t.reqURL <- "https://api.twitter.com/oauth/request_token"
t.accessURL <- "https://api.twitter.com/oauth/access_token"
t.authURL <- "https://api.twitter.com/oauth/authorize"
t.credentials <- OAuthFactory$new(consumerKey=t.api.key,
                                  consumerSecret=t.api.secret, 
                                  requestURL=t.reqURL,
                                  accessURL=t.accessURL,
                                  authURL=t.authURL)
t.credentials$handshake()
setup_twitter_oauth(t.credentials$consumerKey,t.credentials$consumerSecret,t.credentials$oauthKey,
                    t.credentials$oauthSecret)
save(list="t.credentials", file="twitter_credentials.RData")
#Pick 2 sets of 3 stocks. For the first set select the 3 largest gainer stocks for that day, and
#for the second set select the 3 largest loser stocks for that day
#Set-1:Top 3 gainers for the day Sunday 22 April 2018
#a. Search for the 100 tweets associated with each of these two sets
tweets1=searchTwitter("$TRU",n=100)
tweets2=searchTwitter("$PF",n=100)
tweets3=searchTwitter("$VHI",n=100)
display.tweet <- function (tweet) {
  cat("Screen name:", tweet$getScreenName(), 
      "\nText:", tweet$getText(), "\n\n")
}
for (t in tweets1) {
  display.tweet(t)
}
for (t in tweets2) {
  display.tweet(t)
}
for (t in tweets3) {
  display.tweet(t)
}
#Set-2:Top losers for the day Sunday 22 April 2018
tweets4=searchTwitter("$MAN",n=100)
tweets5=searchTwitter("$SAGE",n=100)
tweets6=searchTwitter("$GNTX",n=100)
display.tweet <- function (tweet) {
  cat("Screen name:", tweet$getScreenName(), 
      "\nText:", tweet$getText(), "\n\n")
}
for (t in tweets4) {
  display.tweet(t)
}
for (t in tweets5) {
  display.tweet(t)
}
for (t in tweets6) {
  display.tweet(t)
}
#Combine all the gainers (losers) into one set of 300 tweets. 
tweetgain<-c(tweets1,tweets2,tweets3)
length(tweetgain)
save(tweetgain,file="tweetgain.RData")
tweetloss<-c(tweets4,tweets5,tweets6)
save(tweetloss,file = "tweetloss.RData")
length(tweetloss)
#b) Create two separate data corpora for the above two sets of tweets.
#Create a simple function that takes a tweet and returns a corpus. 
getCorpus <- function(tweets){
  tweets.text <- lapply(tweets, function(t){t$getText()})
  data.source <- VectorSource(tweets.text)
  data.corpus <- Corpus(data.source)
  return(data.corpus)
}
data.corpus1 <- getCorpus(tweetgain)
data.corpus2 <- getCorpus(tweetloss)
#savingthis
save(data.corpus1,file="Data1.RData")
save(data.corpus2,file="Data2.RData")
#c Pre-Processing
removeURL <- function(x){
  gsub("(http[^ ]*)","",x)
}

removeNumbersWords <- function(x){
  gsub("([[:digit:]]+0=)[[:alnum:]])*","",x)
  
}

transformcorp <- function(data.corpus) {
  data.corpus <- tm_map(data.corpus, content_transformer(removeURL))
  data.corpus <- tm_map(data.corpus, content_transformer(removePunctuation))
  english.stopwords <- stopwords("en")
  data.corpus <- tm_map(data.corpus, content_transformer(removeWords),english.stopwords)
  data.corpus <- tm_map(data.corpus, content_transformer(removeNumbersWords))
  data.corpus <- tm_map(data.corpus, content_transformer(stemDocument))
  data.corpus <- tm_map(data.corpus, content_transformer(stripWhitespace))
  return(data.corpus)
}
datat1<- transformcorp(data.corpus1)
datat2<- transformcorp(data.corpus2)
#d Create the term-document matrix for each set
tdm1 <- TermDocumentMatrix(datat1)
dtm2 <- TermDocumentMatrix(datat2)
save(tdm1,file="tdm1.RData")
save(dtm2,file="dtm2.RData")
#e Find the most frequent terms from each set
fqterm1<- findFreqTerms(tdm1, lowfreq = 3)
fqterm2<- findFreqTerms(dtm2, lowfreq = 3)
fqterms<- c(fqterm1,fqterm2)
fqterms
#Show word cloud for each set
wordFreq1 <- rowSums(as.matrix(tdm1))
wordFreq2 <- rowSums(as.matrix(dtm2))
palette <- brewer.pal(8,"Dark2")
palette
set.seed(137)
wordcloud(words=names(wordFreq1), 
          freq=wordFreq1, 
          min.freq=5, 
          random.order=F,
          colors=palette)
wordcloud(words=names(wordFreq2), 
          freq=wordFreq2, 
          min.freq=10, 
          random.order=F,
          colors=palette)
#f. Sentiment Analysis
sentiment <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  score <- sum(pos.matches) - sum(neg.matches)
  cat (" Positive: ", words[pos.matches], "\n")
  cat (" Negative: ", words[neg.matches], "\n")
  return (score)
}

pos.words = scan('positive-words.txt',
                 what='character',
                 comment.char=';')

neg.words = scan('negative-words.txt',  
                 what='character', 
                 comment.char=';')

save(list="tweetgain", file="tweetgain.RData")
load(file="tweetgain.RData")
tweetgain.texts <- lapply(tweetgain, function(t) {iconv(t$getText(),"latin1", "ASCII", sub="")})

save(list="tweetloss", file="tweetloss.RData")
load(file="tweetloss.RData")
tweetloss.texts <- lapply(tweetloss, function(t) {iconv(t$getText(),"latin1", "ASCII", sub="")})

sentiment(tweetgain.texts, pos.words, neg.words)
sentiment(tweetloss.texts, pos.words, neg.words)
scores <- sapply(tweetgain.texts,sentiment, pos.words, neg.words)
scores1 <- sapply(tweetloss.texts,sentiment, pos.words, neg.words)
table(scores)
table(scores1)
barplot(table(scores), xlab="Score", ylab="Count",col="red")
barplot(table(scores1), xlab="Score", ylab="Count",col="yellow")
