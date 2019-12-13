library(tm)
library(lubridate)
library(reshape2)
library(ggplot2)
library(scales)
library(devtools)
library(ROAuth)
library(twitteR)
library(base64enc)
library(httpuv)
library(wordcloud)
library(wordcloud2)
library(syuzhet)


cred <- OAuthFactory$new(consumerKey='kekouhydPNNp8Uy6TQBP',
                         consumerSecret='pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token', accessURL='https://api.twitter.com/oauth/access_token')
save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

#Access Token Secret

setup_twitter_oauth("kekouhydpPNNp8Uy6TQBP",
                    "pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa", #Consumer Secret (API Secret)
                    "9093468391584-Ev31ZLB7Cf0iohfyfgjfhSgRnu",  # Access Token
                    "iwuffgwywhfrX7d6sjQxuB08l48JHhmjgdywgd86G2OPG7BS")  #Access Token Secret
#registerTwitterOAuth(cred)

Tweets <- userTimeline('prattprattpratt', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Tweets.csv",row.names = F)
getwd()
# handleTweets <- searchTwitter('DataScience', n = 10000)
# Read file
prattprattpratt <- read.csv(file.choose())
str(prattprattpratt)
# Build Corpus and DTM/TDM
corpus <- prattprattpratt$text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('thank','just','get'))
cleanset <- tm_map(cleanset, gsub,pattern = 'pages', replacement = 'page')
 
# counted as one.

inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

# Term Document Matrix : Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)

tdm <- as.matrix(tdm)
tdm[1:10,1:20]
# Bar Plot 

w <- rowSums(tdm)  
w <- subset(w, w>= 25) 
barplot(w, las = 2, col = rainbow(50))
# Word Cloud :

w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

## Shapecloud

w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)


############ Sentiment Analysis for tweets:

# Read File 
tcdata <- read.csv(file.choose(), header = TRUE)
tweets <- as.character(tcdata$text)
class(tweets)
# Obtain Sentiment scores 
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment('pretending')
# Pretend has one value of negative and one value for anger
get_nrc_sentiment('can learn') #1 for positive
# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for chris pratt Tweets')


TWO:
  
  library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews #############################
aurl <- "http://www.amazon.in/Apple-MMGF2HN-13-3-inch-Integrated-Graphics/product-reviews/B01FUK9TKG/ref=cm_cr_arp_d_paging_btm_2?showViewpoints=1&pageNumber"
amazon_reviews <- NULL
for (i in 1:20){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"apple.txt")

getwd()
# Snapdeal reviews #############################
surl_1 <- "https://www.snapdeal.com/product/samsung-galaxy-J3-8gb-4g/676860597612/ratedreviews?page="
surl_2 <- "&sortBy=HELPFUL&ratings=4,5#defRevPDP"
snapdeal_reviews <- NULL
for (i in 1:20){
  surl <- read_html(as.character(paste(surl_1,surl_2,sep=as.character(i))))
  srev <- surl %>%
    html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews,srev)
}

write.table(snapdeal_reviews,"samsung.txt")
getwd()

########## Extracting reviews from a travel website ###################
a<-10
rev<-NULL
url1<-"https://www.tripadvisor.in/Hotel_Review-g147399-d2354539-Reviews-or"
url2<-"-The_Venetian_on_Grace_Bay-Providenciales_Turks_and_Caicos.html#REVIEWS"
for(i in 0:8){
  url<-read_html(as.character(paste(url1,i*a,url2,sep="")))
  ping<-url %>%
    html_nodes(".partial_entry") %>%
    html_text() 
  rev<-c(rev,ping)
}
write.table(rev,"travel.txt")

############# IMDB reviews Extraction ################
a<-10
wonder_woman<-NULL
url1<-"http://www.imdb.com/title/tt0451279/reviews?start="
for(i in 0:6){
  url<-read_html(as.character(paste(url1,i*a,sep="")))
  wonder<-url %>%
    html_nodes("#tn15content div+ p") %>%
    html_text() 
  wonder_woman<-c(wonder_woman,wonder)
}
write.table(wonder_woman,file="wonder_woman.txt")

3.
  library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(reshape2)
library(dplyr)
library(ggplot2)
library(scales)
# IMDBReviews #############################
aurl <- "https://www.imdb.com/title/tt1950186/reviews?ref_=tt_ql_3"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)   ##690

write.table(IMDB_reviews,"ford vs ferrari.txt",row.names = F)   ## will be in notepad .txt format
getwd()

fvf <- read.delim('ford vs ferrari.txt')
str(fvf)
View(fvf)

# Build Corpus and DTM/TDM
library(tm)
corpus <- fvf[-1,]
head(corpus)
class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('can','film','will','big','since'))
# Since the word film,since,big ,will and can were used, this can be removed as we are 
# mining the tweets for this film.Also the word "Can" is common english word.
# we can pull back the word "can"  if needed.

cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))
# Removing the word movie and movies on similar grounds - as unnecessary.


cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')
# the barplot pulls both character and characters as separate words. this should be 
# counted as one as both holds the same synonym.

inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])
#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]
# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 50) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))
# the word dark,joaquin,like and phoenix as the highest frequency. This implies
# that Movie Joker has got more reviews about the actor and its genre and most of them liked the movie.

# Word Cloud :

w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 500,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(4,0.3),
          rot.per = 0.7)


w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')

## shape cloud
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, 
           minSize = 1)

# lettercloud 
letterCloud(w,word = 'A',frequency(5), size=1)

# Sentiment Analysis for tweets:
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read File 
IMDB_reviews <- read.delim('ford vs ferrari.TXT')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)
# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)
reviews[4]
# ""Just saw F v F at the Telluride Film Festival and it is phenomenal. Perfect performances, incredible visuals and editing, immersive sound, and a riveting story."
# on tweet 3, you have 11 each for anger and anticipation,  4 for disgust, 12 for fear, 9 for sadness, 15 for trust , 
# 17 words for negative and 24 positive.
get_nrc_sentiment('excellent')
# excellent has one Joy, one trust and one positive 
get_nrc_sentiment('no words') #1 Anger and 1 Negative
# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for ford vs ferrari')
