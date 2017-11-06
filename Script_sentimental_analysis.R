library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(streamR)
library(ROAuth)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(httpuv)


api_key <- "API KEY" # From dev.twitter.com
api_secret <- "API Secret" # From dev.twitter.com
token <- "Access token" # From dev.twitter.com
token_secret <- "Access token secret" # From dev.twitter.com


api_key <- "0GHvjraDkig57Sqo7UGooojeW"
api_secret <- "wEtxz8TVwhxBcSrr1IlYcXEOUEayje9Fv3BllFOgWpkGP7h56n"
token <- "4925212030-xgAdYM95owRbUGCtPulgmM9S9g2uXKJqhuD2nyD"
token_secret <- "7UgTCF4ARXn6UqXk6VYfz2KJtJoD2d8wL7bPL8bUZkXOV" 

setup_twitter_oauth(api_key, api_secret, token, token_secret)


tweet_collection = searchTwitter('New York + Apartment +  Rental', n=200, lang="en")

tweets_extact = sapply(tweet_collection, function(x) x$getText())


# remove retweet entries
tweets_extact = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_extact)

# remove at people
tweets_extact = gsub("@\\w+", "", tweets_extact)

# remove punctuation
tweets_extact = gsub("[[:punct:]]", "", tweets_extact)

# remove numbers
tweets_extact = gsub("[[:digit:]]", "", tweets_extact)

# remove online html links
tweets_extact = gsub("http\\w+", "", tweets_extact)

# remove irrevalent spaces
tweets_extact = gsub("[ \t]{2,}", "", tweets_extact)
tweets_extact = gsub("^\\s+|\\s+$", "", tweets_extact)

# define "tolower error handling" function 
try.error = function(x)
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

# lower case using try.error with sapply 
tweets_extact = sapply(tweets_extact, try.error)

# remove NAs in tweets_extact
tweets_extact = tweets_extact[!is.na(tweets_extact)]
names(tweets_extact) = NULL

# classify emotion
emotion_analysis = classify_emotion(tweets_extact, algorithm="bayes", prior=1.0)

# get emotion best fit
emotion = emotion_analysis[,7]

# substitute NA's by "unknown"
emotion[is.na(emotion)] = "neutral"

# classify polarity
polarity_analysis = classify_polarity(tweets_extact, algorithm="bayes")
# get polarity best fit
polarity = polarity_analysis[,4]


# data frame with results
tweets_df1 = data.frame(text=tweets_extact, emotion=emotion,
                           polarity=polarity, stringsAsFactors=FALSE, Date = as.Date('02/25/2016','%m/%d/%Y'))

# sort data frame
tweets_df1 = within(tweets_df1,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))), Date = as.Date('02/25/2016','%m/%d/%Y'))

#tweets_df <- rbind(tweets_df1, tweets_df2, tweets_df3, tweets_df4, tweets_df5, tweets_df6, tweets_df7, tweets_df8)

# Let’s do some plots of the obtained results
# plot distribution of emotions
ggplot(tweets_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="Count of Tweets") +
  ggtitle("Sentiment Analysis of Tweets about Real State Data Analysis\n(classification by emotion)") +
  theme(plot.title = element_text(size=12, face="bold"))


# plot distribution of polarity
ggplot(tweets_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="Count of Tweets") +
  ggtitle("Sentiment Analysis of Tweets about Real State Data Analysis\n(classification by polarity)") +
  theme(plot.title = element_text(size=12, face="bold"))



# Separate the text by emotions and visualize the words with a comparison cloud
# separating text by emotion
emos = levels(factor(tweets_df$emotion))
colorcombination = length(emos)
emo.docs = rep("", colorcombination)
for (i in 1:colorcombination)
{
  tmp = tweets_extact[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
# get word counts in decreasing order
word_freqs = sort(rowSums(tdm), decreasing = TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word = names(word_freqs), freq = word_freqs)

wordcloud(dm$word, dm$freq, random.order = FALSE,max.words=Inf, lang="en", colors = brewer.pal(colorcombination, "Dark2"), title.size = 1.2, scale = c(3,.7))
