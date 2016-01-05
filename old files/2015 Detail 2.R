library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)
# library(Rgraphviz) ##Need to install
library(stringr)
library(RWeka)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggplot2) # plot word frequencies
library(scales) # format axis scales for plots

# pull in text data
V4 <- read.csv("~/Desktop/R/Air Force Little Blue Book/2015r.txt", header=FALSE, comment.char="#", stringsAsFactors = FALSE)

# convert to list
V4 <- list(V4 = V4)

# convert to corpus
V4.corpus <- Corpus(VectorSource(V4))


#######################
# Text Pre-processing #
#######################
## Function(s) for text cleaning
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

## Cleaning the Text:
V4.corpus <- tm_map(V4.corpus, toSpace, "/|@|\\|")
V4.corpus <- tm_map(V4.corpus, toSpace, "[^[:alpha:]]")
V4.corpus <- tm_map(V4.corpus, tolower)
V4.corpus <- tm_map(V4.corpus, removeWords, stopwords("english"))
V4.corpus <- tm_map(V4.corpus, stripWhitespace)
V4.corpus <- tm_map(V4.corpus, stemDocument)
V4.corpus <- str_trim(V4.corpus, side = "both")

## convert back to corpus
V4.corpus <- list(V4.corpus = V4.corpus)
V4.corpus <- Corpus(VectorSource(V4.corpus))

## Developing the Wordcloud:
wordcloud (V4.corpus
           , scale=c(4,.05)
           , min.freq = 2
           , max.words=250
           , random.order=FALSE
           , random.color = FALSE
           , rot.per=0 # % of vertical words
           , fixed.asp = TRUE
           , use.r.layout=TRUE #Uses C++ for collision detection
           #, tryfit=TRUE
           #, colors=brewer.pal(8, "Dark2"))
           , colors="black")

rm(V4, toSpace)

######################
# Generating n-grams #
######################
options(mc.cores = 1)
V4.corpus <- tm_map(V4.corpus, PlainTextDocument)

# create 1-grams
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
V4.1gram <- TermDocumentMatrix(V4.corpus, control = list(tokenize = UnigramTokenizer))

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(V4.1gram)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$Token <- rownames(matrix.tdm)
names(df.tdm)[1] <- c("Frequency")
df.tdm <- select(df.tdm, Token, Frequency)

# create full Unigram dataframe
Unigram.df <- df.tdm %>%
  group_by(Token) %>%
  summarise(Frequency = sum(Frequency)) %>%
  arrange(desc(Frequency))

# create 2-grams
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
V4.2gram <- TermDocumentMatrix(V4.corpus, control = list(tokenize = BigramTokenizer))

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(V4.2gram)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$Token <- rownames(matrix.tdm)
names(df.tdm)[1] <- c("Frequency")
df.tdm <- select(df.tdm, Token, Frequency)

# create full Bigram dataframe
Bigram.df <- df.tdm %>%
  group_by(Token) %>%
  summarise(Frequency = sum(Frequency)) %>%
  arrange(desc(Frequency))

# create 3-grams
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
V4.3gram <- TermDocumentMatrix(V4.corpus, control = list(tokenize = TrigramTokenizer))

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(V4.3gram)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$Token <- rownames(matrix.tdm)
names(df.tdm)[1] <- c("Frequency")
df.tdm <- select(df.tdm, Token, Frequency)

# create full Trigram dataframe
Trigram.df <- df.tdm %>%
  group_by(Token) %>%
  summarise(Frequency = sum(Frequency)) %>%
  arrange(desc(Frequency))

# create 4-gram
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
V4.4gram <- TermDocumentMatrix(V4.corpus, control = list(tokenize = QuadgramTokenizer))

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(V4.4gram)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$Token <- rownames(matrix.tdm)
names(df.tdm)[1] <- c("Frequency")
df.tdm <- select(df.tdm, Token, Frequency)

# create full Unigram dataframe
Quadgram.df <- df.tdm %>%
  group_by(Token) %>%
  summarise(Frequency = sum(Frequency)) %>%
  arrange(desc(Frequency))

rm(df.tdm, matrix.tdm, V4.1gram, V4.2gram, V4.3gram,
   V4.4gram, QuadgramTokenizer, BigramTokenizer, TrigramTokenizer, UnigramTokenizer)

##############################
# Visual Analysis of n-grams #
##############################
# Plot unigram
Unigram.Top10 <- Unigram.df %>%
  mutate(Rank = seq(from = 1, to = length(Token), by = 1)) %>%
  filter(Rank <= 10)

ggplot(Unigram.Top10, aes(Frequency, reorder(Token, -Rank))) +
  geom_segment(aes(yend=Token), xend=0, colour="grey50") +
  geom_point(size=3) +
  theme_bw() +
  ylab("Term") +
  theme(axis.text.y = element_text(size = 12)) +
  ggtitle("Top 10 Most Frequent Words in 2015")

# Plot bigram
Bigram.Top10 <- Bigram.df %>%
  mutate(Rank = seq(from = 1, to = length(Token), by = 1)) %>%
  filter(Rank <= 5)

ggplot(Bigram.Top10, aes(Frequency, reorder(Token, -Rank))) +
  geom_segment(aes(yend=Token), xend=0, colour="grey50") +
  geom_point(size=3) +
  theme_bw() +
  ylab("Term") +
  theme(axis.text.y = element_text(size = 12)) +
  ggtitle("Top 5 Bi-grams in 2015")

# Plot trigram
Trigram.Top10 <- Trigram.df %>%
  mutate(Rank = seq(from = 1, to = length(Token), by = 1)) %>%
  filter(Rank <= 5)

ggplot(Trigram.Top10, aes(Frequency, reorder(Token, -Rank))) +
  geom_segment(aes(yend=Token), xend=0, colour="grey50") +
  geom_point(size=3) +
  theme_bw() +
  ylab("Term") +
  theme(axis.text.y = element_text(size = 12)) +
  ggtitle("Top 5 Most Frequent Tri-grams in 2015")

# Plot quadgram  
Quadgram.Top10 <- Quadgram.df %>%
  mutate(Rank = seq(from = 1, to = length(Token), by = 1)) %>%
  filter(Rank <= 5)

ggplot(Quadgram.Top10, aes(Frequency, reorder(Token, -Rank))) +
  geom_segment(aes(yend=Token), xend=0, colour="grey50") +
  geom_point(size=3) +
  theme_bw() +
  ylab("Term") +
  theme(axis.text.y = element_text(size = 12)) +
  ggtitle("Top 5 Most Frequent 4-grams in 2015")