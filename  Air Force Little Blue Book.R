setwd("~/Desktop/R/Air Force Little Blue Book")
library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)
library(stringr)
library(RWeka)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggplot2) # plot word frequencies
library(scales) # format axis scales for plots
# pull in text data
V1 <- read.csv("~/Desktop/R/Air Force Little Blue Book/1983r.txt", header=FALSE, comment.char="#", stringsAsFactors = FALSE)
V2 <- read.csv("~/Desktop/R/Air Force Little Blue Book/1997r.txt", header=FALSE, comment.char="#", stringsAsFactors = FALSE)
V3 <- read.csv("~/Desktop/R/Air Force Little Blue Book/2012r.txt", header=FALSE, comment.char="#", stringsAsFactors = FALSE)
V4 <- read.csv("~/Desktop/R/Air Force Little Blue Book/2015r.txt", header=FALSE, comment.char="#", stringsAsFactors = FALSE)
# convert to list
V1 <- list(V1 = V1)
V2 <- list(V2 = V2)
V3 <- list(V3 = V3)
V4 <- list (V4 = V4)
#######################
# Text Cleaning #
#######################
# Cleaning the Text Function:
clean.text <- function(x)
{
  x <- tolower(x)
  x <- gsub("rt", "", x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("[[:digit:]]", "", x)
  x <- gsub("^ ", "", x)
  x <- gsub(" $", "", x)
  return(x)
}
#Clean the texts:
V1_clean <- clean.text(V1)
V2_clean <- clean.text(V2)
V3_clean <- clean.text(V3)
V4_clean <- clean.text(V4)
#Join texts in a vector for each company
V1 <- paste(V1_clean, collapse=" ")
V2 <- paste(V2_clean, collapse=" ")
V3 <- paste(V3_clean, collapse=" ")
V4 <- paste(V4_clean, collapse=" ")
all <- c(V2, V1, V3, V4)
#Remove stopwords
all <- removeWords(all, stopwords("english"))
#Corpus and term-document matrix
corpus <- Corpus(VectorSource(all))
tdm <- TermDocumentMatrix(corpus)
tdm <- as.matrix(tdm)
colnames(tdm) <- c("1997", "1983", "2012", "2015")
## Developing the Wordcloud:
comparison.cloud(tdm
                 , scale=c(5,.05)
                 , min.freq = 10
                 , max.words=1000
                 , random.order=FALSE
                 , title.size=1
                 , random.color = FALSE
                 , rot.per=0 # % of vertical words
                 , fixed.asp = TRUE
                 , use.r.layout=TRUE
                 , tryfit=TRUE
                 , colors=brewer.pal(8, "Set1"))