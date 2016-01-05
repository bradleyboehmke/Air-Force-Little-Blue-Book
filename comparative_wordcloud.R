# This script imports the Air Force Blue Book text documents and assesses 
# performs a comparative word cloud.

library(tm)
library(wordcloud)

# import text files
V1 <- read.csv("1983r.txt", header=FALSE, comment.char="#", stringsAsFactors = FALSE)
V2 <- read.csv("1997r.txt", header=FALSE, comment.char="#", stringsAsFactors = FALSE)
V3 <- read.csv("2012r.txt", header=FALSE, comment.char="#", stringsAsFactors = FALSE)
V4 <- read.csv("2015r.txt", header=FALSE, comment.char="#", stringsAsFactors = FALSE)

# convert to list
V1 <- list(V1 = V1)
V2 <- list(V2 = V2)
V3 <- list(V3 = V3)
V4 <- list (V4 = V4)

#######################
# Text Cleaning #
#######################
# Text cleaning function:
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

# Clean texts:
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

## Comparison Wordcloud:
comparison.cloud(tdm, 
                 scale=c(5,0.5),
                 min.freq = 10,
                 max.words=1000,
                 random.order=FALSE,
                 title.size=1.75,
                 random.color = FALSE,
                 rot.per=0, 
                 fixed.asp = TRUE,
                 use.r.layout=FALSE,
                 tryfit=TRUE,
                 colors=brewer.pal(8, "Set1"))

## Commonality Wordcloud:
commonality.cloud(tdm,
                  max.words = 40,
                  random.order = FALSE,
                  random.color = FALSE,
                  rot.per=0, 
                  fixed.asp = TRUE,
                  use.r.layout=FALSE)
