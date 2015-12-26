# This script imports the 1983 Air Force Little Blue Book text document and 
# pre-processes the text for sentiment analysis. We use the sentiment package
# which allows us to classify the polarity (e.g. positive or negative) of a set 
# of texts using a naive Bayes classifier trained on Janyce Wiebe's subjectivity 
# lexicon. The positive vs negative word list used by the classify_polarity 
# function can be viewed at sentiment/data/subjectivity.csv

#install.packages("Rstem_0.4-1.tar", repos = NULL, type = "source")
#install.packages("sentiment_0.2.tar.gz", repos = NULL, type = "source")
library(magrittr)
library(Rstem)
library(sentiment)


# import 1983 .txt file and pre-process
words_v <- "1983r.txt" %>%
        scan(what="character", sep="\n") %>%
        tolower(.) %>%
        strsplit("\\W") %>%
        unlist(.) %>%
        .[. != ""] %>%
        paste(collapse = " ")

#Perform sentiment analysis for polarity of the text:
Polarity <- classify_polarity(words_v, algorithm = "bayes")
Polarity
