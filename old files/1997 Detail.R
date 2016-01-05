# This script imports the 1997 Air Force Little Blue Book text document and 
# pre-processes the text into a corpus for n-gram analysis. The script then 
# identifies the 1-4 grams in the document and then visualizes the most
# frequent n-grams.

# libraries required
library(magrittr)
library(tm)
library(stringr)
library(wordcloud)
library(RWeka)
library(dplyr)
library(ggplot2)

# read in text data and convert to corpus
text_corpus <- "1997r.txt" %>%
        scan(what="character", sep="\n") %>%
        list() %>%
        VectorSource() %>%
        Corpus()

#######################
# Text Pre-processing #
#######################
## Function(s) for text cleaning
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

## Cleaning the Text:
clean_corpus <- text_corpus %>%
        tm_map(toSpace, "/|@|\\|") %>%
        tm_map(toSpace, "[^[:alpha:]]") %>%
        tm_map(tolower) %>%
        tm_map(removeWords, stopwords("english")) %>%
        tm_map(stripWhitespace) %>%
        tm_map(stemDocument) %>%
        str_trim(side = "both") %>%
        list() %>%
        VectorSource() %>%
        Corpus() %>%
        tm_map(PlainTextDocument)

rm(text_corpus, toSpace)

#############
# Wordcloud #
#############
wordcloud (clean_corpus
           , scale=c(4,.05)
           , min.freq = 2
           , max.words=250
           , random.order=FALSE
           , random.color = FALSE
           , rot.per=0 # % of vertical words
           , fixed.asp = TRUE
           , use.r.layout=TRUE #Uses C++ for collision detection
           , colors="black")


######################
# Generating n-grams #
######################
options(mc.cores = 1)

# create 1-grams
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
unigram_ls <- TermDocumentMatrix(clean_corpus, control = list(tokenize = UnigramTokenizer))

# turn TermDocumentMatrix into a sorted unigram data frame
unigram_df <- data.frame(Token = unigram_ls$dimnames$Terms, 
                         Frequency = as.matrix(unigram_ls)[,1],
                         row.names = NULL, stringsAsFactors = FALSE) %>%
        arrange(desc(Frequency))


# create 2-grams
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigram_ls <- TermDocumentMatrix(clean_corpus, control = list(tokenize = BigramTokenizer))

# turn TermDocumentMatrix into a sorted bigram data frame
bigram_df <- data.frame(Token = bigram_ls$dimnames$Terms, 
                        Frequency = as.matrix(bigram_ls)[,1],
                        row.names = NULL, stringsAsFactors = FALSE) %>%
        arrange(desc(Frequency))


# create 3-grams
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
trigram_ls <- TermDocumentMatrix(clean_corpus, control = list(tokenize = TrigramTokenizer))

# turn TermDocumentMatrix into a sorted trigram data frame
trigram_df <- data.frame(Token = trigram_ls$dimnames$Terms, 
                         Frequency = as.matrix(trigram_ls)[,1],
                         row.names = NULL, stringsAsFactors = FALSE) %>%
        arrange(desc(Frequency))


# create 4-gram
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
quadgram_ls <- TermDocumentMatrix(clean_corpus, control = list(tokenize = QuadgramTokenizer))

# turn TermDocumentMatrix into a sorted quadgram data frame
quadgram_df <- data.frame(Token = quadgram_ls$dimnames$Terms, 
                          Frequency = as.matrix(quadgram_ls)[,1],
                          row.names = NULL, stringsAsFactors = FALSE) %>%
        arrange(desc(Frequency))


# clean up workspace
rm(unigram_ls, bigram_ls, trigram_ls, quadgram_ls,QuadgramTokenizer, 
   BigramTokenizer, TrigramTokenizer, UnigramTokenizer)


##############################
# Visual Analysis of n-grams #
##############################
# Plot top 10 most frequent unigrams
unigram_df %>%
        mutate(Rank = seq(from = 1, to = length(Token), by = 1)) %>%
        filter(Rank <= 10) %>%
        ggplot(., aes(Frequency, reorder(Token, -Rank))) +
        geom_segment(aes(yend=Token), xend=0, colour="grey50") +
        geom_point(size=3) +
        theme_bw() +
        ylab("Term") +
        theme(axis.text.y = element_text(size = 12)) +
        ggtitle("Top 10 Most Frequent Words in 1997")

# Plot top 10 most frequent bigram
bigram_df %>%
        mutate(Rank = seq(from = 1, to = length(Token), by = 1)) %>%
        filter(Rank <= 10) %>% 
        ggplot(., aes(Frequency, reorder(Token, -Rank))) +
        geom_segment(aes(yend=Token), xend=0, colour="grey50") +
        geom_point(size=3) +
        theme_bw() +
        ylab("") +
        theme(axis.text.y = element_text(size = 12)) +
        ggtitle("Top 10 Bi-grams in 1997")

# Plot top 5 most frequent trigrams
trigram_df %>%
        mutate(Rank = seq(from = 1, to = length(Token), by = 1)) %>%
        filter(Rank <= 5) %>%
        ggplot(., aes(Frequency, reorder(Token, -Rank))) +
        geom_segment(aes(yend=Token), xend=0, colour="grey50") +
        geom_point(size=3) +
        theme_bw() +
        ylab("Term") +
        scale_x_continuous(expand = c(0, 2)) +
        theme(axis.text.y = element_text(size = 12)) +
        ggtitle("Top 5 Most Frequent Tri-grams in 1997")

# Plot top 5 most frequent quadgrams 
quadgram_df %>%
        mutate(Rank = seq(from = 1, to = length(Token), by = 1)) %>%
        filter(Rank <= 5) %>%
        ggplot(., aes(Frequency, reorder(Token, -Rank))) +
        geom_segment(aes(yend=Token), xend=0, colour="grey50") +
        geom_point(size=3) +
        theme_bw() +
        ylab("Term") +
        theme(axis.text.y = element_text(size = 12)) +
        ggtitle("Top 5 Most Frequent 4-grams in 1997")  


