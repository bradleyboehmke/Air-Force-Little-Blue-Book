# This script imports the 2012 Air Force Blue Book text document and assesses 
# the commonality of the key words "you" and "our". The final output provides 
# the keyword count and the ratio of keywords to total word count for these 
# specific keywords along with the top 10 most common words in this document.

library(magrittr)

# import 2012 .txt file and pre-process
words_v <- "2012r.txt" %>%
        scan(what="character", sep="\n") %>%
        tolower(.) %>%
        strsplit("\\W") %>%
        unlist(.) %>%
        .[. != ""]

######################
# create list output #
######################
# list to hold key word stats
key_word <- list()

# Total word count in document
key_word$total_word_count <- length(words_v)

# Key word frequency
key_word$keyword_count <- words_v %>%
        .[. %in% c("you", "our")] %>%
        table(.)

# Key word ratios
key_word$keyword_ratio <- key_word$keyword_count %>%
        divide_by(length(words_v)) %>%
        multiply_by(100) %>%
        round(digits = 2) %>%
        paste0(., "%") %>%
        noquote(.) %>%
        setNames(c("our", "you"))


# Top ten words
key_word$top10_words <- words_v %>%
        table(.) %>%
        sort(., decreasing = TRUE) %>%
        .[1:10]

key_word
