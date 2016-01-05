# This script imports the Air Force Blue Book text documents and assesses 
# the word count and page density.

library(magrittr)

###############
# Import Data #
###############

# import 1983 .txt file and pre-process
words_1983 <- "1983r.txt" %>%
        scan(what="character", sep="\n") %>%
        tolower(.) %>%
        strsplit("\\W") %>%
        unlist(.) %>%
        .[. != ""]

# import 1997 .txt file and pre-process
words_1997 <- "1997r.txt" %>%
        scan(what="character", sep="\n") %>%
        tolower(.) %>%
        strsplit("\\W") %>%
        unlist(.) %>%
        .[. != ""]

# import 2012 .txt file and pre-process
words_2012 <- "2012r.txt" %>%
        scan(what="character", sep="\n") %>%
        tolower(.) %>%
        strsplit("\\W") %>%
        unlist(.) %>%
        .[. != ""]

# import 2015 .txt file and pre-process
words_2015 <- "2015r.txt" %>%
        scan(what="character", sep="\n") %>%
        tolower(.) %>%
        strsplit("\\W") %>%
        unlist(.) %>%
        .[. != ""]

##########################
# Calculate Descriptives #
##########################

# create empty list
descriptives <- list()

# incorprate page count from manual observation
descriptives$Page_Count <- c(`1983` = 35, 
                             `1997` = 13, 
                             `2012` = 22, 
                             `2015` = 10)

# calculate the word count of each document
descriptives$Word_Count <- c(`1983` = length(words_1983), 
                             `1997` = length(words_1997), 
                             `2012` = length(words_2012), 
                             `2015` = length(words_2015))

# calculate the density (words per page) for each page
descriptives$Words_per_Page <- round(descriptives$Word_Count/descriptives$Page_Count, 0)


print(descriptives)


