# This script imports the Air Force Blue Book text documents and assesses 
# the frequency of single terms (aka 1-grams).

library(magrittr)
library(dplyr)

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

####################################
# Sorted frequency of single terms #
####################################

# create data frames for each year that sort the most frequently used terms
freq_1983 <- words_1983 %>%
        table(.) %>%
        sort(decreasing = TRUE) %>%
        data.frame() %>%
        add_rownames() %>%
        set_colnames(c("term", "freq"))

freq_1997 <- words_1997 %>%
        table(.) %>%
        sort(decreasing = TRUE) %>%
        data.frame() %>%
        add_rownames() %>%
        set_colnames(c("term", "freq"))

freq_2012 <- words_2012 %>%
        table(.) %>%
        sort(decreasing = TRUE) %>%
        data.frame() %>%
        add_rownames() %>%
        set_colnames(c("term", "freq"))
        
freq_2015 <- words_2015 %>%
        table(.) %>%
        sort(decreasing = TRUE) %>%
        data.frame() %>%
        add_rownames() %>%
        set_colnames(c("term", "freq"))


##############################################
# Look at common words between all documents #
##############################################
common_words <- freq_1983 %>%
        full_join(freq_1997) %>%
        full_join(freq_2012) %>%
        full_join(freq_2015) %>%
        group_by(term) %>%
        summarize(total = sum(freq)) %>%
        arrange(desc(total))

############################################
# Look at uncommon words between documents #
############################################

# insert the years you want to compare. If you want to compare 1983 to
# 1997 use the default values for x and y. If you want to compare 2012 to
# 2015 then insert `freq_2012` for x and `freq_2015` for y.

x <- freq_1983        # choices: freq_1983, freq_1997, freq_2012, freq_2015
y <- freq_1997        # choices: freq_1983, freq_1997, freq_2012, freq_2015

uncommon_words <- x %>%
        anti_join(y, by = "term") %>%
        arrange(desc(freq))








