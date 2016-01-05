# This script plots for comparison the use of the directive term "you" versus
# the inclusive term "our" across the four Air Force Little Blue Book documents

library(magrittr)
library(ggplot2)

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


#################################
# assess inclusive vs directive #
#################################
our_vs_you_1983 <- words_1983 %>%
        .[. %in% c("you", "our")] %>%
        table(.) %>%
        as.data.frame() %>%
        set_names(c("term", "freq")) %>%
        mutate(year = 1983, ratio = round(freq / length(words_1983) * 100, 2))

our_vs_you_1997 <- words_1997 %>%
        .[. %in% c("you", "our")] %>%
        table(.) %>%
        as.data.frame() %>%
        set_names(c("term", "freq")) %>%
        mutate(year = 1997, ratio = round(freq / length(words_1997) * 100, 2))

our_vs_you_2012 <- words_2012 %>%
        .[. %in% c("you", "our")] %>%
        table(.) %>%
        as.data.frame() %>%
        set_names(c("term", "freq")) %>%
        mutate(year = 2012, ratio = round(freq / length(words_2012) * 100, 2))

our_vs_you_2015 <- words_2015 %>%
        .[. %in% c("you", "our")] %>%
        table(.) %>%
        as.data.frame() %>%
        set_names(c("term", "freq")) %>%
        mutate(year = 2015, ratio = round(freq / length(words_2015) * 100, 2))

# combine data
our_vs_you_all_yrs <- our_vs_you_1983 %>%
        full_join(our_vs_you_1997) %>%
        full_join(our_vs_you_2012) %>%
        full_join(our_vs_you_2015) %>%
        select(year, term, freq, ratio)

# plot "you" vs "our"
our_vs_you_all_yrs %>%
        select(year, term, ratio) %>%
        spread(term, ratio) %>%
        ggplot(., aes(you, our, label = year)) +
        geom_point(size=3) +
        theme_bw() +
        xlab("Directive: 'You'/per 100 words") +
        ylab("Inclusive: 'Our'/per 100 words") +
        theme(axis.text = element_text(size = 12)) +
        scale_x_continuous(limits = c(0,3), breaks = seq(0, 3, by=0.5)) +
        scale_y_continuous(limits = c(0,3),  breaks = seq(0, 3, by=0.5)) +
        geom_text(hjust = -.25, vjust = 0)
