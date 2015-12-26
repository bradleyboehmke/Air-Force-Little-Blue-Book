# This script plots for comparison the use of the directive term "you" versus
# the inclusive term "our" across the four Air Force Little Blue Book documents

library(magrittr)
library(ggplot2)

# Vectors consisting of the ratio of 'you' vs. 'our' per 100 words in each document.
# These values were calculated in the "Basic Stuff XXXX.R" scripts
v1983 <- c(2.11, 0.07)
v1997 <- c(0.48, 0.75)
v2012 <- c(0.70, 0.27)
v2015 <- c(0.09, 2.84)
my.m <- rbind(v1983, v1997, v2012, v2015) %>%
        as.data.frame() %>%
        set_colnames(c("you", "our"))

ggplot(my.m, aes(you, our, label = substring(rownames(my.m), first = 2))) +
        geom_point(size=3) +
        theme_bw() +
        xlab("Directive: 'You'/per 100 words") +
        ylab("Inclusive: 'Our'/per 100 words") +
        theme(axis.text = element_text(size = 12)) +
        scale_x_continuous(limits = c(0,3), breaks = seq(0, 3, by=0.5)) +
        scale_y_continuous(limits = c(0,3),  breaks = seq(0, 3, by=0.5)) +
        geom_text(hjust = -.25, vjust = 0)