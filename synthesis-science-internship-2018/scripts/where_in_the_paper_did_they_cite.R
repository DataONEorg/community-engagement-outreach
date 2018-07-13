source("~/Desktop/DataONE/systematic_review/synthesis-science-internship-2018/scripts/clean_data.R")

## Load libraries ####
library (dplyr)
library(ggplot2)
library(ggthemes)

## Clean data ####
names(raw_data)

# Get only unique IDs and spatial extent info
where_did_they_cite_data <- select(raw_data, unique_ID, input_where_did_they_cite_data)

## Make figure ####
gg <- ggplot(where_did_they_cite_data, aes(x = reorder(input_where_did_they_cite_data,input_where_did_they_cite_data, function(x)-length(x))))
gg <-gg + geom_bar(stat="count", fill = "#A569BD")
gg
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Citation location")
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20))

gg

count(where_did_they_cite_data, input_where_did_they_cite_data)
