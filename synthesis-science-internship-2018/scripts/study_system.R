source("~/Desktop/DataONE/systematic_review/synthesis-science-internship-2018/scripts/clean_data.R")

## Load libraries ####
library (dplyr)
library(ggplot2)
library(ggthemes)

## Clean data ####

# Get only unique IDs and ecosystem  info
names(raw_data)
ecosystem_data <- select(raw_data, unique_ID, study_system)

ecosystem_data_by_article <- distinct(ecosystem_data, unique_ID, .keep_all = TRUE)

## Make figure ####
gg <- ggplot(ecosystem_data_by_article, aes(x = reorder(study_system,study_system, function(x)-length(x))))
gg <-gg + geom_bar(stat="count", fill = "#F8C471")
gg
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Spatial Scale")
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20))
gg <- gg + geom_vline(aes(xintercept= 2.5), colour="#D35400", size = 1, linetype="dashed")

gg

# To access the data for each bar in barplot
count(ecosystem_data_by_article, study_system)
