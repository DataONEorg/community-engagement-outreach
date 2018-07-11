source("~/Desktop/DataONE/systematic_review/synthesis-science-internship-2018/scripts/clean_data.R")

## Load libraries ####
library (dplyr)
library(ggplot2)
library(ggthemes)

## Clean data ####
names(raw_data)

# Get only unique IDs and spatial extent info
spatial_extent_data <- select(raw_data, unique_ID, spatial_scale)

spatial_extent_data_by_article <- distinct(spatial_extent_data, unique_ID, .keep_all = TRUE)

## Make figure ####
gg <- ggplot(spatial_extent_data_by_article, aes(x = reorder(spatial_scale,spatial_scale, function(x)-length(x))))
gg <-gg + geom_bar(stat="count")
gg
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Spatial Scale")
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20))

gg

# To get the actual numbers used in the barplot
count(spatial_extent_data_by_article, spatial_scale)
