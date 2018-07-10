source("~/Desktop/DataONE/systematic_review/synthesis-science-internship-2018/scripts/clean_data.R")

## Load libraries ####
library (dplyr)
library(ggplot2)
library(ggthemes)

## Clean data ####
names(raw_data)

# Get only unique IDs and spatial extent info
pub_year_data <- select(raw_data, unique_ID, pub_year)

pub_year_data_by_article <- distinct(pub_year_data, unique_ID, .keep_all = TRUE)
pub_year_data_by_article

count(pub_year_data_by_article, pub_year)

## This probably doesn't need a figure, just a mention in the manuscript