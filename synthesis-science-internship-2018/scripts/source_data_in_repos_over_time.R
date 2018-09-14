#Load Fresh Data
#if you use the main repository
source("./synthesis-science-internship-2018/scripts/clean_data.R")

#if you decide to use the subfolder as the main umbrella
setwd("./synthesis-science-internship-2018")
source("./scripts/clean_data.R")

#will now have your most up to date csv loaded

## Load libraries ####
library(ggplot2)
library(ggthemes)

## Clean data ####
names(raw_data)

Are more repos used over time?

link_information <- select(raw_data, unique_ID, pub_year, input_citation_year, search_where, input_storage_loc_aggregated, input_link_to_data, input_storage_loc_aggregated)
repo_citeyear <- count(link_information, input_citation_year, input_storage_loc_aggregated) %>%
  as.data.frame()

repo_year <- filter(link_information, input_storage_loc_aggregated == "repo")

ggplot(repo_year, aes(x=input_citation_year))+
  geom_bar()

