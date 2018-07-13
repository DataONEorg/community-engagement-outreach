source("~/Desktop/DataONE/systematic_review/synthesis-science-internship-2018/scripts/clean_data.R")

## Load libraries ####
library(ggplot2)
library(ggthemes)

## Clean data ####
names(raw_data)
citation_data <- select(raw_data, unique_ID, input_citation_for_data, input_link_to_data)

## Analysis

# Here, we see that there are a total of 558 data sources
dim(citation_data)

# how many of these have an entry in EITHER the citation column or link column
citation_data %>% 
  filter_at(.vars = vars(input_link_to_data, input_citation_for_data), .vars_predicate = any_vars(!is.na(.)))

# Here we see that 464 we at least cited in the form of an actual text citation or a link
