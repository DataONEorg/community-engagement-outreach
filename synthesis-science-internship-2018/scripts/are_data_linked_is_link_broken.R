source("~/Desktop/DataONE/systematic_review/synthesis-science-internship-2018/scripts/clean_data.R")

## Load libraries ####
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(scales)

## Clean data ####
names(raw_data)

# Get only unique IDs and spatial extent info
link_information <- select(raw_data, unique_ID, pub_year, input_how_to_access, input_link_to_data)

link_information_by_year_count <- count(link_information, pub_year, input_how_to_access) %>% # count function has to come BEFORE complete
  as.data.frame() %>%
  na.omit()
head(link_information_by_year_count)

# Now, select only rows with data on link (broken) and link (working)
link_work_and_broken_only <- filter(link_information_by_year_count, input_how_to_access == "link (broken)" | input_how_to_access == "link (working)")
link_work_only <- filter(link_information_by_year_count, input_how_to_access == "link (working)")
link_work_only$prop <- c(.82,.8,.93)
head(link_work_only)

gg <- ggplot() + geom_bar(aes(y = prop, x = pub_year), data = link_work_only,
                          stat="identity", fill = "#1D8348")
gg <- gg + theme_tufte()
gg
gg <- gg + ylab("Proportion working links")
gg <- gg + xlab("Year")
gg <- gg + theme(legend.title=element_blank()) # Remove legend title
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20))
gg

# Get the ratios of broken to working by year
link_work_and_broken_only
