source("~/Desktop/DataONE/systematic_review/synthesis-science-internship-2018/scripts/clean_data.R")

## Load libraries ####
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

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

## Make stacked barplot for all types of data sources ####

colourCount <- length(unique(link_information_by_year_count$input_how_to_access))
getPalette = colorRampPalette(brewer.pal(9, "Set3"))

gg <- ggplot() + geom_bar(aes(y = n, x = pub_year, fill = input_how_to_access), data = link_information_by_year_count,
                          stat="identity")
gg <- gg + scale_fill_manual(values = getPalette(colourCount))
gg <- gg + theme_tufte()
gg
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Year")
gg <- gg + theme(legend.title=element_blank()) # Remove legend title
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20))
gg

# Make figure for ONLY whether work is broken or working
gg <- ggplot() + geom_bar(aes(y = n, x = pub_year, fill = input_how_to_access), data = link_work_and_broken_only,
                          stat="identity")
gg <- gg + scale_fill_manual(values = c("#F8C471", "#76D7C4"))
gg <- gg + theme_tufte()
gg
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Year")
gg <- gg + theme(legend.title=element_blank()) # Remove legend title
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20))
gg

# Get the ratios of broken to working by year
link_work_and_broken_only
