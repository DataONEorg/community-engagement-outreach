source("~/Desktop/DataONE/systematic_review/synthesis-science-internship-2018/scripts/clean_data.R")

## Load libraries ####
library(ggplot2)
library(ggthemes)
library(cowplot)

## Clean data ####

# Get only unique IDs and ecosystem  info
names(raw_data)
ecosystem_data <- select(raw_data, unique_ID, study_system)

ecosystem_data_by_article <- distinct(ecosystem_data, unique_ID, .keep_all = TRUE)

# Recode a new column so that we either have aquatic or terrestrial study system
ecosystem_data_by_article$aquatic_or_terrestrial <- ecosystem_data_by_article$study_system
head(ecosystem_data_by_article)
ecosystem_data_by_article$aquatic_or_terrestrial[ecosystem_data_by_article$study_system=="marine"] <- "aquatic"
ecosystem_data_by_article$aquatic_or_terrestrial[ecosystem_data_by_article$study_system=="coastal"] <- "aquatic"
ecosystem_data_by_article$aquatic_or_terrestrial[ecosystem_data_by_article$study_system=="wetland"] <- "aquatic"
ecosystem_data_by_article$aquatic_or_terrestrial[ecosystem_data_by_article$study_system=="lotic"] <- "aquatic"
ecosystem_data_by_article$aquatic_or_terrestrial[ecosystem_data_by_article$study_system=="terrestrial"] <- "terrestrial"
ecosystem_data_by_article$aquatic_or_terrestrial[ecosystem_data_by_article$study_system=="forest"] <- "terrestrial"
ecosystem_data_by_article$aquatic_or_terrestrial[ecosystem_data_by_article$study_system=="agriculture"] <- "terrestrial"
ecosystem_data_by_article$aquatic_or_terrestrial[ecosystem_data_by_article$study_system=="polar"] <- "terrestrial"
ecosystem_data_by_article$aquatic_or_terrestrial[ecosystem_data_by_article$study_system=="urban"] <- "terrestrial"
ecosystem_data_by_article$aquatic_or_terrestrial[ecosystem_data_by_article$study_system=="multiple"] <- NA

# Make dataframe w/ only aqautic/terrestrial info
# Remove any rows w/ NA
ecosystem_aquatic_or_terrestrial <- select(ecosystem_data_by_article, unique_ID, aquatic_or_terrestrial) %>%
  na.omit()
  
# Make dataframe w/ only fine grained study system info
ecosystem_data_fine_grained <- ecosystem_data_by_article %>% 
  filter(!grepl('marine', study_system))  %>% 
  filter(!grepl('terrestrial', study_system))

## Make figures ####

# Barplot for broad ecosystem categories
gg2 <- ggplot(ecosystem_aquatic_or_terrestrial, aes(x = reorder(aquatic_or_terrestrial,aquatic_or_terrestrial, function(x)-length(x))))
gg2 <-gg2 + geom_bar(stat="count", fill = "#2980B9")
gg2
gg2 <- gg2 + theme_tufte()
gg2 <- gg2 + ylab("Frequency")
gg2 <- gg2 + xlab("Study System")
gg2 <- gg2 + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20))
gg2


# To access the data for each bar in barplot
count(ecosystem_data_by_article, study_system)
count(ecosystem_aquatic_or_terrestrial, aquatic_or_terrestrial)

# Barplot for fine-grained ecosystem caegories
gg <- ggplot(ecosystem_data_fine_grained, aes(x = reorder(study_system,study_system, function(x)-length(x))))
gg <-gg + geom_bar(stat="count", fill = "#F8C471")
gg
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Study System")
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20))
gg

# Add both plots to same figure
plot_grid(gg2, gg, labels = "AUTO")

