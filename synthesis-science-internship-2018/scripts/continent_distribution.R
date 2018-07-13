#setwd to github project repository

#source("~/synthesis-science-internship-2018/scripts/clean_data.R")
#source("~/Desktop/DataONE/systematic_review/synthesis-science-internship-2018/scripts/clean_data.R")

#should automatically put you in your GitHub dataone ceo project folder/repository, if not, set working directory to the github project repository
#Or, you could decide you want to have the subfolder "synthesis-science-internship-2018" be the folder you work from, for less text to type up and potentially screw up. Could set to that folder as wd.

#if you use the main repository
source("./synthesis-science-internship-2018/scripts/clean_data.R")

#if you decide to use the subfolder as the main umbrella
setwd("./synthesis-science-internship-2018")
source("./scripts/clean_data.R")

#will now have your most up to date csv loaded

## Load libraries ####
library(ggplot2)
library(ggthemes)
library(viridis)
library(rgdal)
library(rgeos)
library(httr)

## Clean data ####
# Subset only the columns we need for this analysis
names(raw_data)
continent_data <- select(raw_data, unique_ID, continent)

# make sure we only have one row for each article
continent_data_by_article <- distinct(continent_data, unique_ID, .keep_all = TRUE)

# Make sure there are no mis-spelled continents
unique(continent_data_by_article$continent)

# Make a table of continent data
continents_data_df <- plyr::count(continent_data_by_article, "continent") %>%
  rename(id = continent, value = freq) %>%
  na.omit() %>% # Remove instances where continents were not indicated, r can't make map with these
  filter(!id == "Global")

continents_data_df # check to make sure df looks OK

# Get continent map data
url <- "https://gist.githubusercontent.com/hrbrmstr/91ea5cc9474286c72838/raw/f3fde312c9b816dff3994f39f2bcda03209eff8f/continents.json"
stop_for_status(GET(url, write_disk("continents.json")))
continents <- readOGR("continents.json", "OGRGeoJSON")
continents_map <- fortify(continents, region="CONTINENT")
unique(continents_map$id)

## Create map ####
gg <- ggplot()
gg <- gg + geom_map(data=continents_map,
                    map=continents_map,
                    aes(x=long, y=lat, map_id=id),
                    color="black")

gg <- gg + geom_map(data=continents_data_df,
                    map=continents_map,
                    aes(map_id=id, fill=value),
                    color="black")

gg

gg <- gg + scale_fill_distiller(palette = "Spectral") # needs latest ggplot2
gg <- gg + coord_equal()
gg <- gg + theme_bw()
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(panel.grid=element_blank())
gg

## Create barplot ####

gg <- ggplot(continent_data_by_article, aes(x = reorder(continent,continent, function(x)-length(x))))
gg <-gg + geom_bar(stat="count", fill = "#1f78b4")
gg
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Continent")
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20))
count(continent_data_by_article, continent)
  