source("~/Desktop/DataONE/systematic_review/synthesis-science-internship-2018/scripts/clean_data.R")

## Load libraries ####
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(scales)

## Clean data ####
names(raw_data)

# Get only unique IDs and spatial extent info
link_information <- select(raw_data, unique_ID, pub_year, search_where, input_how_to_access, input_link_to_data, input_storage_loc_aggregated)

link_information_by_year_count <- count(link_information, pub_year, search_where, input_how_to_access) %>% # count function has to come BEFORE complete
  as.data.frame() %>%
  na.omit()
head(link_information_by_year_count)

#What if we only look at those links to websites.. not include repos
link_information_by_year_count_store <- count(link_information, pub_year, search_where, input_how_to_access, input_storage_loc_aggregated) %>% # count function has to come BEFORE complete
  as.data.frame() %>%
  na.omit()
head(link_information_by_year_count_store)

link_count <- count(link_information,input_link_to_data) %>%
as.data.frame()
                                              
# Now, select only rows with data on link (broken) and link (working)
link_work_and_broken_only <- filter(link_information_by_year_count, input_how_to_access == "link (broken)" | input_how_to_access == "link (working)")
link_work_only <- filter(link_information_by_year_count, input_how_to_access == "link (working)")
link_work_only$propforcohort <- c(82,90,97,67,97,88)
link_work_only$propforyear <- c(67,16,43,37,53,40)
head(link_work_only)


working <- ggplot(link_work_only, aes(x = pub_year, y = propforyear, fill=search_where)) + 
  geom_bar(stat="identity") +
  theme_bw()+
  scale_fill_brewer(palette = "Set2",name="Database") +
  ylab("Percent Working Links")+
  xlab("Year")+
  theme(legend.position="none") +# Remove legend title
  theme(axis.text.x = element_text(size=15, vjust = .5),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=20))
working

# Get the ratios of broken to working by year
link_work_and_broken_only


##ORIGINAL-OLD
broken <- ggplot(link_work_only, aes(x = pub_year, y = prop)) + 
  geom_bar(stat="identity", fill="#8da0cb") +
  theme_bw()+
  #scale_fill_brewer(palette = "Set2",name="Database") +
  ylab("Of 200% working links")+
  xlab("Year")+
  theme(legend.position="none") +# Remove legend title
  theme(axis.text.x = element_text(size=13, vjust = .5),
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=15))
broken
