
#if you use the main repository
source("./synthesis-science-internship-2018/scripts/clean_data.R")

#if you decide to use the subfolder as the main umbrella
setwd("./synthesis-science-internship-2018")
source("./scripts/clean_data.R")

#will now have your most up to date csv loaded

## Load libraries ####
library (dplyr)
library(ggplot2)
library(ggthemes)

## Clean data ####
names(raw_data)

# Get only unique IDs and spatial extent info
where_did_they_cite_data <- select(raw_data, unique_ID, input_where_did_they_cite_data, search_where)
where_did_they_cite_data$input_where_did_they_cite_data <- as.character(where_did_they_cite_data$input_where_did_they_cite_data)
where_did_they_cite_data$input_where_did_they_cite_data[where_did_they_cite_data$input_where_did_they_cite_data=="table"]<-"methods (table)"
#where_did_they_cite_data$input_where_did_they_cite_data <- as.factor(where_did_they_cite_data$input_where_did_they_cite_data)
where_data<-table(where_did_they_cite_data$input_where_did_they_cite_data,where_did_they_cite_data$search_where)

where_data <- as.data.frame(where_data)


library(cowplot)
## Make figure ####
where <- ggplot(where_data, aes(x = Var1, y=Freq, fill=Var2))+
  geom_bar(stat="identity", position ="dodge")+
  coord_flip()+
  scale_x_discrete(limits = rev(levels(where_data$Var1)))+
  theme_bw()+
  scale_fill_brewer(palette = "Set2")+
  ylab("Count")+
  xlab("Citation Location")+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=15, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20))
where
plot_grid(storage,where, ncol=1, align = 'v', rel_heights=c(1,1.2))
#plot_grid(storage,where,working, ncol=1, align = 'v', rel_heights=c(1,1.2, .7), labels="auto")

count(where_did_they_cite_data, input_where_did_they_cite_data)
