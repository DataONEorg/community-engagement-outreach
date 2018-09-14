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
data_distance_and_how_to_access <- select(raw_data, unique_ID, input_how_to_access, input_data_distance)
data_distance_and_how_to_access$input_data_distance[data_distance_and_how_to_access$input_data_distance=="not accessible"] <- 4
data_distance_and_how_to_access$input_data_distance[data_distance_and_how_to_access$input_data_distance=="not found"] <- 4
data_distance_and_how_to_access$input_data_distance <- as.integer(data_distance_and_how_to_access$input_data_distance)

# Get averages by input_how_to_access
# replace any not accessible with a 4
data_distance_and_how_to_access
unique(data_distance_and_how_to_access$input_data_distance)

data_distance_and_how_to_access %>%
  group_by(input_how_to_access) %>%
  dplyr::summarize(Mean = mean(input_data_distance, na.rm = TRUE))

# Get averages by input_storage_loc_aggregated
# tells us where input data were stored

xy<-table(raw_data$input_storage_loc_aggregated,raw_data$search_where)
xy <- as.data.frame(xy)

xy$PerFreq <- c(0.440233236,
0.075801749,
0.023323615,
0.256559767,
0,
0,
0.075801749,
0.128279883,
0.142857143,
0.180722892,
0.006024096,
0.253012048,
0.180722892,
0.012048193,
0.13253012,
0.234939759,
3.361445783)




#Results -> NCEAS and WOS use the same number of data sources

#xy$rel.freq <- xy$Freq / aggregate(Freq ~ Var2, FUN = sum, data = xy)$Freq
#xy$Var1 <- factor(xy$Var1, levels = xy$Var1[order(-xy$rel.freq)])
#gg <- ggplot(xy, aes(x = reorder(Var1, -rel.freq), y= rel.freq, fill=Var2)) +


#### Make figure ####
storage <- ggplot(xy, aes(x = Var1, y= Freq, fill=Var2)) +
  geom_bar(stat="identity", position ="dodge")+
  coord_flip() +
  scale_x_discrete(limits = rev(levels(xy$Var1)))+
  scale_fill_brewer(palette = "Set2",name="Database") +
  xlab("Storage Location") + 
  ylab(NA) +
#  ggtitle("Data Management Training Effect") +
  theme_bw()+
  theme(legend.position = c(.8, .2), 
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(size=20))
#  theme(legend.position = c(.8, .3), 
#        legend.title=element_blank(), 
#        axis.text.x = element_text(size=13, vjust = .5),
#        axis.text.y = element_text(size=13),
#        axis.title = element_text(size=15))
storage








gg <- gg + theme_tufte()
gg <- gg + ylab("Count")
gg <- gg + xlab("Data Storage Location")
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                   axis.text.y = element_text(size=15),
                   axis.title = element_text(size=20))
gg

gg <- ggplot(data_distance_and_how_to_access, aes(x = input_how_to_access, y = input_data_distance)) +
  geom_jitter(alpha = 0.6, aes(color = input_how_to_access))
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Study System")
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20),
                 legend.position = "NONE")
gg

