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



#### Does NCEAS have more data sources in their studies? ####
# https://uc-r.github.io/t_test Two-sample ttest comparing the means of number of source papers in NCEAS studies vs. WOS
wheretest<- select(raw_data, unique_ID, search_where)
wheretable<-table(raw_data$search_where,raw_data$unique_ID)

df.where <- as.data.frame(wheretable)
df.where<-df.where[df.where$Freq!=0,] #remove 0s
summary (df.where %>% filter (Var1=="NCEAS") %>% .$Freq)
summary (df.where %>% filter (Var1=="WOS") %>% .$Freq)

df.where$logFreq<-log(df.where$Freq)

library(gridExtra) 
ggplot(df.where, aes(Var1, logFreq))+
  geom_boxplot()

p1 <- ggplot(df.where, aes(Freq)) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ Var1)

p2 <- ggplot(df.where, aes(Freq)) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ Var1) +
  scale_x_log10()

grid.arrange(p1, p2, nrow = 2)

#performe ttest with log due to non-normal distribution

t.test(log(Freq)~Var1, data=df.where)
wilcox.test(Freq ~ Var1, data = df.where)