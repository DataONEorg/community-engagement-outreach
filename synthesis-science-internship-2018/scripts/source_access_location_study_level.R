setwd('C:\\Users\\Giancarlo\\Documents\\community-engagement-outreach\\synthesis-science-internship-2018\\') #gs wd [ignore]

source("scripts/clean_data.R")

# a function to derive multiple summary stats for tables
summStats = function(x) {out=c(length(x),mean(x),sd(x),min(x),max(x));names(out)=c('n','mean','sd','min','max');return(out)}
#summStats = function(x) {c(mean(x),sd(x),min(x),max(x))}

# a custom aggregate function which combines a simple one-variable, one-group aggregation 
# (using whichever function is provided) with results from pooled groups (default == T).
custAgg = function(form,data,func,pooled=T){ 
  agg = aggregate(form,data,func)
  if(pooled==T){
    datCol = as.character(formula(form))[2]
    datGroup = as.character(formula(form))[3]
    agg = rbind(agg,rep(NA,length(levels(datGroup))+1))
    rowsAgg = nrow(agg)
    agg[rowsAgg,datGroup] = "Pooled"
    agg[,datCol][rowsAgg,] = func(data[,datCol])
  }
  return(agg)
}


tempDat = raw_data[raw_data$gt1ExceptionRule=='unique',]
tempDat$datDist1 = ifelse(tempDat$input_data_distance==1,1,0)
tempDat$datDist2 = ifelse(tempDat$input_data_distance==1 | tempDat$input_data_distance==2,1,0)
tempDat$datDist3 = ifelse(tempDat$input_data_distance==1 | tempDat$input_data_distance==2 | tempDat$input_data_distance==3,1,0)

dataAcc = data.frame(aggregate(datDist1~unique_ID+search_where+pub_year,tempDat,FUN=mean),
                     datDist2 = aggregate(datDist2~unique_ID+search_where,tempDat,FUN=mean)[,3],
                     datDist3 = aggregate(datDist3~unique_ID+search_where,tempDat,FUN=mean)[,3],
                     sourceCt = aggregate(datDist3~unique_ID+search_where,tempDat,FUN=length)[,3])
dataAccNCEAS = dataAcc[dataAcc$search_where=="NCEAS",]
dataAccWOS = dataAcc[dataAcc$search_where=="WOS",]


#4a. accessibility

#first, get numbers of sources per synthesis
write.csv(custAgg(sourceCt~search_where,dataAcc,summStats),'clipboard')
write.csv(custAgg(sourceCt~pub_year,dataAcc,summStats),'clipboard')
cor.test(dataAcc$pub_year,dataAcc$sourceCt)
#test these numbers
wilcox.test(sourceCt~search_where,dataAcc)
kruskal.test(sourceCt~pub_year,dataAcc)

#repeatability at varying data distance thresholds
aggregate(I(datDist1==1)~search_where,dataAcc,FUN=mean)
fisher.test(table(dataAcc$datDist1==1,dataAcc$search_where))
aggregate(I(datDist1==1 | datDist2==1)~search_where,dataAcc,FUN=mean)
fisher.test(table(dataAcc$datDist1==1 | dataAcc$datDist2==1,dataAcc$search_where))
aggregate(I(datDist1==1 | datDist2==1 | datDist3==1 )~search_where,dataAcc,FUN=mean)
fisher.test(table(dataAcc$datDist1==1 | dataAcc$datDist2==1 |dataAcc$datDist3==1,dataAcc$search_where))

#examine NCEAS vs. WOS, write to table
write.csv(custAgg(datDist1~search_where,dataAcc,summStats),'clipboard')
write.csv(custAgg(datDist2~search_where,dataAcc,summStats),'clipboard')
write.csv(custAgg(datDist3~search_where,dataAcc,summStats),'clipboard')

#testing
wilcox.test(datDist1~search_where,dataAcc)
wilcox.test(datDist2~search_where,dataAcc)
wilcox.test(datDist3~search_where,dataAcc)

#examine data distance patterns across years

#----accessibility of data sources (by study) across years----

library(coin)

#table of search vs. year
tempTab = table(dataAcc[,c('pub_year','search_where')])
chisq.test(tempTab)
prop.table(tempTab)
par(mar=c(4,4,1,3))
spineplot(tempTab,xlab='Publication year',ylab='Search type')
lbl_test(tempTab,distribution = 'approximate')



#examine accessibility by year across three levels of data distance, write to table
write.csv(custAgg(datDist1~pub_year,dataAcc,summStats),'clipboard')
write.csv(custAgg(datDist2~pub_year,dataAcc,summStats),'clipboard')
write.csv(custAgg(datDist3~pub_year,dataAcc,summStats),'clipboard')

require(clinfun)
#Pooled NCEAS and WOS
#plotting potential differences and trends
boxplot(datDist1~pub_year,dataAcc)
boxplot(datDist2~pub_year,dataAcc)
boxplot(datDist3~pub_year,dataAcc)
#testing for a positive trend in accessibility 
jonckheere.test(dataAcc$datDist1,dataAcc$pub_year, alternative = 'increasing',nperm=2000)
jonckheere.test(dataAcc$datDist2,dataAcc$pub_year, alternative = 'increasing',nperm=2000) #appears primarily driven by NCEAS papers
jonckheere.test(dataAcc$datDist3,dataAcc$pub_year, alternative = 'increasing',nperm=2000)
#testing for differences between years
kruskal.test(dataAcc$datDist1,dataAcc$pub_year)
kruskal.test(dataAcc$datDist2,dataAcc$pub_year)
kruskal.test(dataAcc$datDist3,dataAcc$pub_year)

#NCEAS only
#plotting potential trends
boxplot(datDist1~pub_year,dataAccNCEAS)
boxplot(datDist2~pub_year,dataAccNCEAS)
boxplot(datDist3~pub_year,dataAccNCEAS)
#testing for a positive trend in accessibility
jonckheere.test(dataAccNCEAS$datDist1,dataAccNCEAS$pub_year, alternative = 'increasing',nperm=2000)
jonckheere.test(dataAccNCEAS$datDist2,dataAccNCEAS$pub_year, alternative = 'increasing',nperm=2000)
jonckheere.test(dataAccNCEAS$datDist3,dataAccNCEAS$pub_year, alternative = 'increasing',nperm=2000)
#testing for differences between years
kruskal.test(dataAccNCEAS$datDist1,dataAccNCEAS$pub_year)
kruskal.test(dataAccNCEAS$datDist2,dataAccNCEAS$pub_year)
kruskal.test(dataAccNCEAS$datDist3,dataAccNCEAS$pub_year)

#WOS only
#plotting potential trends
boxplot(datDist1~pub_year,dataAccWOS)
boxplot(datDist2~pub_year,dataAccWOS)
boxplot(datDist3~pub_year,dataAccWOS)
#testing for a positive trend in accessibility
jonckheere.test(dataAccWOS$datDist1,dataAccWOS$pub_year, alternative = 'increasing',nperm=2000)
jonckheere.test(dataAccWOS$datDist2,dataAccWOS$pub_year, alternative = 'increasing',nperm=2000)
jonckheere.test(dataAccWOS$datDist3,dataAccWOS$pub_year, alternative = 'increasing',nperm=2000)
#testing for differences between years
kruskal.test(dataAccWOS$datDist1,dataAccWOS$pub_year)
kruskal.test(dataAccWOS$datDist2,dataAccWOS$pub_year)
kruskal.test(dataAccWOS$datDist3,dataAccWOS$pub_year)

#----repeatability of studies across years----

#repeatability at varying data distance thresholds by year (pooled NCEAS and WOS)
library(coin)
#data distance == 1
tempTab = table(dataAcc$pub_year,dataAcc$datDist1==1)
prop.table(tempTab)
spineplot(tempTab)
lbl_test(tempTab,alternative = "less",distribution = 'approximate')
#data distance <= 2
tempTab = table(dataAcc$pub_year,dataAcc$datDist1==1 | dataAcc$datDist2==1)
prop.table(tempTab)
spineplot(tempTab)
lbl_test(tempTab,alternative = "less",distribution = 'approximate')
#data distance <= 3
tempTab = table(dataAcc$pub_year,dataAcc$datDist1==1 | dataAcc$datDist2==1 |dataAcc$datDist3==1)
prop.table(tempTab)
spineplot(tempTab)
lbl_test(tempTab,alternative = "less",distribution = 'approximate')

#repeatability at varying data distance thresholds by year (NCEAS only)
#data distance == 1
tempTab = table(dataAccNCEAS$pub_year,dataAccNCEAS$datDist1==1)
prop.table(tempTab)
spineplot(tempTab)
lbl_test(tempTab,alternative = "less",distribution = 'approximate')
#data distance <= 2
tempTab = table(dataAccNCEAS$pub_year,dataAccNCEAS$datDist1==1 | dataAccNCEAS$datDist2==1)
prop.table(tempTab)
spineplot(tempTab)
lbl_test(tempTab,alternative = "less",distribution = 'approximate')
#data distance <= 3
tempTab = table(dataAccNCEAS$pub_year,dataAccNCEAS$datDist1==1 | dataAccNCEAS$datDist2==1 |dataAccNCEAS$datDist3==1)
prop.table(tempTab)
spineplot(tempTab)
lbl_test(tempTab,alternative = "less",distribution = 'approximate')

#repeatability at varying data distance thresholds by year (WOS only)
#data distance == 1
tempTab = table(dataAccWOS$pub_year,dataAccWOS$datDist1==1)
prop.table(tempTab)
spineplot(tempTab)
lbl_test(tempTab,alternative = "less")
#data distance <= 2
tempTab = table(dataAccWOS$pub_year,dataAccWOS$datDist1==1 | dataAccWOS$datDist2==1)
prop.table(tempTab)
spineplot(tempTab)
lbl_test(tempTab,alternative = "less",distribution = 'approximate')
#data distance <= 3
tempTab = table(dataAccWOS$pub_year,dataAccWOS$datDist1==1 | dataAccWOS$datDist2==1 |dataAccWOS$datDist3==1)
prop.table(tempTab)
spineplot(tempTab)
lbl_test(tempTab,alternative = "less")

#----data source locations by study----

tempDat = raw_data[raw_data$gt1ExceptionRule=='unique',]
length(unique(tempDat$unique_ID))
tempDat$sourceRepoRepo = ifelse(tempDat$input_storage_location=='gov_repo' |
                                 tempDat$input_storage_location=='ngo_repo' |
                                 tempDat$input_storage_location=='repo', 1, 0)
tempDat$sourceRepoRepoweb = ifelse(tempDat$input_storage_location=='gov_repo' |
                                 tempDat$input_storage_location=='ngo_repo' |
                                 tempDat$input_storage_location=='repo' | 
                                   tempDat$input_storage_location=='website', 1, 0)
tempDat$sourceRepoRepo[is.na(tempDat$sourceRepoRepo)] = 0; summary(tempDat$sourceRepoRepo)
tempDat$sourceRepoRepoweb[is.na(tempDat$sourceRepoRepoweb)] = 0; summary(tempDat$sourceRepoRepoweb)

dataSourceRepo = data.frame(aggregate(sourceRepoRepo~unique_ID+search_where+pub_year,tempDat,FUN=mean),
                            sourceRepoRepoweb = aggregate(sourceRepoRepoweb~unique_ID+pub_year,tempDat,FUN=mean)[,3],
                     sourceCt = aggregate(sourceRepoRepoweb~unique_ID+pub_year,tempDat,FUN=length)[,3])

nrow(dataSourceRepo)

dataSourceRepoNCEAS = dataSourceRepo[dataSourceRepo$search_where=="NCEAS",]
dataSourceRepoWOS = dataSourceRepo[dataSourceRepo$search_where=="WOS",]

write.csv(custAgg(sourceRepoRepo~search_where,dataSourceRepo,summStats),'clipboard')
write.csv(custAgg(sourceRepoRepoweb~search_where,dataSourceRepo,summStats),'clipboard')

#testing repository use among papers, NCEAS vs WOS
wilcox.test(sourceRepoRepo~search_where,dataSourceRepo,alternative = "greater")
wilcox.test(sourceRepoRepoweb~search_where,dataSourceRepo,alternative = "greater")

#table of repository use among papers, across years
write.csv(custAgg(sourceRepoRepo~pub_year,dataSourceRepo,summStats),'clipboard')
write.csv(custAgg(sourceRepoRepoweb~pub_year,dataSourceRepo,summStats),'clipboard')

require(clinfun)
#Pooled NCEAS and WOS
#plotting potential differences and trends
boxplot(sourceRepoRepo~pub_year,dataSourceRepo)
boxplot(sourceRepoRepoweb~pub_year,dataSourceRepo)
#testing for a positive trend in accessibility 
jonckheere.test(dataSourceRepo$sourceRepoRepo,dataSourceRepo$pub_year, alternative = 'increasing',nperm=2000)
jonckheere.test(dataSourceRepo$sourceRepoRepoweb,dataSourceRepo$pub_year, alternative = 'increasing',nperm=2000) #appears primarily driven by NCEAS papers

cor.test(dataSourceRepo$sourceRepoRepo,dataSourceRepo$pub_year, alternative = 'greater',method='spearman',nperm=2000)
cor.test(dataSourceRepo$sourceRepoRepoweb,dataSourceRepo$pub_year,alternative = 'greater',method='spearman',nperm=2000) #appears primarily driven by NCEAS papers

table(tempDat$input_storage_location)


