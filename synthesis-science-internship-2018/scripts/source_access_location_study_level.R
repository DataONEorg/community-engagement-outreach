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

#table of search vs. year
chisq.test(table(dataAcc[,c('search_where','pub_year')]))


#examine NCEAS vs. WOS, write to table
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

