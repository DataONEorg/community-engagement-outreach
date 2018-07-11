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

library(ordinal)
library(lme4)

modDat = raw_data[raw_data$gt1ExceptionRule=='unique',]
modDat$input_data_distance_num = as.numeric(ifelse(modDat$input_data_distance=="not accessible" | modDat$input_data_distance=="not found" ,4,modDat$input_data_distance))

# make an ordinal data_distance response with four levels
modDat$input_data_distance_ord = factor(modDat$input_data_distance_num,levels=1:4,ordered=T)
table(modDat$input_data_distance_ord)

# make an ordinal data_distance response with three levels (combine 1 & 2)
modDat$input_data_distance_ord3cat_a = factor(ifelse(modDat$input_data_distance_num<=2,1,modDat$input_data_distance_num-1),levels=1:3,ordered=T)
table(modDat$input_data_distance_ord3cat_a)

# make an ordinal data_distance response with three levels (combine 3 & 4)
modDat$input_data_distance_ord3cat_b = factor(ifelse(modDat$input_data_distance_num==4,3,modDat$input_data_distance_num),levels=1:3,ordered=T)
table(modDat$input_data_distance_ord3cat_b)


table(modDat$study_system )
modDat$study_system_binary = ifelse(modDat$study_system=='marine','marine','terrestrial')
table(modDat$study_system_binary)
table(modDat$input_data_form,modDat$input_data_distance_ord)
modDat$input_data_form_reclass = with(modDat,ifelse(
  input_data_form=="[media sources]" | input_data_form=="unknown" | input_data_form=="person" |  input_data_form=="thesis","other",input_data_form
));table(modDat$input_data_form_reclass)

table(modDat$input_data_distance_ord)
table(modDat$spatial_scale)
table(modDat$continent)
table(modDat$input_data_form)
table(modDat$input_storage_location)

modCLMM = clmm(input_data_distance_ord~
             study_system_binary+
             input_data_form_reclass+
             #factor(pub_year)+
             I(pub_year-2016)+search_where+spatial_scale+
             (1|unique_ID)+(1|continent), 
           data=modDat, Hess=T)#, nAGQ=17)
summary(modCLMM)
plot(mod)

modCLMM = clmm(input_data_distance_ord3cat_a~
                 #study_system_binary+
                 #input_data_form_reclass+
                 #factor(pub_year)+
                 factor(pub_year)+search_where+
                 #I(pub_year-2016)*search_where+
                 #random effects
                 (1|unique_ID)+     #random effect to control for within-aggregation correlation and 'observer effects'
                 (1|spatial_scale)+ #spatial scale [5 levels]
                 (1|continent)+     #continent [9 levels]
                 (1|study_system) + #study systems [10 levels]
                 (1|input_data_form)+    #data form [8 levels]              
                 (1|input_storage_location), #storage location [12 levels]
               data=modDat, Hess=T)#, nAGQ=17)
summary(modCLMM)
plot(mod)
library(car)
library(RVAideMemoire)

Anova(modCLMM,
      type = "II")

library(lsmeans)

marginal = lsmeans(modCLMM,
                   pairwise ~ factor(pub_year)+search_where,
                   adjust="tukey")        ### Tukey-adjusted comparisons
marginal
cld(marginal,
    alpha   = 0.05,
    Letters = letters,       ### Use lower-case letters for .group
    adjust  = "tukey")       ### Tukey-adjusted comparisons  

aggregate(input_data_distance_num~pub_year+search_where,modDat,FUN=mean)
aggregate(input_data_distance_num~pub_year+search_where,modDat,FUN=mean)
aggregate(I(input_data_distance_num==1)~pub_year+search_where,modDat,FUN=mean)
aggregate(I(input_data_distance_num<=2)~pub_year+search_where,modDat,FUN=mean)

modGLMM = glmer(I(input_data_distance_num==1)~
                  #study_system_binary+
                  #input_data_form_reclass+
                  #factor(pub_year)+
                  factor(pub_year-2016)*search_where+
                  #random effects
                  (1|unique_ID)+     #random effect to control for within-aggregation correlation and 'observer effects'
                  (1|spatial_scale)+ #spatial scale [5 levels]
                  (1|continent)+     #
                  (1|study_system) + #study systems [10 levels]
                  (1|input_data_form)+    #data form [8 levels]              
                  (1|input_storage_location), #storage location [12 levels]
                  data=modDat,family=binomial)#, nAGQ=17)
summary(modGLMM)
plot(modGLMM)


table()
modGLMM = glmer(I(input_data_distance_num<=2)~
                  study_system_binary+
                  #input_data_form_reclass+
                  #factor(pub_year)+
                  #factor(pub_year-2016)+search_where+
                  I(pub_year-2016)+search_where+
                  #random effects
                  (1|unique_ID)+     #random effect to control for within-aggregation correlation and 'observer effects'
                  (1|spatial_scale)+ #spatial scale [5 levels]
                  (1|continent)+     #continent [9 levels]
                  (1|input_data_form)+    #data form [8 levels]              
                  (1|input_storage_location), #storage location [12 levels]
                data=modDat,family=binomial)#, nAGQ=17)
summary(modGLMM)
plot(modGLMM)

# binary mixed effects model 1+2 vs. 3+4
modGLMM = glmer(I(input_data_distance_num==1)~
                  #study_system_binary+
                  #input_data_form_reclass+
                  #factor(pub_year)+
                  factor(pub_year)*search_where+
                  #random effects
                  (1|unique_ID)+     #random effect to control for within-aggregation correlation and 'observer effects'
                  (1|spatial_scale)+ #spatial scale [5 levels]
                  (1|continent)+     #
                  (1|study_system) + #study systems [10 levels]
                  (1|input_data_form)+    #data form [8 levels]              
                  (1|input_storage_location), #storage location [12 levels]
                data=modDat,family=binomial)#, nAGQ=17)
summary(modGLMM)
plot(modGLMM)

Anova(modGLMM,
      type = "II")

library(lsmeans)

marginal = lsmeans(modGLMM,
                   pairwise ~ factor(pub_year)+search_where,
                   adjust="tukey")        ### Tukey-adjusted comparisons
marginal
cld(marginal,
    alpha   = 0.05,
    Letters = letters,       ### Use lower-case letters for .group
    adjust  = "tukey")       ### Tukey-adjusted comparisons

# binary mixed effects model 1+2 vs. 3+4
modGLMM = glmer(I(input_data_distance_num<=2)~
                  #study_system_binary+
                  #input_data_form_reclass+
                  #factor(pub_year)+
                  factor(pub_year)*search_where+
                  #random effects
                  (1|unique_ID)+     #random effect to control for within-aggregation correlation and 'observer effects'
                  (1|spatial_scale)+ #spatial scale [5 levels]
                  (1|continent)+     #
                  (1|study_system) + #study systems [10 levels]
                  (1|input_data_form)+    #data form [8 levels]              
                  (1|input_storage_location), #storage location [12 levels]
                data=modDat,family=binomial)#, nAGQ=17)
summary(modGLMM)
plot(modGLMM)

Anova(modGLMM,
      type = "II")

library(lsmeans)

marginal = lsmeans(modGLMM,
                   pairwise ~ factor(pub_year)+search_where,
                   adjust="tukey")        ### Tukey-adjusted comparisons
library(multcomp)
marginal = glht(modGLMM, mcp(myfactor="Tukey"))
marginal = glht(modGLMM, lsm(pairwise ~ factor(pub_year)+search_where))

marginal
cld(marginal,
    alpha   = 0.05,
    Letters = letters,       ### Use lower-case letters for .group
    adjust  = "tukey")       ### Tukey-adjusted comparisons  


# binary mixed effects model 1+2 vs. 3+4
modGLMM = glmer(I(input_data_distance_num<=3)~
                  #study_system_binary+
                  #input_data_form_reclass+
                  #factor(pub_year)+
                  factor(pub_year)*search_where+
                  #random effects
                  (1|unique_ID)+     #random effect to control for within-aggregation correlation and 'observer effects'
                  (1|spatial_scale)+ #spatial scale [5 levels]
                  (1|continent)+     #
                  (1|study_system) + #study systems [10 levels]
                  (1|input_data_form)+    #data form [8 levels]              
                  (1|input_storage_location), #storage location [12 levels]
                data=modDat,family=binomial)#, nAGQ=17)
summary(modGLMM)
plot(modGLMM)

Anova(modGLMM,
      type = "II")

library(lsmeans)

marginal = lsmeans(modGLMM,
                   pairwise ~ factor(pub_year)+search_where,
                   adjust="tukey")        ### Tukey-adjusted comparisons
marginal
cld(marginal,
    alpha   = 0.05,
    Letters = letters,       ### Use lower-case letters for .group
    adjust  = "tukey")       ### Tukey-adjusted comparisons  
