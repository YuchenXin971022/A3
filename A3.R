library(tidyverse)
library(tidyr) 
library(dplyr)
library(ggplot2)
library(car)
library(knitr)
library(devtools)


# Exercise 1 
population=read.csv("Users/Desktop/2021 Spring/Econ613/homework/population.csv")
crime_long=read.csv("sers/Desktop/2021 Spring/Econ613/homework/crime_long.csv")
officers=read.csv("sers/Desktop/2021 Spring/Econ613/homework/officers.csv")


## Exercise 2
crime2<-aggregate(crime_long$crimes, by=list(month=crime_long$crime_month), FUN=sum)
colnames(crime2)<-c("month","totalcrime")
head(crime2)

ggplot(crime2, aes(x = month, y = totalcrime, group = 1))+geom_line()


plot2<-left_join(population,crime_long, by=c("month"="crime_month","district"="district"))
head(plot2)

plot2n<-plot2 %>% group_by(month,district, crime_type) %>% 
  summarise(total_crime =sum(crimes))
head(plot2n)

plot2n<-spread(e2n, key = crime_type, value = total_crime)
head(e2n)

plot2n2<-plot2 %>% group_by(month,district) %>% 
  summarise(total_crime =sum(crimes))
head(plot2n2)

plot2n1<-plot2 %>% group_by(month,district) %>% 
  summarise(median_income=median(p50_inc),black_share=sum(tot_black)/sum(tot_pop),hispanic_share=sum(tot_hisp)/sum(tot_pop),white_share=sum(tot_white)/sum(tot_pop))
head(plot2n1)

panel<-left_join(plot2n,plot2n1, by=c("month"="month","district"="district"))
panel<-left_join(panel,plot2n2, by=c("month"="month","district"="district"))
panel<-subset(panel, select=-c(other,drug))
panel<-subset(panel, select=-c(5))

## Exercise 3
panel3<-left_join(panel,officers,by=c("month"="month","district"="unit"))
panel3<-drop_na(panel3)
head(panel3)

model3<-lm(arrest ~tenure+total_crime+median_income+black_share+hispanic_share+white_share,data=panel3)
summary(model3)

## Exercise 4
model4<-lm(arrest ~tenure+total_crime+median_income+black_share+hispanic_share+white_share+c(month)+factor(district),data=panel3,x=FALSE)
summary(model4)

## Exercise 5
#1. consider within estimator

panel4<-panel3 %>%
  group_by(NUID) %>%
  summarise(meanarrest=mean(arrest),meantenure=mean(tenure),meantotal_crime=mean(total_crime),
            meanmed=mean(median_income), meanblack=mean(black_share),meanhis=mean(hispanic_share),meanwhite=mean(white_share))
head(panel4)

between<-lm(meanarrest ~meantenure+meantotal_crime+meanmed+meanblack+meanhis+meanwhite,data=panel4)
summary(between)


#consider within estimator

panel5<-left_join(panel4,panel3,by=c("NUID"="NUID"))

panel5 <-panel5 %>%
  mutate(arrest=arrest-meanarrest,tenure=tenure-meantenure,black=black_share-meanblack,white=white_share-meanwhite,hispanic=hispanic_share-meanhis,median_income=median_income-meanmed)

within<-lm(arrest~tenure+total_crime+median_income+black_share+hispanic_share+white_share,panel5)
summary(within)


#consider first difference estimator

panel6<-panel3 %>%
  group_by(NUID) %>%
  mutate(beforetenure = lag(tenure),beforearrest=lag(arrest),before_crime=lag(total_crime),before_median=lag(median_income),before_black=lag(black_share),before_white=lag(white_share),before_his=lag(hispanic_share),order_by=month)
panel6 <-panel6 %>%
  mutate(tenurediff=tenure-beforetenure,arrestdiff=arrest-beforearrest,crime=total_crime-before_crime,median=median_income-before_median,black=black_share-before_black,his=hispanic_share-before_his,white=white_share-before_white)
panel6<-drop_na(panel6)

firstd<-lm(arrestdiff~tenurediff+median+crime+median+black+his+white,panel6)
summary(firstd)

#Comparing these three results, the estimator for beta in within and between are quite similiar, while coeffcients for other varibales are quite different. While first difference estimator has a large diff. we can see that the first difference estimator can not handle time fixed effects, the failure for considering time also exists for withiin and between estimator. Also, first difference lack cross section information and fixed effects(since all of them are offset). 

#using GMM in one step:
  #consider dataset, we should use panel3 
#the computation eats all my memory and I coould not get the result at all.

phi=length(unique(panel3$district))
chi=length(unique(panel3$month))
alpha=length(unique(panel3$NUID))
beta=0
gamma=5

#initialize the coefficient&create intercept

panel3$intercept=1
alpha1<-rep(0,alpha)

library(fastDummies)
library(dummies)
X=panel3
X<-dummy_cols(X,select=c("month","district"))
y=panel3$arrest

library(hash)

h <- hash()
individual<-as.list(unique(X$NUID))

coeff<-rep(1,1+1+5+phi+chi)


#consider function (my pc cannot handle this computation)


result5<-optim(runif(10,-0.1,0.1),gmm,method="BFGS")