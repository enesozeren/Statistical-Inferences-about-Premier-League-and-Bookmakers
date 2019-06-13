library(ggplot2)
library(gridExtra)
library(data.table)
library(lubridate)
setwd("/Users/enesozeren/Downloads/")
matches=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds')
odds=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds')

matches[home=="manchester-united"]$home="manchester united"
matches[home=="manchester-utd"]$home="manchester united"
matches[home=="manchester utd"]$home="manchester united"
matches[home=="manchester-city"]$home="manchester city"
matches[home=="newcastle"]$home="newcastle utd"
matches[home=="west-ham"]$home="west ham"
matches[home=="crystal-palace"]$home="crystal palace"
matches[home=="stoke"]$home="stoke city"
sort(unique(matches$home))

matches[away=="manchester-united"]$away="manchester united"
matches[away=="manchester-utd"]$away="manchester united"
matches[away=="manchester utd"]$away="manchester united"
matches[away=="manchester-city"]$away="manchester city"
matches[away=="newcastle"]$away="newcastle utd"
matches[away=="west-ham"]$away="west ham"
matches[away=="crystal-palace"]$away="crystal palace"
matches[away=="stoke"]$away="stoke city"
sort(unique(matches$away))



matches[,c('HomeGoal','AwayGoal'):=tstrsplit(score,':')]
matches[,HomeGoal:=as.numeric(HomeGoal)]
matches[,AwayGoal:=as.numeric(AwayGoal)]
matches[,TotalGoal:=HomeGoal+AwayGoal]

filtered_matches=matches[TotalGoal>=3]

#Bookmaker1
filtered_odds=odds[betType=='ou'& bookmaker=='Pinnacle'& totalhandicap==2.5]
filtered_odds_for_init=filtered_odds[order(matchId,date, decreasing = TRUE)]
filtered_odds=filtered_odds[order(matchId,date)]

initial_odds=filtered_odds_for_init[,list(initial_odd=odd[.N]),by=list(matchId,oddtype)]
initial_odds=dcast(initial_odds,matchId~oddtype,value.var='initial_odd')
latest_odds=filtered_odds[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_odds=dcast(latest_odds,matchId~oddtype,value.var='final_odd')

#Latest Odds for Bookmaker1
latest_odds[,prob_over:=1/over]
latest_odds[,prob_under:=1/under]
Total_odds=latest_odds$prob_over+latest_odds$prob_under

latest_odds[,Total_odds:=latest_odds$prob_over+latest_odds$prob_under]

latest_odds[,P_over:=prob_over/Total_odds]
latest_odds[,P_under:=prob_under/Total_odds]

cut_levels=c(0:20)/20
latest_odds[,diff_bucket:=cut(prob_over,cut_levels)]


temp=matches[,list(matchId,date,TotalGoal)]

latest_odds=merge(temp,latest_odds,by='matchId')

result_summary=latest_odds[,
                           list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]

real_over_r=result_summary$real_over_ratio
b=result_summary$bookmaker_over_ratio

##Ggpolt
ggplot(result_summary,aes(x=b, y=real_over_r))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)+
  ggtitle("Scatter Plot of Latest Over Ratios for Pinnacle")+
  xlab("Bookmaker Over Ratio")+
  ylab("Real Over Ratio")+
  xlim(0,1)+
  ylim(0,1)

#Initial Odds for Bookmaker1
initial_odds[,prob_over:=1/over]
initial_odds[,prob_under:=1/under]
Total_odds=initial_odds$prob_over+initial_odds$prob_under

initial_odds[,Total_odds:=initial_odds$prob_over+initial_odds$prob_under]

initial_odds[,P_over:=prob_over/Total_odds]
initial_odds[,P_under:=prob_under/Total_odds]

cut_levels=c(0:20)/20
initial_odds[,diff_bucket:=cut(prob_over,cut_levels)]


temp=matches[,list(matchId,date,TotalGoal)]

initial_odds=merge(temp,initial_odds,by='matchId')

result_summary=initial_odds[,
                           list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]

real_over_r=result_summary$real_over_ratio
b=result_summary$bookmaker_over_ratio

##Ggpolt
ggplot(result_summary,aes(x=b, y=real_over_r))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)+
  ggtitle("Scatter Plot of Initial Over Ratios for Pinnacle")+
  xlab("Bookmaker Over Ratio")+
  ylab("Real Over Ratio")+
  xlim(0,1)+
  ylim(0,1)






#Bookmaker 2
filtered_odds2=odds[betType=='ou'& bookmaker=='bet365'& totalhandicap==2.5]
filtered_odds_for_init2=filtered_odds2[order(matchId,date, decreasing = TRUE)]
filtered_odds2=filtered_odds2[order(matchId,date)]

initial_odds2=filtered_odds_for_init2[,list(initial_odd=odd[.N]),by=list(matchId,oddtype)]
initial_odds2=dcast(initial_odds2,matchId~oddtype,value.var='initial_odd')
latest_odds2=filtered_odds2[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_odds2=dcast(latest_odds2,matchId~oddtype,value.var='final_odd')

#Latest Odds for Bookmaker2
latest_odds2[,prob_over:=1/over]
latest_odds2[,prob_under:=1/under]
Total_odds2=latest_odds2$prob_over+latest_odds2$prob_under

latest_odds2[,Total_odds2:=latest_odds2$prob_over+latest_odds2$prob_under]

latest_odds2[,P_over:=prob_over/Total_odds2]
latest_odds2[,P_under:=prob_under/Total_odds2]

cut_levels=c(0:20)/20
latest_odds2[,diff_bucket:=cut(prob_over,cut_levels)]


temp2=matches[,list(matchId,date,TotalGoal)]

latest_odds2=merge(temp2,latest_odds2,by='matchId')

result_summary2=latest_odds2[,
                           list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]

real_over_r2=result_summary2$real_over_ratio
b2=result_summary2$bookmaker_over_ratio

##Ggpolt
ggplot(result_summary2,aes(x=b2, y=real_over_r2))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)+
  ggtitle("Scatter Plot of Latest Over Ratios for bet356")+
  xlab("Bookmaker Over Ratio")+
  ylab("Real Over Ratio")+
  xlim(0,1)+
  ylim(0,1)

#Initial Odds for Bookmaker2
initial_odds2[,prob_over:=1/over]
initial_odds2[,prob_under:=1/under]
Total_odds=initial_odds2$prob_over+initial_odds2$prob_under

initial_odds2[,Total_odds:=initial_odds2$prob_over+initial_odds2$prob_under]

initial_odds2[,P_over:=prob_over/Total_odds]
initial_odds2[,P_under:=prob_under/Total_odds]

cut_levels=c(0:20)/20
initial_odds2[,diff_bucket:=cut(prob_over,cut_levels)]


temp=matches[,list(matchId,date,TotalGoal)]

initial_odds2=merge(temp,initial_odds2,by='matchId')

result_summary2=initial_odds2[,
                            list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                 bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]

real_over_r=result_summary2$real_over_ratio
b=result_summary2$bookmaker_over_ratio

##Ggpolt
ggplot(result_summary2,aes(x=b, y=real_over_r))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)+
  ggtitle("Scatter Plot of Initial Over Ratios for bet365")+
  xlab("Bookmaker Over Ratio")+
  ylab("Real Over Ratio")+
  xlim(0,1)+
  ylim(0,1)








#Bookmaker3
filtered_odds3=odds[betType=='ou'& bookmaker=='Betsafe'& totalhandicap==2.5]
filtered_odds_for_init3=filtered_odds3[order(matchId,date, decreasing = TRUE)]
filtered_odds3=filtered_odds3[order(matchId,date)]

initial_odds3=filtered_odds_for_init3[,list(initial_odd=odd[.N]),by=list(matchId,oddtype)]
initial_odds3=dcast(initial_odds3,matchId~oddtype,value.var='initial_odd')
latest_odds3=filtered_odds3[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_odds3=dcast(latest_odds3,matchId~oddtype,value.var='final_odd')


#Latest Odds for Bookmaker3
latest_odds3[,prob_over:=1/over]
latest_odds3[,prob_under:=1/under]
Total_odds3=latest_odds3$prob_over+latest_odds3$prob_under

latest_odds3[,Total_odds3:=latest_odds3$prob_over+latest_odds3$prob_under]

latest_odds3[,P_over:=prob_over/Total_odds3]
latest_odds3[,P_under:=prob_under/Total_odds3]

cut_levels=c(0:20)/20
latest_odds3[,diff_bucket:=cut(prob_over,cut_levels)]


temp3=matches[,list(matchId,date,TotalGoal)]

latest_odds3=merge(temp3,latest_odds3,by='matchId')

result_summary3=latest_odds3[,
                             list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                  bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]

real_over_r3=result_summary3$real_over_ratio
b3=result_summary3$bookmaker_over_ratio

##Ggpolt
ggplot(result_summary3,aes(x=b3, y=real_over_r3))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)+
  ggtitle("Scatter Plot of Latest Over Ratios for Betsafe")+
  xlab("Bookmaker Over Ratio")+
  ylab("Real Over Ratio")+
  xlim(0,1)+
  ylim(0,1)


#Initial Odds for Bookmaker3
initial_odds3[,prob_over:=1/over]
initial_odds3[,prob_under:=1/under]
Total_odds=initial_odds3$prob_over+initial_odds3$prob_under

initial_odds3[,Total_odds:=initial_odds3$prob_over+initial_odds3$prob_under]

initial_odds3[,P_over:=prob_over/Total_odds]
initial_odds3[,P_under:=prob_under/Total_odds]

cut_levels=c(0:20)/20
initial_odds3[,diff_bucket:=cut(prob_over,cut_levels)]


temp=matches[,list(matchId,date,TotalGoal)]

initial_odds3=merge(temp,initial_odds3,by='matchId')

result_summary3=initial_odds3[,
                              list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                   bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]

real_over_r=result_summary3$real_over_ratio
b=result_summary3$bookmaker_over_ratio

##Ggpolt
ggplot(result_summary3,aes(x=b, y=real_over_r))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)+
  ggtitle("Scatter Plot of Initial Over Ratios for BetSafe")+
  xlab("Bookmaker Over Ratio")+
  ylab("Real Over Ratio")+
  xlim(0,1)+
  ylim(0,1)




#Bookmaker 4
filtered_odds4=odds[betType=='ou'& bookmaker=='BetVictor'& totalhandicap==2.5]
filtered_odds_for_init4=filtered_odds4[order(matchId,date, decreasing = TRUE)]
filtered_odds4=filtered_odds4[order(matchId,date)]

initial_odds4=filtered_odds_for_init4[,list(initial_odd=odd[.N]),by=list(matchId,oddtype)]
initial_odds4=dcast(initial_odds4,matchId~oddtype,value.var='initial_odd')
latest_odds4=filtered_odds4[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_odds4=dcast(latest_odds4,matchId~oddtype,value.var='final_odd')


#Latest Odds for Bookmaker4
latest_odds4[,prob_over:=1/over]
latest_odds4[,prob_under:=1/under]
Total_odds4=latest_odds4$prob_over+latest_odds4$prob_under

latest_odds4[,Total_odds4:=latest_odds4$prob_over+latest_odds4$prob_under]

latest_odds4[,P_over:=prob_over/Total_odds4]
latest_odds4[,P_under:=prob_under/Total_odds4]

cut_levels=c(0:20)/20
latest_odds4[,diff_bucket:=cut(prob_over,cut_levels)]


temp4=matches[,list(matchId,date,TotalGoal)]

latest_odds4=merge(temp4,latest_odds4,by='matchId')

result_summary4=latest_odds4[,
                             list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                  bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]

real_over_r4=result_summary4$real_over_ratio
b4=result_summary4$bookmaker_over_ratio

##Ggpolt
ggplot(result_summary4,aes(x=b4, y=real_over_r4))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)+
  ggtitle("Scatter Plot of Latest Over Ratios for BetVictor")+
  xlab("Bookmaker Over Ratio")+
  ylab("Real Over Ratio")+
  xlim(0,1)+
  ylim(0,1)


#Initial Odds for Bookmaker4
initial_odds4[,prob_over:=1/over]
initial_odds4[,prob_under:=1/under]
Total_odds=initial_odds4$prob_over+initial_odds4$prob_under

initial_odds4[,Total_odds:=initial_odds4$prob_over+initial_odds4$prob_under]

initial_odds4[,P_over:=prob_over/Total_odds]
initial_odds4[,P_under:=prob_under/Total_odds]

cut_levels=c(0:20)/20
initial_odds4[,diff_bucket:=cut(prob_over,cut_levels)]


temp=matches[,list(matchId,date,TotalGoal)]

initial_odds4=merge(temp,initial_odds4,by='matchId')

result_summary4=initial_odds4[,
                              list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                   bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]

real_over_r=result_summary4$real_over_ratio
b=result_summary4$bookmaker_over_ratio

##Ggpolt
ggplot(result_summary4,aes(x=b, y=real_over_r))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)+
  ggtitle("Scatter Plot of Initial Over Ratios for BetVictor")+
  xlab("Bookmaker Over Ratio")+
  ylab("Real Over Ratio")+
  xlim(0,1)+
  ylim(0,1)









#3.2
require(lubridate)
matches[,timestamp:=as_datetime(date,tz='Turkey')]
matches[,date_of_match:=date(timestamp)]
latest_odds[,date_of_match:=date(timestamp)]

latest_odds[,timestamp:=as_datetime(date,tz='Turkey')]

filtered_odds[,timestamp:=as_datetime(date,tz='Turkey')]
odds[,timestamp:=as_datetime(date,tz='Turkey')]

temp=matches[,list(matchId,date_of_match)]
latest_odds=merge(latest_odds,temp,by='matchId')

matches_of_2011=latest_odds[date_of_match>'2011-01-01' & date_of_match<'2012-01-01']

cut_levels=c(25:30)/50
matches_of_2011[,diff_bucket:=cut(prob_over,cut_levels)]

matches_of_2011=matches_of_2011[complete.cases(matches_of_2011)]

result_summary_2011=matches_of_2011[,
                                    list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                         bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]

matches_of_2012=latest_odds[date_of_match>'2012-01-01' & date_of_match<'2013-01-01']


matches_of_2012[,diff_bucket:=cut(prob_over,cut_levels)]

matches_of_2012=matches_of_2012[complete.cases(matches_of_2012)]

result_summary_2012=matches_of_2012[,
                                    list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                         bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]



matches_of_2013=latest_odds[date_of_match>'2013-01-01' & date_of_match<'2014-01-01']


matches_of_2013[,diff_bucket:=cut(prob_over,cut_levels)]

matches_of_2013=matches_of_2013[complete.cases(matches_of_2013)]

result_summary_2013=matches_of_2013[,
                                    list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                         bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]

matches_of_2014=latest_odds[date_of_match>'2014-01-01' & date_of_match<'2015-01-01']


matches_of_2014[,diff_bucket:=cut(prob_over,cut_levels)]

matches_of_2014=matches_of_2014[complete.cases(matches_of_2014)]

result_summary_2014=matches_of_2014[,
                                    list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                         bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]



matches_of_2015=latest_odds[date_of_match>'2015-01-01' & date_of_match<'2016-01-01']


matches_of_2015[,diff_bucket:=cut(prob_over,cut_levels)]

matches_of_2015=matches_of_2015[complete.cases(matches_of_2015)]

result_summary_2015=matches_of_2015[,
                                    list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                         bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]


matches_of_2016=latest_odds[date_of_match>'2016-01-01' & date_of_match<'2017-01-01']


matches_of_2016[,diff_bucket:=cut(prob_over,cut_levels)]

matches_of_2016=matches_of_2016[complete.cases(matches_of_2016)]

result_summary_2016=matches_of_2016[,
                                    list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                         bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]


matches_of_2017=latest_odds[date_of_match>'2017-01-01' & date_of_match<'2018-01-01']


matches_of_2017[,diff_bucket:=cut(prob_over,cut_levels)]

matches_of_2017=matches_of_2017[complete.cases(matches_of_2017)]

result_summary_2017=matches_of_2017[,
                                    list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                         bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]


matches_of_2018=latest_odds[date_of_match>'2018-01-01' & date_of_match<'2019-01-01']


matches_of_2018[,diff_bucket:=cut(prob_over,cut_levels)]

matches_of_2018=matches_of_2018[complete.cases(matches_of_2018)]

result_summary_2018=matches_of_2018[,
                                    list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                         bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]


matches_of_2019=latest_odds[date_of_match>'2019-01-01' & date_of_match<'2020-01-01']


matches_of_2019[,diff_bucket:=cut(prob_over,cut_levels)]

matches_of_2019=matches_of_2019[complete.cases(matches_of_2019)]

result_summary_2019=matches_of_2019[,
                                    list(real_over_ratio=sum(TotalGoal>=3,na.rm = TRUE)/.N,
                                         bookmaker_over_ratio=mean(prob_over[TotalGoal>=3],na.rm=TRUE)),by=list(diff_bucket)]



order1<-order(result_summary_2011$diff_bucket)
result_summary_2011=result_summary_2011[order1,]
order2<-order(result_summary_2012$diff_bucket)
result_summary_2012=result_summary_2012[order2,]
order3<-order(result_summary_2013$diff_bucket)
result_summary_2013=result_summary_2013[order3,]
order4<-order(result_summary_2014$diff_bucket)
result_summary_2014=result_summary_2014[order4,]
order5<-order(result_summary_2015$diff_bucket)
result_summary_2015=result_summary_2015[order5,]
order6<-order(result_summary_2016$diff_bucket)
result_summary_2016=result_summary_2016[order6,]
order7<-order(result_summary_2017$diff_bucket)
result_summary_2017=result_summary_2017[order7,]
order8<-order(result_summary_2018$diff_bucket)
result_summary_2018=result_summary_2018[order8,]
order9<-order(result_summary_2019$diff_bucket)
result_summary_2019=result_summary_2019[order9,]


plot(result_summary_2011$bookmaker_over_ratio,result_summary_2011$real_over_ratio,axes=T,col='dark red', xlim = c(5:6)/10, ylim = c(0:1),xlab="Real", ylab = "Bookmaker")
lines(result_summary_2011$bookmaker_over_ratio,result_summary_2011$real_over_ratio,col='dark red')
par(new=TRUE)

plot(result_summary_2012$bookmaker_over_ratio,result_summary_2012$real_over_ratio,axes=F,col='yellow', xlim = c(5:6)/10, ylim = c(0:1),xlab="Real", ylab = "Bookmaker")
lines(result_summary_2012$bookmaker_over_ratio,result_summary_2012$real_over_ratio,col='yellow')
par(new=TRUE)

plot(result_summary_2013$bookmaker_over_ratio,result_summary_2013$real_over_ratio,axes=F,col='dark blue',xlim = c(5:6)/10, ylim = c(0:1),xlab="Real", ylab = "Bookmaker")

lines(result_summary_2013$bookmaker_over_ratio,result_summary_2013$real_over_ratio,col='dark blue')
par(new=TRUE)

plot(result_summary_2014$bookmaker_over_ratio,result_summary_2014$real_over_ratio,axes=F,col='black', xlim = c(5:6)/10, ylim = c(0:1),xlab="Real", ylab = "Bookmaker")
lines(result_summary_2014$bookmaker_over_ratio,result_summary_2014$real_over_ratio)
par(new=TRUE)

plot(result_summary_2015$bookmaker_over_ratio,result_summary_2015$real_over_ratio,axes=F,col='gray', xlim = c(5:6)/10, ylim = c(0:1),xlab="Real", ylab = "Bookmaker")
lines(result_summary_2015$bookmaker_over_ratio,result_summary_2015$real_over_ratio,col='gray')
par(new=TRUE)

plot(result_summary_2016$bookmaker_over_ratio,result_summary_2016$real_over_ratio,axes=F,col='green', xlim = c(5:6)/10, ylim = c(0:1),xlab="Real", ylab = "Bookmaker")
lines(result_summary_2016$bookmaker_over_ratio,result_summary_2016$real_over_ratio,col='green')
par(new=TRUE)

plot(result_summary_2017$bookmaker_over_ratio,result_summary_2017$real_over_ratio,axes=F,col='orange', xlim = c(5:6)/10, ylim = c(0:1),xlab="Real", ylab = "Bookmaker")
lines(result_summary_2017$bookmaker_over_ratio,result_summary_2017$real_over_ratio,col='orange')
par(new=TRUE)

plot(result_summary_2018$bookmaker_over_ratio,result_summary_2018$real_over_ratio,axis=F,col='brown', xlim = c(5:6)/10, ylim = c(0:1),xlab="Real", ylab = "Bookmaker")
lines(result_summary_2018$bookmaker_over_ratio,result_summary_2018$real_over_ratio,col='brown')
par(new=TRUE)

abline(h=0.55)

