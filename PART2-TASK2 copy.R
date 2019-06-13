library(ggplot2)
library(gridExtra)
library(data.table)
library(lubridate)
setwd("/Users/enesozeren/Downloads/")
matches=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds')
odds=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds')


matches[,c('HomeGoal','AwayGoal'):=tstrsplit(score,':')]
matches[,HomeGoal:=as.numeric(HomeGoal)]
matches[,AwayGoal:=as.numeric(AwayGoal)]
matches[,leagueId:=NULL]
matches[,Difference:=HomeGoal-AwayGoal]
#Tarih
matches[,Date:=as_datetime(date,tz='Turkey')]
matches[,date:=NULL]
matches[,type:=NULL]

#geç
matches[home=="manchester-united"]$home="manchester united"
matches[home=="manchester-utd"]$home="manchester united"
matches[home=="manchester utd"]$home="manchester united"
matches[home=="manchester-city"]$home="manchester city"
matches[home=="west-ham"]$home="west ham"
matches[home=="crystal-palace"]$home="crystal palace"
matches[home=="stoke"]$home="stoke city"
sort(unique(matches$home))
matches[away=="manchester-united"]$away="manchester united"
matches[away=="manchester-utd"]$away="manchester united"
matches[away=="manchester utd"]$away="manchester united"
matches[away=="manchester-city"]$away="manchester city"
matches[away=="west-ham"]$away="west ham"
matches[away=="crystal-palace"]$away="crystal palace"
matches[away=="stoke"]$away="stoke city"
sort(unique(matches$away))


#TASK2
temp=matches[,list(matchId,Date)]

#bet365
odds_bet365=odds[betType=="1x2" & bookmaker == "bet365"]
latest_odds_bet365=odds_bet365[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_odds_bet365=dcast(latest_odds_bet365,matchId~oddtype,value.var = "final_odd")

latest_odds_bet365[,prob_home:=1/odd1]
latest_odds_bet365[,prob_away:=1/odd2]
latest_odds_bet365[,prob_draw:=1/oddX]
latest_odds_bet365[,total_prob:=prob_home+prob_away+prob_draw]

latest_odds_bet365[,prob_home_normalized:=prob_home/total_prob]
latest_odds_bet365[,prob_away_normalized:=prob_away/total_prob]
latest_odds_bet365[,prob_draw_normalized:=prob_draw/total_prob]

latest_odds_bet365=merge(temp,latest_odds_bet365,by="matchId")

odds_2010_bet365=latest_odds_bet365[Date>'2010-07-01'& Date<'2011-07-01']
odds_2010_bet365=odds_2010_bet365[,difference:=prob_home_normalized-prob_away_normalized]

odds_2016_bet365=latest_odds_bet365[Date>'2016-07-01'& Date<'2017-07-01']
odds_2016_bet365=odds_2016_bet365[,difference:=prob_home_normalized-prob_away_normalized]

mean_2010_diff_bet365 = mean(odds_2010_bet365$difference)
sd_2010_diff_bet365 = sd(odds_2010_bet365$difference)

prob_2010_bet365 = 1- pt( (mean_2010_diff_bet365/(sd_2010_diff_bet365/sqrt(nrow(odds_2010_bet365)))),((nrow(odds_2010_bet365))-1))

mean_2016_diff_bet365 = mean(odds_2016_bet365$difference)
sd_2016_diff_bet365 = sd(odds_2016_bet365$difference)

prob_2016_bet365 =  1-pt( (mean_2016_diff_bet365/(sd_2016_diff_bet365/sqrt(nrow(odds_2016_bet365)))),((nrow(odds_2016_bet365))-1))

##bet365_2010 graph
x <- seq(-11, 11, length=1000)
y <- dt(x, df=(nrow(odds_2010_bet365)-1))
c1 <- qt(0.975,df=nrow(odds_2010_bet365)-1)
c2 <- qt(0.025,df=nrow(odds_2010_bet365)-1)
pvalue_bet365_2010<-(mean_2010_diff_bet365/(sd_2010_diff_bet365/sqrt(nrow(odds_2010_bet365))))

plot(x, y, type="l",lty=1,main="bet365_2010 Test", ylab="density",xlab="t value")
abline(v= c1, col='dark blue')
abline(v= c2, col='dark blue')
abline(v= pvalue_bet365_2010, col='dark red')
arrows(x0=c1, y0=0.3, x1=c2, y1=0.3, code=3, col='dark blue')
text(x=0, y=0.31,labels='confidence interval', col='black')


##bet365_2016 graph
x <- seq(-7, 7, length=1000)
y <- dt(x, df=(nrow(odds_2016_bet365)-1))
c1 <- qt(0.975,df=nrow(odds_2016_bet365)-1)
c2 <- qt(0.025,df=nrow(odds_2016_bet365)-1)
pvalue_bet365_2016<-(mean_2016_diff_bet365/(sd_2016_diff_bet365/sqrt(nrow(odds_2016_bet365))))

plot(x, y, type="l",lty=1,main="bet365_2016 Test", ylab="density",xlab="t value")
abline(v= c1, col='dark blue')
abline(v= c2, col='dark blue')
abline(v= pvalue_bet365_2016, col='dark red')
arrows(x0=c1, y0=0.3, x1=c2, y1=0.3, code=3, col='dark blue')
text(x=0, y=0.31,labels='confidence interval', col='black')







#10Bet
odds_10Bet=odds[betType=="1x2" & bookmaker == "10Bet"]
latest_odds_10Bet=odds_10Bet[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_odds_10Bet=dcast(latest_odds_10Bet,matchId~oddtype,value.var = "final_odd")

latest_odds_10Bet[,prob_home:=1/odd1]
latest_odds_10Bet[,prob_away:=1/odd2]
latest_odds_10Bet[,prob_draw:=1/oddX]
latest_odds_10Bet[,total_prob:=prob_home+prob_away+prob_draw]

latest_odds_10Bet[,prob_home_normalized:=prob_home/total_prob]
latest_odds_10Bet[,prob_away_normalized:=prob_away/total_prob]
latest_odds_10Bet[,prob_draw_normalized:=prob_draw/total_prob]

latest_odds_10Bet=merge(temp,latest_odds_10Bet,by="matchId")

odds_2010_10Bet=latest_odds_10Bet[Date>'2010-07-01'& Date<'2011-07-01']
odds_2010_10Bet=odds_2010_10Bet[,difference:=prob_home_normalized-prob_away_normalized]

odds_2016_10Bet=latest_odds_10Bet[Date>'2016-07-01'& Date<'2017-07-01']
odds_2016_10Bet=odds_2016_10Bet[,difference:=prob_home_normalized-prob_away_normalized]

mean_2010_diff_10Bet = mean(odds_2010_10Bet$difference)
sd_2010_diff_10Bet = sd(odds_2010_10Bet$difference)

prob_2010_10Bet = 1- pt( (mean_2010_diff_10Bet/(sd_2010_diff_10Bet/sqrt(nrow(odds_2010_10Bet)))),((nrow(odds_2010_10Bet))-1))

mean_2016_diff_10Bet = mean(odds_2016_10Bet$difference)
sd_2016_diff_10Bet = sd(odds_2016_10Bet$difference)

prob_2016_10Bet =  1-pt( (mean_2016_diff_10Bet/(sd_2016_diff_10Bet/sqrt(nrow(odds_2016_10Bet)))),((nrow(odds_2016_10Bet))-1))


##10Bet_2010 graph
x <- seq(-11, 11, length=1000)
y <- dt(x, df=(nrow(odds_2010_10Bet)-1))
c1 <- qt(0.975,df=nrow(odds_2010_10Bet)-1)
c2 <- qt(0.025,df=nrow(odds_2010_10Bet)-1)
pvalue_10Bet_2010<-(mean_2010_diff_10Bet/(sd_2010_diff_10Bet/sqrt(nrow(odds_2010_10Bet))))

plot(x, y, type="l",lty=1,main="10Bet_2010 Test", ylab="density",xlab="t value")
abline(v= c1, col='dark blue')
abline(v= c2, col='dark blue')
abline(v= pvalue_10Bet_2010, col='dark red')
arrows(x0=c1, y0=0.3, x1=c2, y1=0.3, code=3, col='dark blue')
text(x=0, y=0.31,labels='confidence interval', col='black')


##10bet_2016 graph
x <- seq(-7, 7, length=1000)
y <- dt(x, df=(nrow(odds_2016_10Bet)-1))
c1 <- qt(0.975,df=nrow(odds_2016_10Bet)-1)
c2 <- qt(0.025,df=nrow(odds_2016_10Bet)-1)
pvalue_10Bet_2016<-(mean_2016_diff_10Bet/(sd_2016_diff_10Bet/sqrt(nrow(odds_2016_10Bet))))

plot(x, y, type="l",lty=1,main="10bet_2016 Test", ylab="density",xlab="t value")
abline(v= c1, col='dark blue')
abline(v= c2, col='dark blue')
abline(v= pvalue_10Bet_2016, col='dark red')
arrows(x0=c1, y0=0.3, x1=c2, y1=0.3, code=3, col='dark blue')
text(x=0, y=0.31,labels='confidence interval', col='black')







#BetVictor
odds_BetVictor=odds[betType=="1x2" & bookmaker == "BetVictor"]
latest_odds_BetVictor=odds_BetVictor[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_odds_BetVictor=dcast(latest_odds_BetVictor,matchId~oddtype,value.var = "final_odd")

latest_odds_BetVictor[,prob_home:=1/odd1]
latest_odds_BetVictor[,prob_away:=1/odd2]
latest_odds_BetVictor[,prob_draw:=1/oddX]
latest_odds_BetVictor[,total_prob:=prob_home+prob_away+prob_draw]

latest_odds_BetVictor[,prob_home_normalized:=prob_home/total_prob]
latest_odds_BetVictor[,prob_away_normalized:=prob_away/total_prob]
latest_odds_BetVictor[,prob_draw_normalized:=prob_draw/total_prob]

latest_odds_BetVictor=merge(temp,latest_odds_BetVictor,by="matchId")

odds_2010_BetVictor=latest_odds_BetVictor[Date>'2010-07-01'& Date<'2011-07-01']
odds_2010_BetVictor=odds_2010_BetVictor[,difference:=prob_home_normalized-prob_away_normalized]

odds_2016_BetVictor=latest_odds_BetVictor[Date>'2016-07-01'& Date<'2017-07-01']
odds_2016_BetVictor=odds_2016_BetVictor[,difference:=prob_home_normalized-prob_away_normalized]

mean_2010_diff_BetVictor = mean(odds_2010_BetVictor$difference)
sd_2010_diff_BetVictor = sd(odds_2010_BetVictor$difference)

prob_2010_BetVictor = 1- pt( (mean_2010_diff_BetVictor/(sd_2010_diff_BetVictor/sqrt(nrow(odds_2010_BetVictor)))),((nrow(odds_2010_BetVictor))-1))

mean_2016_diff_BetVictor = mean(odds_2016_BetVictor$difference)
sd_2016_diff_BetVictor = sd(odds_2016_BetVictor$difference)

prob_2016_BetVictor =  1-pt( (mean_2016_diff_BetVictor/(sd_2016_diff_BetVictor/sqrt(nrow(odds_2016_BetVictor)))),((nrow(odds_2016_BetVictor))-1))



##BetVictor_2010 graph
x <- seq(-11, 11, length=1000)
y <- dt(x, df=(nrow(odds_2010_BetVictor)-1))
c1 <- qt(0.975,df=nrow(odds_2010_BetVictor)-1)
c2 <- qt(0.025,df=nrow(odds_2010_BetVictor)-1)
pvalue_BetVictor_2010<-(mean_2010_diff_BetVictor/(sd_2010_diff_BetVictor/sqrt(nrow(odds_2010_BetVictor))))

plot(x, y, type="l",lty=1,main="BetVictor_2010 Test", ylab="density",xlab="t value")
abline(v= c1, col='dark blue')
abline(v= c2, col='dark blue')
abline(v= pvalue_BetVictor_2010, col='dark red')
arrows(x0=c1, y0=0.3, x1=c2, y1=0.3, code=3, col='dark blue')
text(x=0, y=0.31,labels='confidence interval', col='black')


##BetVictor_2016 graph
x <- seq(-7, 7, length=1000)
y <- dt(x, df=(nrow(odds_2016_BetVictor)-1))
c1 <- qt(0.975,df=nrow(odds_2016_BetVictor)-1)
c2 <- qt(0.025,df=nrow(odds_2016_BetVictor)-1)
pvalue_BetVictor_2016<-(mean_2016_diff_BetVictor/(sd_2016_diff_BetVictor/sqrt(nrow(odds_2016_BetVictor))))

plot(x, y, type="l",lty=1,main="BetVictor_2016 Test", ylab="density",xlab="t value")
abline(v= c1, col='dark blue')
abline(v= c2, col='dark blue')
abline(v= pvalue_BetVictor_2016, col='dark red')
arrows(x0=c1, y0=0.3, x1=c2, y1=0.3, code=3, col='dark blue')
text(x=0, y=0.31,labels='confidence interval', col='black')






#Betsafe

odds_Betsafe=odds[betType=="1x2" & bookmaker == "Betsafe"]
latest_odds_Betsafe=odds_Betsafe[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_odds_Betsafe=dcast(latest_odds_Betsafe,matchId~oddtype,value.var = "final_odd")

latest_odds_Betsafe[,prob_home:=1/odd1]
latest_odds_Betsafe[,prob_away:=1/odd2]
latest_odds_Betsafe[,prob_draw:=1/oddX]
latest_odds_Betsafe[,total_prob:=prob_home+prob_away+prob_draw]

latest_odds_Betsafe[,prob_home_normalized:=prob_home/total_prob]
latest_odds_Betsafe[,prob_away_normalized:=prob_away/total_prob]
latest_odds_Betsafe[,prob_draw_normalized:=prob_draw/total_prob]

latest_odds_Betsafe=merge(temp,latest_odds_Betsafe,by="matchId")

odds_2010_Betsafe=latest_odds_Betsafe[Date>'2010-07-01'& Date<'2011-07-01']
odds_2010_Betsafe=odds_2010_Betsafe[,difference:=prob_home_normalized-prob_away_normalized]

odds_2016_Betsafe=latest_odds_Betsafe[Date>'2016-07-01'& Date<'2017-07-01']
odds_2016_Betsafe=odds_2016_Betsafe[,difference:=prob_home_normalized-prob_away_normalized]

mean_2010_diff_Betsafe = mean(odds_2010_Betsafe$difference)
sd_2010_diff_Betsafe = sd(odds_2010_Betsafe$difference)

prob_2010_Betsafe = 1- pt( (mean_2010_diff_Betsafe/(sd_2010_diff_Betsafe/sqrt(nrow(odds_2010_Betsafe)))),((nrow(odds_2010_Betsafe))-1))

mean_2016_diff_Betsafe = mean(odds_2016_Betsafe$difference)
sd_2016_diff_Betsafe = sd(odds_2016_Betsafe$difference)

prob_2016_Betsafe =  1-pt( (mean_2016_diff_Betsafe/(sd_2016_diff_Betsafe/sqrt(nrow(odds_2016_Betsafe)))),((nrow(odds_2016_Betsafe))-1))


##Betsafe_2010 graph
x <- seq(-11, 11, length=1000)
y <- dt(x, df=(nrow(odds_2010_Betsafe)-1))
c1 <- qt(0.975,df=nrow(odds_2010_Betsafe)-1)
c2 <- qt(0.025,df=nrow(odds_2010_Betsafe)-1)
pvalue_Betsafe_2010<-(mean_2010_diff_Betsafe/(sd_2010_diff_Betsafe/sqrt(nrow(odds_2010_Betsafe))))

plot(x, y, type="l",lty=1,main="Betsafe_2010 Test", ylab="density",xlab="t value")
abline(v= c1, col='dark blue')
abline(v= c2, col='dark blue')
abline(v= pvalue_Betsafe_2010, col='dark red')
arrows(x0=c1, y0=0.3, x1=c2, y1=0.3, code=3, col='dark blue')
text(x=0, y=0.31,labels='confidence interval', col='black')


##Betsafe_2016 graph
x <- seq(-7, 7, length=1000)
y <- dt(x, df=(nrow(odds_2016_Betsafe)-1))
c1 <- qt(0.975,df=nrow(odds_2016_Betsafe)-1)
c2 <- qt(0.025,df=nrow(odds_2016_Betsafe)-1)
pvalue_Betsafe_2016<-(mean_2016_diff_Betsafe/(sd_2016_diff_Betsafe/sqrt(nrow(odds_2016_Betsafe))))

plot(x, y, type="l",lty=1,main="Betsafe_2016 Test", ylab="density",xlab="t value")
abline(v= c1, col='dark blue')
abline(v= c2, col='dark blue')
abline(v= pvalue_Betsafe_2016, col='dark red')
arrows(x0=c1, y0=0.3, x1=c2, y1=0.3, code=3, col='dark blue')
text(x=0, y=0.31,labels='confidence interval', col='black')







#888sport

odds_888sport=odds[betType=="1x2" & bookmaker == "888sport"]
latest_odds_888sport=odds_888sport[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_odds_888sport=dcast(latest_odds_888sport,matchId~oddtype,value.var = "final_odd")

latest_odds_888sport[,prob_home:=1/odd1]
latest_odds_888sport[,prob_away:=1/odd2]
latest_odds_888sport[,prob_draw:=1/oddX]
latest_odds_888sport[,total_prob:=prob_home+prob_away+prob_draw]

latest_odds_888sport[,prob_home_normalized:=prob_home/total_prob]
latest_odds_888sport[,prob_away_normalized:=prob_away/total_prob]
latest_odds_888sport[,prob_draw_normalized:=prob_draw/total_prob]

latest_odds_888sport=merge(temp,latest_odds_888sport,by="matchId")

odds_2010_888sport=latest_odds_888sport[Date>'2010-07-01'& Date<'2011-07-01']
odds_2010_888sport=odds_2010_888sport[,difference:=prob_home_normalized-prob_away_normalized]

odds_2016_888sport=latest_odds_888sport[Date>'2016-07-01'& Date<'2017-07-01']
odds_2016_888sport=odds_2016_888sport[,difference:=prob_home_normalized-prob_away_normalized]

mean_2010_diff_888sport = mean(odds_2010_888sport$difference)
sd_2010_diff_888sport = sd(odds_2010_888sport$difference)

prob_2010_888sport = 1- pt( (mean_2010_diff_888sport/(sd_2010_diff_888sport/sqrt(nrow(odds_2010_888sport)))),((nrow(odds_2010_888sport))-1))

mean_2016_diff_888sport = mean(odds_2016_888sport$difference)
sd_2016_diff_888sport = sd(odds_2016_888sport$difference)

prob_2016_888sport =  1-pt( (mean_2016_diff_888sport/(sd_2016_diff_888sport/sqrt(nrow(odds_2016_888sport)))),((nrow(odds_2016_888sport))-1))


##888sport_2010 graph
x <- seq(-11, 11, length=1000)
y <- dt(x, df=(nrow(odds_2010_888sport)-1))
c1 <- qt(0.975,df=nrow(odds_2010_888sport)-1)
c2 <- qt(0.025,df=nrow(odds_2010_888sport)-1)
pvalue_888sport_2010<-(mean_2010_diff_888sport/(sd_2010_diff_888sport/sqrt(nrow(odds_2010_888sport))))

plot(x, y, type="l",lty=1,main="888sport_2010 Test", ylab="density",xlab="t value")
abline(v= c1, col='dark blue')
abline(v= c2, col='dark blue')
abline(v= pvalue_888sport_2010, col='dark red')
arrows(x0=c1, y0=0.3, x1=c2, y1=0.3, code=3, col='dark blue')
text(x=0, y=0.31,labels='confidence interval', col='black')


##888sport_2016 graph
x <- seq(-7, 7, length=1000)
y <- dt(x, df=(nrow(odds_2016_888sport)-1))
c1 <- qt(0.975,df=nrow(odds_2016_888sport)-1)
c2 <- qt(0.025,df=nrow(odds_2016_888sport)-1)
pvalue_888sport_2016<-(mean_2016_diff_888sport/(sd_2016_diff_888sport/sqrt(nrow(odds_2016_888sport))))

plot(x, y, type="l",lty=1,main="888sport_2016 Test", ylab="density",xlab="t value")
abline(v= c1, col='dark blue')
abline(v= c2, col='dark blue')
abline(v= pvalue_888sport_2016, col='dark red')
arrows(x0=c1, y0=0.3, x1=c2, y1=0.3, code=3, col='dark blue')
text(x=0, y=0.31,labels='confidence interval', col='black')
