library(ggplot2)
library(gridExtra)
library(data.table)
library(lubridate)
setwd("/Users/enesozeren/Downloads/")

matches=readRDS("df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")
matches[, c("HomeGoal", "AwayGoal"):=tstrsplit(score, ":")]

matches$leagueId=NULL
matches
matches$type=NULL
matches
sort(unique(matches$home))

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

matches[,c("HomeGoal", "AwayGoal"):=tstrsplit(score, ":")]
matches

matches[,HomeGoal:=as.numeric(HomeGoal)]
matches[,AwayGoal:=as.numeric(AwayGoal)]
matches[,MatchResult:=ifelse(HomeGoal>AwayGoal,"home", ifelse(HomeGoal==AwayGoal,"draw", "away"))]
matches

require(lubridate)
matches[,timestamp:=as_datetime(date)]
matches


#Summarize by home goal
summary_by_homegoal=matches[,list(count=.N),by=list(matchId,HomeGoal)]

factor(summary_by_homegoal$HomeGoal)
table_for_homegoal=table(summary_by_homegoal$HomeGoal)
table_for_homegoal

hist(summary_by_homegoal$HomeGoal,main = "HomeGoal Table", xlab = "Home Goals", ylab = "Number of Games", las =1, breaks = 30,col='light blue')
mean_homegoal=mean(matches$HomeGoal,na.rm = T)
mean_homegoal
par(new=TRUE)
plot(dpois(x=0:8,lambda=mean_homegoal), xlab = "Home Goals",ylab="Number of Games",axes=F,col='dark red',pch=19)


HomeGoal_pois=c(dpois(0,mean_homegoal)*sum(table_for_homegoal),
                dpois(1,mean_homegoal)*sum(table_for_homegoal),
                dpois(2,mean_homegoal)*sum(table_for_homegoal),
                dpois(3,mean_homegoal)*sum(table_for_homegoal),
                dpois(4,mean_homegoal)*sum(table_for_homegoal),
                dpois(5,mean_homegoal)*sum(table_for_homegoal),
                dpois(6,mean_homegoal)*sum(table_for_homegoal),
                dpois(7,mean_homegoal)*sum(table_for_homegoal),
                dpois(8,mean_homegoal)*sum(table_for_homegoal))

real_vs_poison_homegoal=data.table(Real_HomeGoal=table_for_homegoal,Poison_HomeGoal=HomeGoal_pois)

ggplot(real_vs_poison_homegoal, aes(real_vs_poison_homegoal$Real_HomeGoal.V1, cumsum(real_vs_poison_homegoal$Real_HomeGoal.N))) +
  geom_step(aes(group=1))+
  ggtitle("CDF of Real Home Goals")+
  xlab("Number of Home Goals")+
  ylab("Cumulative HomeGoals")+
  ylim(500, 3500)


ggplot(real_vs_poison_homegoal, aes(real_vs_poison_homegoal$Real_HomeGoal.V1, cumsum(real_vs_poison_homegoal$Poison_HomeGoal))) +
  geom_step(aes(group=1))+
  ggtitle("CDF of Poisson Home Goals")+
  xlab("Number of Home Goals")+
  ylab("Cumulative HomeGoals")+
  ylim(500, 3500)


#Summerize by awaygoals
summary_by_awaygoal=matches[,list(count=.N),by=list(matchId,AwayGoal)]
factor(summary_by_awaygoal$AwayGoal)
table_for_awaygoal=table(summary_by_awaygoal$AwayGoal)
table_for_awaygoal

hist(summary_by_awaygoal$AwayGoal,main = "AwayGoal Table", xlab = "Away Goals", ylab = "Number of Games",las=1, breaks = 30,col='light blue')

mean_awaygoal=mean(matches$AwayGoal,na.rm = T)
par(new=TRUE)
plot(dpois(x=0:7,lambda=mean_awaygoal), xlab = "Away Goals",ylab="Number of Games",axes=F,col='dark red',pch=19)

AwayGoal_pois=c(dpois(0,mean_awaygoal)*sum(table_for_awaygoal),
                dpois(1,mean_awaygoal)*sum(table_for_awaygoal),
                dpois(2,mean_awaygoal)*sum(table_for_awaygoal),
                dpois(3,mean_awaygoal)*sum(table_for_awaygoal),
                dpois(4,mean_awaygoal)*sum(table_for_awaygoal),
                dpois(5,mean_awaygoal)*sum(table_for_awaygoal),
                dpois(6,mean_awaygoal)*sum(table_for_awaygoal),
                dpois(7,mean_awaygoal)*sum(table_for_awaygoal))


real_vs_poison_awaygoal=data.table(Real_AwayGoal=table_for_awaygoal,Poison_AwayGoal=AwayGoal_pois)

ggplot(real_vs_poison_awaygoal, aes(real_vs_poison_awaygoal$Real_AwayGoal.V1, cumsum(real_vs_poison_awaygoal$Real_AwayGoal.N))) +
  geom_step(aes(group=1))+
  ggtitle("CDF of Real Away Goals")+
  xlab("Number of Away Goals")+
  ylab("Cumulative Away Goals")+
  ylim(500, 3500)

ggplot(real_vs_poison_awaygoal, aes(real_vs_poison_awaygoal$Real_AwayGoal.V1, cumsum(real_vs_poison_awaygoal$Poison_AwayGoal))) +
  geom_step(aes(group=1))+
  ggtitle("CDF of Poisson Away Goals")+
  xlab("Number of Away Goals")+
  ylab("Cumulative Away Goals")+
  ylim(500, 3500)



#(HomeGoals-AwayGoals)
summary_by_homegoal_and_awaygoal=matches[,list(count=.N),by=list(matchId,HomeGoal,AwayGoal)]
summary_by_homegoal_and_awaygoal
homegoal_minus_awaygoal=summary_by_homegoal_and_awaygoal[,list(count=.N),by=list(matchId,HomeGoal-AwayGoal)]
homegoal_minus_awaygoal[,c("HomeGoal-AwayGoal"):=HomeGoal]
homegoal_minus_awaygoal$HomeGoal=NULL
homegoal_minus_awaygoal$count=NULL
homegoal_minus_awaygoal
hist(homegoal_minus_awaygoal$`HomeGoal-AwayGoal`,main = "HomeGoal - AwayGoal Table", xlab = "Home Goals - Away Goals", ylab = "Number of Games", las =1, breaks = 60,col='light blue')


table_for_homegoal
sum(table_for_homegoal)


qchisq(0.95,df=8)


sum_away<-sum(((table_for_awaygoal[1:8]-AwayGoal_pois[1:8])^2)/(AwayGoal_pois[1:8]))


x <- seq(0, 40, length=1000)
y <- dchisq(x, df=7)

c <- qchisq(0.95,df=7)
plot(x, y, type="l",lty=1,main="Away Test", ylab="chi-square",xlab="x value")

abline(v= c, col='dark blue')
abline(v= sum_away, col='dark red')

arrows(x0=0, y0=0.08, x1=c, y1=0.08, code=3, col='dark blue')

text(x=4, y=0.085,labels='confidence interval', col='black')



sum_home<-sum(((table_for_homegoal[1:9]-HomeGoal_pois[1:9])^2)/(HomeGoal_pois[1:9]))

x <- seq(0, 25, length=1000)
y <- dchisq(x, df=8)

c_home <- qchisq(0.95,df=8)
plot(x, y, type="l",lty=1,main="Home Test", ylab="chi-square",xlab="x value")

abline(v= c_home, col='dark blue')
abline(v= sum_home, col='dark red')

arrows(x0=0, y0=0.08, x1=c_home, y1=0.08, code=3, col='dark blue')
arrows(x0=0, y0=0.06, x1=sum_home, y1=0.06, code=3, col='dark red')

text(x=4, y=0.085,labels='confidence interval', col='black')
text(x=2, y=0.065,labels= 'p value', col='black')

