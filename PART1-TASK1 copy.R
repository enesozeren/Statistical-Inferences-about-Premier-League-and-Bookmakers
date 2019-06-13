setwd("/Users/enesozeren/Downloads/")
matches=readRDS("df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")

library(ggplot2)
library(data.table)

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


