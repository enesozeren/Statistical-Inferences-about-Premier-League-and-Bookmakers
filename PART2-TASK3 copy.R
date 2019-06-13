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


#TASK3
season_2010 = matches[Date>'2010-07-01'& Date<'2011-07-01']
sd_2010 = sd(season_2010$AwayGoal)


season_2016 = matches[Date>'2016-07-01'& Date<'2017-07-01']
sd_2016 = sd(season_2016$AwayGoal)



#calculating the F probability
prob_variance_ratio = pf( (sd_2010**2/sd_2016**2) , nrow(season_2010)-1, nrow(season_2016)-1)
                                                                                                                              

#calculating the F probability
prob_variance_ratio = pf( (sd_2010^2/sd_2016^2) , nrow(season_2010)-1, nrow(season_2016)-1)

#for alpha=0.1
x <- seq(0, 2, length=1000)
y <- df(x,  nrow(season_2010)-1, nrow(season_2016)-1)
c1 <- qf(0.95,nrow(season_2010)-1, nrow(season_2016)-1)
c2 <- qf(0.05,nrow(season_2010)-1, nrow(season_2016)-1)
c3 <- (sd_2010^2/sd_2016^2)

plot(x, y, type="l",lty=1,main="F distribution Test", ylab="density",xlab="f value")
abline(v= c1, col='dark blue')
abline(v= c2, col='dark blue')
abline(v= c3, col='dark red')
arrows(x0=c1, y0=3, x1=c2, y1=3, code=3, col='dark blue')
text(x=1, y=3.1,labels='confidence interval', col='black')



#for alpha=0.01
x <- seq(0, 2, length=1000)
y <- df(x,  nrow(season_2010)-1, nrow(season_2016)-1)
c1 <- qf(0.995,nrow(season_2010)-1, nrow(season_2016)-1)
c2 <- qf(0.005,nrow(season_2010)-1, nrow(season_2016)-1)
c3 <- (sd_2010^2/sd_2016^2)

plot(x, y, type="l",lty=1,main="F distribution Test", ylab="density",xlab="f value")
abline(v= c1, col='dark blue')
abline(v= c2, col='dark blue')
abline(v= c3, col='dark red')
arrows(x0=c1, y0=3, x1=c2, y1=3, code=3, col='dark blue')
text(x=1, y=3.1,labels='confidence interval', col='black')


