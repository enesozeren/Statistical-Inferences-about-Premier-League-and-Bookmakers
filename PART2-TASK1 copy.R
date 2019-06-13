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
matches[home=="stoke"]$home="stock city"
sort(unique(matches$home))
matches[away=="manchester-united"]$away="manchester united"
matches[away=="manchester-utd"]$away="manchester united"
matches[away=="manchester utd"]$away="manchester united"
matches[away=="manchester-city"]$away="manchester city"
matches[away=="west-ham"]$away="west ham"
matches[away=="crystal-palace"]$away="crystal palace"
matches[away=="stoke"]$away="stock city"
sort(unique(matches$away))


#remove NA
temp=matches[,list(Difference, Date, matchId)]
temp<-temp[complete.cases(temp),]

#Sezonlara göre Maçlar
season_2010_2011=temp[Date>'2010-07-01'& Date<'2011-07-01']
season_2011_2012=temp[Date>'2011-07-01'& Date<'2012-07-01']
season_2012_2013=temp[Date>'2012-07-01'& Date<'2013-07-01']
season_2013_2014=temp[Date>'2013-07-01'& Date<'2014-07-01']
season_2014_2015=temp[Date>'2014-07-01'& Date<'2015-07-01']
season_2015_2016=temp[Date>'2015-07-01'& Date<'2016-07-01']
season_2016_2017=temp[Date>'2016-07-01'& Date<'2017-07-01']
season_2017_2018=temp[Date>'2017-07-01'& Date<'2018-07-01']
season_2018_2019=temp[Date>'2018-07-01'& Date<'2019-07-01']


#calculating the sd and mean of seasons
sd_of_season_2010_2011=sd(season_2010_2011$Difference)/sqrt(nrow(season_2010_2011))
mean_of_season_2010_2011=mean(season_2010_2011$Difference)

sd_of_season_2011_2012=sd(season_2011_2012$Difference)/sqrt(nrow(season_2011_2012))
mean_of_season_2011_2012=mean(season_2011_2012$Difference)

sd_of_season_2012_2013=sd(season_2012_2013$Difference)/sqrt(nrow(season_2012_2013))
mean_of_season_2012_2013=mean(season_2012_2013$Difference)

sd_of_season_2013_2014=sd(season_2013_2014$Difference)/sqrt(nrow(season_2013_2014))
mean_of_season_2013_2014=mean(season_2013_2014$Difference)

sd_of_season_2014_2015=sd(season_2014_2015$Difference)/sqrt(nrow(season_2014_2015))
mean_of_season_2014_2015=mean(season_2014_2015$Difference)

sd_of_season_2015_2016=sd(season_2015_2016$Difference)/sqrt(nrow(season_2015_2016))
mean_of_season_2015_2016=mean(season_2015_2016$Difference)

sd_of_season_2016_2017=sd(season_2016_2017$Difference)/sqrt(nrow(season_2016_2017))
mean_of_season_2016_2017=mean(season_2016_2017$Difference)

sd_of_season_2017_2018=sd(season_2017_2018$Difference)/sqrt(nrow(season_2017_2018))
mean_of_season_2017_2018=mean(season_2017_2018$Difference)

sd_of_season_2018_2019=sd(season_2018_2019$Difference)/sqrt(nrow(season_2018_2019))
mean_of_season_2018_2019=mean(season_2018_2019$Difference)

#graphs of normal distributions of seasons
pval_2010_2011=1-pnorm(mean_of_season_2010_2011,mean = 0,sd = sd_of_season_2010_2011)
pval_2011_2012=1-pnorm(mean_of_season_2011_2012,mean = 0,sd = sd_of_season_2011_2012)
pval_2012_2013=1-pnorm(mean_of_season_2012_2013,mean = 0,sd = sd_of_season_2012_2013)
pval_2013_2014=1-pnorm(mean_of_season_2013_2014,mean = 0,sd = sd_of_season_2013_2014)
pval_2014_2015=1-pnorm(mean_of_season_2014_2015,mean = 0,sd = sd_of_season_2014_2015)
pval_2015_2016=1-pnorm(mean_of_season_2015_2016,mean = 0,sd = sd_of_season_2015_2016)
pval_2016_2017=1-pnorm(mean_of_season_2016_2017,mean = 0,sd = sd_of_season_2016_2017)
pval_2017_2018=1-pnorm(mean_of_season_2017_2018,mean = 0,sd = sd_of_season_2017_2018)
pval_2018_2019=1-pnorm(mean_of_season_2018_2019,mean = 0,sd = sd_of_season_2018_2019)


#standard normal distribution data
x <- seq(-0.4, 0.5, length=1000)
y <- dnorm(x, mean=0, sd=sd_of_season_2010_2011)
alpha_2010_2011 <- qnorm(0.9,mean=0, sd=sd_of_season_2010_2011)
#plot a standard normal distribution
plot(x, y, type="l",lty=1, main="Season 2010-2011 HomeGoal-AwayGoal",xlab="x value")

#plot a vertical line
abline(v= mean_of_season_2010_2011, col='dark blue')
abline(v= alpha_2010_2011, col='dark red')
#make the arrow
arrows(x0=-0.4, y0=3, x1=alpha_2010_2011, y1=3, code=3, col='dark red')
#plot the text
text(x=-0.28, y=3.1,labels='confidence interval', col='black')




x <- seq(-0.4, 0.5, length=1000)
y <- dnorm(x, mean=0, sd=sd_of_season_2011_2012)
alpha_2011_2012 <- qnorm(0.9,mean=0, sd=sd_of_season_2011_2012)
plot(x, y, type="l",lty=1,  main="Season 2011-2012 HomeGoal-AwayGoal", xlab="x value")

abline(v= mean_of_season_2011_2012, col='dark blue')
abline(v= alpha_2011_2012, col='dark red')

arrows(x0=-0.4, y0=3, x1=alpha_2011_2012, y1=3, code=3, col='dark red')

text(x=-0.28, y=3.1,labels='confidence interval', col='black')




x <- seq(-0.4, 0.5, length=1000)
y <- dnorm(x, mean=0, sd=sd_of_season_2012_2013)
alpha_2012_2013 <- qnorm(0.9,mean=0, sd=sd_of_season_2012_2013)

plot(x, y, type="l",lty=1,  main="Season 2012-2013 HomeGoal-AwayGoal", xlab="x value")
abline(v= mean_of_season_2012_2013, col='dark blue')
abline(v= alpha_2012_2013, col='dark red')
arrows(x0=-0.4, y0=3, x1=alpha_2012_2013, y1=3, code=3, col='dark red')
text(x=-0.28, y=3.1,labels='confidence interval', col='black')



x <- seq(-0.4, 0.5, length=1000)
y <- dnorm(x, mean=0, sd=sd_of_season_2013_2014)
alpha_2013_2014 <- qnorm(0.9,mean=0, sd=sd_of_season_2013_2014)
plot(x, y, type="l",lty=1,  main="Season 2013-2014 HomeGoal-AwayGoal", xlab="x value")

abline(v= mean_of_season_2013_2014, col='dark blue')
abline(v= alpha_2013_2014, col='dark red')
arrows(x0=-0.4, y0=3, x1=alpha_2013_2014, y1=3, code=3, col='dark red')
text(x=-0.28, y=3.1,labels='confidence interval', col='black')



x <- seq(-0.4, 0.5, length=1000)
y <- dnorm(x, mean=0, sd=sd_of_season_2014_2015)
alpha_2014_2015 <- qnorm(0.9,mean=0, sd=sd_of_season_2014_2015)
plot(x, y, type="l",lty=1, main="Season 2014-2015 HomeGoal-AwayGoal", xlab="x value")

abline(v= mean_of_season_2014_2015, col='dark blue')
abline(v= alpha_2014_2015, col='dark red')
arrows(x0=-0.4, y0=3, x1=alpha_2014_2015, y1=3, code=3, col='dark red')
text(x=-0.28, y=3.1,labels='confidence interval', col='black')





x <- seq(-0.4, 0.5, length=1000)
y <- dnorm(x, mean=0, sd=sd_of_season_2015_2016)
alpha_2015_2016 <- qnorm(0.9,mean=0, sd=sd_of_season_2015_2016)
plot(x, y, type="l",lty=1, main="Season 2015-2016 HomeGoal-AwayGoal", xlab="x value")

abline(v= mean_of_season_2015_2016, col='dark blue')
abline(v= alpha_2015_2016, col='dark red')

arrows(x0=-0.4, y0=3, x1=alpha_2015_2016, y1=3, code=3, col='dark red')
text(x=-0.28, y=3.1,labels='confidence interval', col='black')




x <- seq(-0.4, 0.5, length=1000)
y <- dnorm(x, mean=0, sd=sd_of_season_2016_2017)
alpha_2016_2017 <- qnorm(0.9,mean=0, sd=sd_of_season_2016_2017)
plot(x, y, type="l",lty=1,  main="Season 2016-2017 HomeGoal-AwayGoal", xlab="x value")

abline(v= mean_of_season_2016_2017, col='dark blue')
abline(v= alpha_2016_2017, col='dark red')

arrows(x0=-0.4, y0=3, x1=alpha_2016_2017, y1=3, code=3, col='dark red')

text(x=-0.28, y=3.1,labels='confidence interval', col='black')





x <- seq(-0.4, 0.5, length=1000)
y <- dnorm(x, mean=0, sd=sd_of_season_2017_2018)
alpha_2017_2018 <- qnorm(0.9,mean=0, sd=sd_of_season_2017_2018)
plot(x, y, type="l",lty=1, main="Season 2017-2018 HomeGoal-AwayGoal", xlab="x value")

abline(v= mean_of_season_2017_2018, col='dark blue')
abline(v= alpha_2017_2018, col='dark red')
arrows(x0=-0.4, y0=3, x1=alpha_2017_2018, y1=3, code=3, col='dark red')
text(x=-0.28, y=3.1,labels='confidence interval', col='black')




x <- seq(-0.4, 0.5, length=1000)
y <- dnorm(x, mean=0, sd=sd_of_season_2018_2019)
alpha_2018_2019 <- qnorm(0.9,mean=0, sd=sd_of_season_2018_2019)
plot(x, y, type="l",lty=1, main="Season 2018-2019 HomeGoal-AwayGoal", xlab="x value")

abline(v= mean_of_season_2018_2019, col='dark blue')
abline(v= alpha_2018_2019, col='dark red')
arrows(x0=-0.4, y0=2.5, x1=alpha_2018_2019, y1=2.5, code=3, col='dark red')
text(x=-0.28, y=2.6,labels='confidence interval', col='black')
