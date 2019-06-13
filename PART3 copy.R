library(data.table)
library(lubridate)
setwd("/Users/enesozeren/Downloads/")
matches=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds')
odds=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds')

matches[,Date:=as_datetime(date,tz='Turkey')]
odds[,Date:=as_datetime(date,tz='Turkey')]
matches[,c('HomeGoal','AwayGoal'):=tstrsplit(score,':')]
matches[,HomeGoal:=as.numeric(HomeGoal)]
matches[,AwayGoal:=as.numeric(AwayGoal)]
matches[, Total:=HomeGoal+AwayGoal]

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


#TEST DATA

matches_2018_2019=matches[Date>'2018-07-01'& Date<'2019-07-01']

ou_odds = odds[betType == "ou"]
latest_ou_odds=ou_odds[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_over_odds=latest_ou_odds[oddtype == "over"]
latest_over_odds=dcast(latest_over_odds,matchId~oddtype,value.var = "final_odd")

matches_2018_2019=merge(latest_over_odds, matches_2018_2019, by = "matchId")

temp_match_list <- copy(matches_2018_2019)



#TRAIN DATA
#Buradan Sonrası Date>'2010-07-01'& Date<'2018-07-01'

odds=odds[Date>'2010-07-01'& Date<'2018-07-01']
matches=matches[Date>'2010-07-01'& Date<'2018-07-01']



ou_odds = odds[betType == "ou"]
latest_ou_odds=ou_odds[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_over_odds=latest_ou_odds[oddtype == "over"]
latest_over_odds=dcast(latest_over_odds,matchId~oddtype,value.var = "final_odd")

matches=merge(latest_over_odds, matches, by = "matchId")


#geç
temp_matches = matches[Date>'2017-07-01'& Date<'2018-07-01']


#MEAN GOALS OF TEAMS
arsenal_home_goal = matches[matches$home == "arsenal"]
arsenal_home_goal_mean = mean(arsenal_home_goal$HomeGoal) 

arsenal_away_goal = matches[matches$away == "arsenal"]
arsenal_away_goal_mean = mean(arsenal_away_goal$AwayGoal) 


aston.villa_home_goal = matches[matches$home == "aston villa"]
aston.villa_home_goal_mean = mean(aston.villa_home_goal$HomeGoal) 

aston.villa_away_goal = matches[matches$away == "aston villa"]
aston.villa_away_goal_mean = mean(aston.villa_away_goal$AwayGoal) 


birmingham_home_goal = matches[matches$home == "birmingham"]
birmingham_home_goal_mean = mean(birmingham_home_goal$HomeGoal) 

birmingham_away_goal = matches[matches$away == "birmingham"]
birmingham_away_goal_mean = mean(birmingham_away_goal$AwayGoal) 


blackburn_home_goal = matches[matches$home == "blackburn"]
blackburn_home_goal_mean = mean(blackburn_home_goal$HomeGoal) 

blackburn_away_goal = matches[matches$away == "blackburn"]
blackburn_away_goal_mean = mean(blackburn_away_goal$AwayGoal) 


blackpool_home_goal = matches[matches$home == "blackpool"]
blackpool_home_goal_mean = mean(blackpool_home_goal$HomeGoal) 

blackpool_away_goal = matches[matches$away == "blackpool"]
blackpool_away_goal_mean = mean(blackpool_away_goal$AwayGoal) 


bolton_home_goal = matches[matches$home == "bolton"]
bolton_home_goal_mean = mean(bolton_home_goal$HomeGoal) 

bolton_away_goal = matches[matches$away == "bolton"]
bolton_away_goal_mean = mean(bolton_away_goal$AwayGoal) 


bournemouth_home_goal = matches[matches$home == "bournemouth"]
bournemouth_home_goal_mean = mean(bournemouth_home_goal$HomeGoal) 

bournemouth_away_goal = matches[matches$away == "bournemouth"]
bournemouth_away_goal_mean = mean(bournemouth_away_goal$AwayGoal) 


brighton_home_goal = matches[matches$home == "brighton"]
brighton_home_goal_mean = mean(brighton_home_goal$HomeGoal) 

brighton_away_goal = matches[matches$away == "brighton"]
brighton_away_goal_mean = mean(brighton_away_goal$AwayGoal) 


burnley_home_goal = matches[matches$home == "burnley"]
burnley_home_goal_mean = mean(burnley_home_goal$HomeGoal) 

burnley_away_goal = matches[matches$away == "burnley"]
burnley_away_goal_mean = mean(burnley_away_goal$AwayGoal) 


cardiff_home_goal = matches[matches$home == "cardiff"]
cardiff_home_goal_mean = mean(cardiff_home_goal$HomeGoal) 

cardiff_away_goal = matches[matches$away == "cardiff"]
cardiff_away_goal_mean = mean(cardiff_away_goal$AwayGoal) 


chelsea_home_goal = matches[matches$home == "chelsea"]
chelsea_home_goal_mean = mean(chelsea_home_goal$HomeGoal) 

chelsea_away_goal = matches[matches$away == "chelsea"]
chelsea_away_goal_mean = mean(chelsea_away_goal$AwayGoal)


crystal.palace_home_goal = matches[matches$home == "crystal palace"]
crystal.palace_home_goal_mean = mean(crystal.palace_home_goal$HomeGoal) 

crystal.palace_away_goal = matches[matches$away == "crystal palace"]
crystal.palace_away_goal_mean = mean(crystal.palace_away_goal$AwayGoal)


everton_home_goal = matches[matches$home == "everton"]
everton_home_goal_mean = mean(everton_home_goal$HomeGoal) 

everton_away_goal = matches[matches$away == "everton"]
everton_away_goal_mean = mean(everton_away_goal$AwayGoal)


fulham_home_goal = matches[matches$home == "fulham"]
fulham_home_goal_mean = mean(fulham_home_goal$HomeGoal) 

fulham_away_goal = matches[matches$away == "fulham"]
fulham_away_goal_mean = mean(fulham_away_goal$AwayGoal)


huddersfield_home_goal = matches[matches$home == "huddersfield"]
huddersfield_home_goal_mean = mean(huddersfield_home_goal$HomeGoal) 

huddersfield_away_goal = matches[matches$away == "huddersfield"]
huddersfield_away_goal_mean = mean(huddersfield_away_goal$AwayGoal)


hull.city_home_goal = matches[matches$home == "hull city"]
hull.city_home_goal_mean = mean(hull.city_home_goal$HomeGoal) 

hull.city_away_goal = matches[matches$away == "hull city"]
hull.city_away_goal_mean = mean(hull.city_away_goal$AwayGoal)



leicester_home_goal = matches[matches$home == "leicester"]
leicester_home_goal_mean = mean(leicester_home_goal$HomeGoal) 

leicester_away_goal = matches[matches$away == "leicester"]
leicester_away_goal_mean = mean(leicester_away_goal$AwayGoal)


liverpool_home_goal = matches[matches$home == "liverpool"]
liverpool_home_goal_mean = mean(liverpool_home_goal$HomeGoal) 

liverpool_away_goal = matches[matches$away == "liverpool"]
liverpool_away_goal_mean = mean(liverpool_away_goal$AwayGoal)


manchester.city_home_goal = matches[matches$home == "manchester city"]
manchester.city_home_goal_mean = mean(manchester.city_home_goal$HomeGoal) 

manchester.city_away_goal = matches[matches$away == "manchester city"]
manchester.city_away_goal_mean = mean(manchester.city_away_goal$AwayGoal)


manchester.united_home_goal = matches[matches$home == "manchester united"]
manchester.united_home_goal_mean = mean(manchester.united_home_goal$HomeGoal) 

manchester.united_away_goal = matches[matches$away == "manchester united"]
manchester.united_away_goal_mean = mean(manchester.united_away_goal$AwayGoal)


middlesbrough_home_goal = matches[matches$home == "middlesbrough"]
middlesbrough_home_goal_mean = mean(middlesbrough_home_goal$HomeGoal) 

middlesbrough_away_goal = matches[matches$away == "middlesbrough"]
middlesbrough_away_goal_mean = mean(middlesbrough_away_goal$AwayGoal)


newcastle.utd_home_goal = matches[matches$home == "newcastle utd"]
newcastle.utd_home_goal_mean = mean(newcastle.utd_home_goal$HomeGoal) 

newcastle.utd_away_goal = matches[matches$away == "newcastle utd"]
newcastle.utd_away_goal_mean = mean(newcastle.utd_away_goal$AwayGoal)


norwich_home_goal = matches[matches$home == "norwich"]
norwich_home_goal_mean = mean(norwich_home_goal$HomeGoal) 

norwich_away_goal = matches[matches$away == "norwich"]
norwich_away_goal_mean = mean(norwich_away_goal$AwayGoal)


qpr_home_goal = matches[matches$home == "qpr"]
qpr_home_goal_mean = mean(qpr_home_goal$HomeGoal) 

qpr_away_goal = matches[matches$away == "qpr"]
qpr_away_goal_mean = mean(qpr_away_goal$AwayGoal)


reading_home_goal = matches[matches$home == "reading"]
reading_home_goal_mean = mean(reading_home_goal$HomeGoal) 

reading_away_goal = matches[matches$away == "reading"]
reading_away_goal_mean = mean(reading_away_goal$AwayGoal)


southampton_home_goal = matches[matches$home == "southampton"]
southampton_home_goal_mean = mean(southampton_home_goal$HomeGoal) 

southampton_away_goal = matches[matches$away == "southampton"]
southampton_away_goal_mean = mean(southampton_away_goal$AwayGoal)


stoke.city_home_goal = matches[matches$home == "stoke city"]
stoke.city_home_goal_mean = mean(stoke.city_home_goal$HomeGoal) 

stoke.city_away_goal = matches[matches$away == "stoke city"]
stoke.city_away_goal_mean = mean(stoke.city_away_goal$AwayGoal)


sunderland_home_goal = matches[matches$home == "sunderland"]
sunderland_home_goal_mean = mean(sunderland_home_goal$HomeGoal) 

sunderland_away_goal = matches[matches$away == "sunderland"]
sunderland_away_goal_mean = mean(sunderland_away_goal$AwayGoal)


swansea_home_goal = matches[matches$home == "swansea"]
swansea_home_goal_mean = mean(swansea_home_goal$HomeGoal) 

swansea_away_goal = matches[matches$away == "swansea"]
swansea_away_goal_mean = mean(swansea_away_goal$AwayGoal)


tottenham_home_goal = matches[matches$home == "tottenham"]
tottenham_home_goal_mean = mean(tottenham_home_goal$HomeGoal) 

tottenham_away_goal = matches[matches$away == "tottenham"]
tottenham_away_goal_mean = mean(tottenham_away_goal$AwayGoal)


watford_home_goal = matches[matches$home == "watford"]
watford_home_goal_mean = mean(watford_home_goal$HomeGoal) 

watford_away_goal = matches[matches$away == "watford"]
watford_away_goal_mean = mean(watford_away_goal$AwayGoal)


west.brom_home_goal = matches[matches$home == "west brom"]
west.brom_home_goal_mean = mean(west.brom_home_goal$HomeGoal) 

west.brom_away_goal = matches[matches$away == "west brom"]
west.brom_away_goal_mean = mean(west.brom_away_goal$AwayGoal)


west.ham_home_goal = matches[matches$home == "west ham"]
west.ham_home_goal_mean = mean(west.ham_home_goal$HomeGoal) 

west.ham_away_goal = matches[matches$away == "west ham"]
west.ham_away_goal_mean = mean(west.ham_away_goal$AwayGoal)


wigan_home_goal = matches[matches$home == "wigan"]
wigan_home_goal_mean = mean(wigan_home_goal$HomeGoal) 

wigan_away_goal = matches[matches$away == "wigan"]
wigan_away_goal_mean = mean(wigan_away_goal$AwayGoal)


wolves_home_goal = matches[matches$home == "wolves"]
wolves_home_goal_mean = mean(wolves_home_goal$HomeGoal) 

wolves_away_goal = matches[matches$away == "wolves"]
wolves_away_goal_mean = mean(wolves_away_goal$AwayGoal)



##

names<- c("arsenal", "aston villa" , "birmingham", "blackburn", "blackpool","bolton"   ,"bournemout", "brighton", "burnley", "cardiff", "chelsea",
          "crystal place", "everton", "fulham", "huddersfield","hull city"  , "leicester", "liverpool", "manchester city",
          "manchester united","middlesbrough" , "newcastle utd","norwich" ,"qpr", "reading" , "southampton", "stoke city" ,"sunderland","swansea" , "tottenham", "watford", "west brom", "west ham","wigan", "wolves")

home_means <- c(arsenal_home_goal_mean,aston.villa_home_goal_mean,birmingham_home_goal_mean,blackburn_home_goal_mean,blackpool_home_goal_mean, 
                bolton_home_goal_mean,bournemouth_home_goal_mean, brighton_home_goal_mean, burnley_home_goal_mean, 
                cardiff_home_goal_mean, chelsea_home_goal_mean, crystal.palace_home_goal_mean, everton_home_goal_mean, 
                fulham_home_goal_mean, huddersfield_home_goal_mean,hull.city_home_goal_mean, leicester_home_goal_mean, liverpool_home_goal_mean, 
                manchester.city_home_goal_mean, manchester.united_home_goal_mean,middlesbrough_home_goal_mean, newcastle.utd_home_goal_mean, 
                norwich_home_goal_mean,qpr_home_goal_mean,reading_home_goal_mean,southampton_home_goal_mean,stoke.city_home_goal_mean ,
                sunderland_home_goal_mean,swansea_home_goal_mean,tottenham_home_goal_mean, watford_home_goal_mean,west.brom_home_goal_mean, 
                west.ham_home_goal_mean,wigan_home_goal_mean, wolves_home_goal_mean)

away_means <- c(arsenal_away_goal_mean,aston.villa_away_goal_mean,birmingham_away_goal_mean,blackburn_away_goal_mean,blackpool_away_goal_mean, 
                bolton_away_goal_mean,bournemouth_away_goal_mean, brighton_away_goal_mean, burnley_away_goal_mean, 
                cardiff_away_goal_mean, chelsea_away_goal_mean, crystal.palace_away_goal_mean, everton_away_goal_mean, 
                fulham_away_goal_mean, huddersfield_away_goal_mean,hull.city_away_goal_mean, leicester_away_goal_mean, liverpool_away_goal_mean, 
                manchester.city_away_goal_mean, manchester.united_away_goal_mean,middlesbrough_away_goal_mean, newcastle.utd_away_goal_mean, 
                norwich_away_goal_mean,qpr_away_goal_mean,reading_away_goal_mean,southampton_away_goal_mean,stoke.city_away_goal_mean ,
                sunderland_away_goal_mean,swansea_away_goal_mean,tottenham_away_goal_mean, watford_away_goal_mean,west.brom_away_goal_mean, 
                west.ham_away_goal_mean,wigan_away_goal_mean, wolves_away_goal_mean)



team_goal_means <- data.frame(names, home_means, away_means)


matches[, MeanTotal:= 0]
matches[, MeanTotal := ifelse(home == "arsenal", MeanTotal + arsenal_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "arsenal", MeanTotal + arsenal_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "aston villa", MeanTotal + aston.villa_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "aston villa", MeanTotal + aston.villa_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "birmingham", MeanTotal + birmingham_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "birmingham", MeanTotal + birmingham_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "blackburn", MeanTotal + blackburn_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "blackburn", MeanTotal + blackburn_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "blackpool", MeanTotal + blackpool_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "blackpool", MeanTotal + blackpool_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "bolton", MeanTotal + bolton_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "bolton", MeanTotal + bolton_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "bournemouth", MeanTotal + bournemouth_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "bournemouth", MeanTotal + bournemouth_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "brighton", MeanTotal + brighton_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "brighton", MeanTotal + brighton_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "burnley", MeanTotal + burnley_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "burnley", MeanTotal + burnley_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "cardiff", MeanTotal + cardiff_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "cardiff", MeanTotal + cardiff_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "chelsea", MeanTotal + chelsea_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "chelsea", MeanTotal + chelsea_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "crystal palace", MeanTotal + crystal.palace_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "crystal palace", MeanTotal + crystal.palace_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "everton", MeanTotal + everton_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "everton", MeanTotal + everton_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "fulham", MeanTotal + fulham_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "fulham", MeanTotal + fulham_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "huddersfield", MeanTotal + huddersfield_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "huddersfield", MeanTotal + huddersfield_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "hull city", MeanTotal + hull.city_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "hull city", MeanTotal + hull.city_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "leicester", MeanTotal + leicester_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "leicester", MeanTotal + leicester_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "liverpool", MeanTotal + liverpool_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "liverpool", MeanTotal + liverpool_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "manchester city", MeanTotal + manchester.city_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "manchester city", MeanTotal + manchester.city_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "manchester united", MeanTotal + manchester.united_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "manchester united", MeanTotal + manchester.united_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "middlesbrough", MeanTotal + middlesbrough_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "middlesbrough", MeanTotal + middlesbrough_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "newcastle utd", MeanTotal + newcastle.utd_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "newcastle utd", MeanTotal + newcastle.utd_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "norwich", MeanTotal + norwich_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "norwich", MeanTotal + norwich_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "qpr", MeanTotal + qpr_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "qpr", MeanTotal + qpr_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "reading", MeanTotal + reading_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "reading", MeanTotal + reading_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "southampton", MeanTotal + southampton_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "southampton", MeanTotal + southampton_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "stoke city", MeanTotal + stoke.city_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "stoke city", MeanTotal + stoke.city_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "sunderland", MeanTotal + sunderland_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "sunderland", MeanTotal + sunderland_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "swansea", MeanTotal + swansea_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "swansea", MeanTotal + swansea_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "tottenham", MeanTotal + tottenham_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "tottenham", MeanTotal + tottenham_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "watford", MeanTotal + watford_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "watford", MeanTotal + watford_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "west brom", MeanTotal + west.brom_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "west brom", MeanTotal + west.brom_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "west ham", MeanTotal + west.ham_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "west ham", MeanTotal + west.ham_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "wigan", MeanTotal + wigan_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "wigan", MeanTotal + wigan_away_goal_mean, MeanTotal)]

matches[, MeanTotal := ifelse(home == "wolves", MeanTotal + wolves_home_goal_mean, MeanTotal)]
matches[, MeanTotal := ifelse(away == "wolves", MeanTotal + wolves_away_goal_mean, MeanTotal)]



matches[,OnlyHour:= substr(Date,12,13)]
matches[,OnlyHour:= as.numeric(OnlyHour)]
matches[,EveningMatch:= ifelse(OnlyHour >= 20, 1, 0)]
matches[,MorningMatch:= ifelse(OnlyHour < 17, 1, 0)]


matches[,MatchMonth:= substr(Date,6,7)]
matches[,MatchMonth:= as.numeric(MatchMonth)]
matches[,BegOfSeason := ifelse(MatchMonth == 8 | MatchMonth == 9 | MatchMonth == 10, 1, 0)]
matches[,MidOfSeason := ifelse(MatchMonth == 11 | MatchMonth == 12 | MatchMonth == 1 | MatchMonth == 2, 1, 0)]
matches[,EndOfSeason := ifelse(MatchMonth == 3 | MatchMonth == 4 | MatchMonth == 5, 1, 0)]

temp_matches_2010_2018 <- copy(matches)

matches[,Date:=NULL]
matches[,date:=NULL]
matches[,type:=NULL]
matches[,leagueId:=NULL]
matches[,score:=NULL]
matches[,HomeGoal:=NULL]
matches[,AwayGoal:=NULL]
matches[,OnlyHour:=NULL]
matches[,MatchMonth:=NULL]



##TEST DATA ADDING VALUES OF PREDICTORS(x)

names2<- c("arsenal", "bournemout", "brighton", "burnley", "cardiff", "chelsea",
           "crystal place", "everton", "fulham", "huddersfield", "leicester", "liverpool", "manchester city",
           "manchester united", "newcastle utd", "southampton", "tottenham", "watford", "west ham", "wolves")

home_means2 <- c(arsenal_home_goal_mean, bournemouth_home_goal_mean, brighton_home_goal_mean, burnley_home_goal_mean, 
                 cardiff_home_goal_mean, chelsea_home_goal_mean, crystal.palace_home_goal_mean, everton_home_goal_mean, 
                 fulham_home_goal_mean, huddersfield_home_goal_mean, leicester_home_goal_mean, liverpool_home_goal_mean, 
                 manchester.city_home_goal_mean, manchester.united_home_goal_mean, newcastle.utd_home_goal_mean, 
                 southampton_home_goal_mean, tottenham_home_goal_mean, watford_home_goal_mean, west.ham_home_goal_mean, wolves_home_goal_mean)


away_means2 <- c(arsenal_away_goal_mean, bournemouth_away_goal_mean, brighton_away_goal_mean, burnley_away_goal_mean,
                 cardiff_away_goal_mean, chelsea_away_goal_mean, crystal.palace_away_goal_mean, everton_away_goal_mean,
                 fulham_away_goal_mean, huddersfield_away_goal_mean, leicester_away_goal_mean, liverpool_away_goal_mean,
                 manchester.city_away_goal_mean, manchester.united_away_goal_mean, newcastle.utd_away_goal_mean,
                 southampton_away_goal_mean, tottenham_away_goal_mean, watford_away_goal_mean, west.ham_away_goal_mean,
                 wolves_away_goal_mean)


team_goal_means2 <- data.frame(names2, home_means2, away_means2)

matches_2018_2019[, MeanTotal:= 0]
matches_2018_2019[, MeanTotal := ifelse(home == "arsenal", MeanTotal + arsenal_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "arsenal", MeanTotal + arsenal_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "bournemouth", MeanTotal + bournemouth_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "bournemouth", MeanTotal + bournemouth_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "brighton", MeanTotal + brighton_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "brighton", MeanTotal + brighton_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "burnley", MeanTotal + burnley_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "burnley", MeanTotal + burnley_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "cardiff", MeanTotal + cardiff_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "cardiff", MeanTotal + cardiff_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "chelsea", MeanTotal + chelsea_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "chelsea", MeanTotal + chelsea_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "crystal palace", MeanTotal + crystal.palace_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "crystal palace", MeanTotal + crystal.palace_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "everton", MeanTotal + everton_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "everton", MeanTotal + everton_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "fulham", MeanTotal + fulham_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "fulham", MeanTotal + fulham_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "huddersfield", MeanTotal + huddersfield_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "huddersfield", MeanTotal + huddersfield_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "leicester", MeanTotal + leicester_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "leicester", MeanTotal + leicester_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "liverpool", MeanTotal + liverpool_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "liverpool", MeanTotal + liverpool_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "manchester city", MeanTotal + manchester.city_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "manchester city", MeanTotal + manchester.city_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "manchester united", MeanTotal + manchester.united_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "manchester united", MeanTotal + manchester.united_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "newcastle utd", MeanTotal + newcastle.utd_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "newcastle utd", MeanTotal + newcastle.utd_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "southampton", MeanTotal + southampton_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "southampton", MeanTotal + southampton_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "tottenham", MeanTotal + tottenham_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "tottenham", MeanTotal + tottenham_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "watford", MeanTotal + watford_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "watford", MeanTotal + watford_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "west ham", MeanTotal + west.ham_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "west ham", MeanTotal + west.ham_away_goal_mean, MeanTotal)]

matches_2018_2019[, MeanTotal := ifelse(home == "wolves", MeanTotal + wolves_home_goal_mean, MeanTotal)]
matches_2018_2019[, MeanTotal := ifelse(away == "wolves", MeanTotal + wolves_away_goal_mean, MeanTotal)]




matches_2018_2019[,OnlyHour:= substr(Date,12,13)]
matches_2018_2019[,OnlyHour:= as.numeric(OnlyHour)]
matches_2018_2019[,EveningMatch:= ifelse(OnlyHour >= 20, 1, 0)]
matches_2018_2019[,MorningMatch:= ifelse(OnlyHour < 17, 1, 0)]


matches_2018_2019[,MatchMonth:= substr(Date,6,7)]
matches_2018_2019[,MatchMonth:= as.numeric(MatchMonth)]
matches_2018_2019[,BegOfSeason := ifelse(MatchMonth == 8 | MatchMonth == 9 | MatchMonth == 10, 1, 0)]
matches_2018_2019[,MidOfSeason := ifelse(MatchMonth == 11 | MatchMonth == 12 | MatchMonth == 1 | MatchMonth == 2, 1, 0)]
matches_2018_2019[,EndOfSeason := ifelse(MatchMonth == 3 | MatchMonth == 4 | MatchMonth == 5, 1, 0)]



matches_2018_2019[,Date:=NULL]
matches_2018_2019[,date:=NULL]
matches_2018_2019[,type:=NULL]
matches_2018_2019[,leagueId:=NULL]
matches_2018_2019[,score:=NULL]
matches_2018_2019[,HomeGoal:=NULL]
matches_2018_2019[,AwayGoal:=NULL]
matches_2018_2019[,OnlyHour:=NULL]
matches_2018_2019[,MatchMonth:=NULL]


##DATA DUPLUCATION

matches_for_ou_bets <- copy(matches)
matches_2018_2019_for_ou_bets <- copy(matches_2018_2019)

matches[, matchId:= NULL]
matches[, home:= NULL]
matches[, away:= NULL]

matches_2018_2019[, matchId:= NULL]
matches_2018_2019[, home:= NULL]
matches_2018_2019[, away:= NULL]


##INVERSE OVER ODDS
matches[, over:=1/over*10]
matches[, over:=over**2]


##MODELLER

model.1<- lm(Total~ -1 +EveningMatch +MorningMatch +over +MeanTotal +BegOfSeason +MidOfSeason +EndOfSeason , data = matches)
model.1
summary(model.1)
plot(model.1)
anova(model.1)

model.1_stepwise<-step(model.1,direction="backward")
model.1_stepwise
summary(model.1_stepwise)
plot(model.1_stepwise)
anova(model.1_stepwise)


predicted=predict(model.1_stepwise,matches_2018_2019)
summary(predicted)
plot(predicted, matches_2018_2019$Total)



Error_model_with_predicted = matches_2018_2019 - predicted
SSE = sum(Error_model_with_predicted$Total**2)

k=length(model.1_stepwise$coefficients)-1 #Subtract one to ignore intercept
SS2E=sum(model.1_stepwise$residuals**2)
n=length(model.1_stepwise$residuals)
Residual_Standart_Error = sqrt(SS2E/(n-(1+k))) #Residual Standard Error



##ÖNEMLİ!!!!! Her x için r hesaplama

temp1 <- copy(matches)
temp1[,over:=NULL]
temp1[,BegOfSeason:=NULL]
temp1[,EndOfSeason:=NULL]
temp1[,MidOfSeason:=NULL]
temp1[,MeanTotal:=NULL]
model_evening_morning <- lm(Total~ -1 +EveningMatch +MorningMatch, data = temp1)
summary(model_evening_morning)

temp2 <- copy(matches)
temp2[,EveningMatch:=NULL]
temp2[,MorningMatch:=NULL]
temp2[,BegOfSeason:=NULL]
temp2[,EndOfSeason:=NULL]
temp2[,MidOfSeason:=NULL]
temp2[,MeanTotal:=NULL]
model_over <- lm(Total~ -1 +over , data = temp2)
summary(model_over)

temp3 <- copy(matches)
temp3[,EveningMatch:=NULL]
temp3[,MorningMatch:=NULL]
temp3[,over:=NULL]
temp3[,MeanTotal:=NULL]
model_Part_of_Season <- lm(Total~ -1  +BegOfSeason +MidOfSeason +EndOfSeason , data = matches)
summary(model_Part_of_Season)

temp4 <- copy(matches)
temp4[,EveningMatch:=NULL]
temp4[,MorningMatch:=NULL]
temp4[,BegOfSeason:=NULL]
temp4[,EndOfSeason:=NULL]
temp4[,MidOfSeason:=NULL]
temp4[,over:=NULL]
model_TotalMeanGoal <- lm(Total~ -1  +MeanTotal , data = matches)
summary(model_TotalMeanGoal)


##RESIDUAL PLOTS

data_for_ou <- copy(matches) 

data_for_ou[, prediction:=predict(model.1_stepwise, matches)]

data_for_ou[, residuals:= Total - prediction]
plot(data_for_ou$prediction, data_for_ou$residuals)


##INTERVALS FOR OU ODDS

predicted_ou_odds <- copy(data_for_ou)
predicted_ou_odds[, predicted_ou:=0]
predicted_ou_odds[, real_ou:= ifelse(Total >= 2.5, 1, 0)]


j = 0

#ogün loop
prob_over <- array(0, dim=10) 
for(ou_numb in 25:35)
{
  predicted_ou_odds[, predicted_ou:= ifelse(prediction >= ou_numb/10, 1, 0)]
  temp <- copy(predicted_ou_odds)
  temp_sub <- subset(temp,predicted_ou==1)
  prob_over[j]= mean(temp_sub$real_ou)
  
  j=j+1
}

prob_over
predicted_ou_odds <- copy(data_for_ou)
predicted_ou_odds[, predicted_ou:=0]
predicted_ou_odds[, real_ou:= ifelse(Total >= 2.5, 1, 0)]

j = 0

prob_under <- array(0, dim=8) 
for(ou_numb in 21:29)
{
  predicted_ou_odds[, predicted_ou:= ifelse(prediction <= ou_numb/10, 1, 0)]
  temp <- copy(predicted_ou_odds)
  temp_sub <- subset(temp,predicted_ou==1)
  prob_under[j]= 1-mean(temp_sub$real_ou)
  j=j+1
}
prob_under


###2010-2018 OU ODDS AND PROFIT

matches_for_2010_2018 <- copy(matches_for_ou_bets)
matches_for_2010_2018[, Predicted_Goals := predict(model.1_stepwise, matches_for_2010_2018)]
matches_for_2010_2018[, Predicted_ou := ifelse(Predicted_Goals >=3.0, 1, ifelse(Predicted_Goals <= 2.3, 0, 13))] #If our odds is over it is coded as 1, if it is under it is coded as 0
matches_for_2010_2018[, real_ou:= ifelse(Total >=2.5, 1, 0)]

temp_2010_2018 <- copy(matches_for_2010_2018)

temp_overs_2010_2018 <- subset(temp_2010_2018, Predicted_ou==1)

total_exp_for_over_2010_2018 = nrow(temp_overs_2010_2018)
abcdef = subset(temp_overs_2010_2018, real_ou == 1)

total_income_for_over_2010_2018 = sum(abcdef$over)

total_profit_for_over_2010_2018 = total_income_for_over_2010_2018 - total_exp_for_over_2010_2018

#enes ;)
#for under bets
temp_another <- copy(matches_for_2010_2018)
temp_another[, under:= 1/(1-1/over)]

temp_unders_2010_2018 <- subset(temp_another, Predicted_ou == 0)

total_exp_for_under_2010_2018 = nrow(temp_unders_2010_2018)
klmn = subset(temp_unders_2010_2018, real_ou == 0)

total_income_for_under_2010_2018 = sum(klmn$under)

total_profit_for_under_2010_2018 = total_income_for_under_2010_2018 - total_exp_for_under_2010_2018

##TOTALPROFIT
Total_profit_2010_2018 = total_profit_for_over_2010_2018 + total_profit_for_under_2010_2018



#PLOT GRAPH FOR 2010-2018 PROFITS


temp_2010_2018_predicted_ou <- copy(matches_for_2010_2018)

temp_2010_2018_predicted_ou = merge(temp_2010_2018_predicted_ou, temp_matches_2010_2018, by = "matchId")

temp_2010_2018_predicted_ou[, profit:=ifelse(Predicted_ou==real_ou, over.x-1, 
                                             ifelse(Predicted_ou==13, 0, -1))]




temp_for_profits_only_2010_2018 <- copy(temp_2010_2018_predicted_ou)
temp_for_profits_only_2010_2018 <- subset(temp_for_profits_only_2010_2018, profit!=0)

plot(temp_for_profits_only_2010_2018$Date, temp_for_profits_only_2010_2018$profit)

mean_profit_2010_2018 = mean(temp_for_profits_only_2010_2018$profit)
abline(h=mean_profit_2010_2018, col="red")


temp_for_profits_only_2010_2018 <- temp_for_profits_only_2010_2018[order(date),]

temp_for_profits_only_2010_2018[, CumMean:= cumsum(profit) / seq_along(profit)]


plot(temp_for_profits_only_2010_2018$Date, temp_for_profits_only_2010_2018$CumMean, 
     xlim=c(min(temp_for_profits_only_2010_2018$Date), max(temp_for_profits_only_2010_2018$Date)), type = "o")






##SEASON 2018-2019
##OVER UNDER ODDS FOR SEASON 2018-2019 WITH RESPECT TO PREDCITIONS

matches_2018_2019_for_ou_bets[, Predicted_total_goals := predict(model.1_stepwise, matches_2018_2019_for_ou_bets)]

matches_2018_2019_for_ou_bets[, Predicted_ou_odds := ifelse(Predicted_total_goals >=3.0, 1, ifelse(Predicted_total_goals <= 2.3, 0, 13))] #If our odds is over it is coded as 1, if it is under it is coded as 0

matches_2018_2019_for_ou_bets[, real_ou:= ifelse(Total >=2.5, 1, 0)]

##CALCULATION OF PROFIT

#for over bets
temp123 <- copy(matches_2018_2019_for_ou_bets)

temp_overs <- subset(temp123, Predicted_ou_odds==1)

total_exp_for_over = nrow(temp_overs)
abc = subset(temp_overs, real_ou == 1)

total_income_for_over = sum(abc$over)

total_profit_for_over = total_income_for_over - total_exp_for_over

#for under bets
temp456 <- copy(matches_2018_2019_for_ou_bets)
temp456[, under:= 1/(1-1/over)]

temp_unders <- subset(temp456, Predicted_ou_odds == 0)

total_exp_for_under = nrow(temp_unders)
def = subset(temp_unders, real_ou == 0)

total_income_for_under = sum(def$under)

total_profit_for_under = total_income_for_under - total_exp_for_under

##TOTALPROFIT
Total_profit = total_profit_for_over + total_profit_for_under


#PLOT GRAPH


temp_2018_2019_predicted_ou <- copy(matches_2018_2019_for_ou_bets)

temp_2018_2019_predicted_ou = merge(temp_2018_2019_predicted_ou, temp_match_list, by = "matchId")

temp_2018_2019_predicted_ou[, profit:=ifelse(Predicted_ou_odds==real_ou, over.x-1, 
                                             ifelse(Predicted_ou_odds==13, 0, -1))]




temp_for_profits_only <- copy(temp_2018_2019_predicted_ou)
temp_for_profits_only <- subset(temp_for_profits_only, profit!=0)

plot(temp_for_profits_only$Date, temp_for_profits_only$profit)

mean_profit = mean(temp_for_profits_only$profit)
abline(h=mean_profit, col="red")


temp_for_profits_only <- temp_for_profits_only[order(date),]

temp_for_profits_only[, CumMean:= cumsum(profit) / seq_along(profit)]


plot(temp_for_profits_only$Date, temp_for_profits_only$CumMean, 
                    xlim=c(min(temp_for_profits_only$Date), max(temp_for_profits_only$Date)), type = "o")





