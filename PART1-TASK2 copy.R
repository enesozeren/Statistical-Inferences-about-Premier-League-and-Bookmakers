library(data.table)
library(lubridate)
library(gridExtra)
setwd("/Users/enesozeren/Downloads/")
odds=readRDS("df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds")
matches=readRDS("df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds")

matches$leagueId=NULL
matches$type=NULL
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

matches[,MatchResult:=ifelse(HomeGoal>AwayGoal,'home',ifelse(HomeGoal==AwayGoal,'draw','away'))]

matches[,timestamp:=as_datetime(date,tz='Turkey')]
matches[,date_of_match:=date(timestamp)]
matches[,hour_of_match:=hour(timestamp)]

#ProjectPart1_Task2
#Bookmaker 1

filtered_odds=odds[betType=='1x2' & bookmaker=='Pinnacle']
filtered_odds[,c('betType','bookmaker','totalhandicap'):=NULL]
filtered_odds=filtered_odds[order(matchId, oddtype,date)]
latest_odds=filtered_odds[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
help(dcast)
latest_odds=dcast(latest_odds,matchId~oddtype,value.var='final_odd')
temp=matches[,list(matchId,date_of_match,home,away,MatchResult)]
matches_with_odds=merge(temp,latest_odds,by='matchId')
summary_odds_by_result=matches_with_odds[,list(mean_home=mean(odd1),
                                              mean_draw=mean(oddX),mean_away=mean(odd2),.N),by=list(MatchResult)]

matches_with_odds[,prob_home:=1/odd1]
matches_with_odds[,prob_draw:=1/oddX]
matches_with_odds[,prob_away:=1/odd2]

matches_with_odds[,total_prob:=prob_home+prob_draw+prob_away]
matches_with_odds[,home_away_diff:=prob_home-prob_away]

plot(matches_with_odds[,list(home_away_diff,prob_draw)])
cut_levels=c(-20:20)/20

matches_with_odds[,diff_bucket:=cut(home_away_diff,cut_levels)]

result_summary=matches_with_odds[,list(real_draw_ratio=sum(MatchResult=='draw', na.rm = T)/.N,draw_prob_bookmaker=mean(prob_draw[MatchResult=='draw'], na.rm = T)),by=list(diff_bucket)]

#Task2.2 Real Probabilities of bookmakers
matches_with_odds[,P_home:=prob_home/total_prob]
matches_with_odds[,P_away:=prob_away/total_prob]
matches_with_odds[,P_draw:=prob_draw/total_prob]
matches_with_odds[,P_home_away_diff:=P_home-P_away]

P_summary_odds_by_result=matches_with_odds[,list(mean_home=mean(P_home),
                                               mean_draw=mean(P_draw),mean_away=mean(P_away),.N),by=list(MatchResult)]


plot(matches_with_odds[,list(P_home_away_diff,P_draw)])

matches_with_odds[,P_diff_bucket:=cut(P_home_away_diff,cut_levels)]

P_result_summary=matches_with_odds[,list(real_draw_ratio=sum(MatchResult=='draw', na.rm = T)/.N,P_draw_prob_bookmaker=mean(P_draw[MatchResult=='draw'], na.rm = T)),by=list(diff_bucket)]


#Bookmaker 2
filtered_odds2=odds[betType=='1x2' & bookmaker=='10Bet']
filtered_odds2[,c('betType','bookmaker','totalhandicap'):=NULL]
filtered_odds2=filtered_odds2[order(matchId, oddtype,date)]
latest_odds2=filtered_odds2[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_odds2=dcast(latest_odds2,matchId~oddtype,value.var='final_odd')

temp2=matches[,list(matchId,date_of_match,home,away,MatchResult)]
matches_with_odds2=merge(temp2,latest_odds2,by='matchId')
summary_odds_by_result2=matches_with_odds2[,list(mean_home=mean(odd1),
                                               mean_draw=mean(oddX),mean_away=mean(odd2),.N),by=list(MatchResult)]

matches_with_odds2[,prob_home:=1/odd1]
matches_with_odds2[,prob_draw:=1/oddX]
matches_with_odds2[,prob_away:=1/odd2]

matches_with_odds2[,total_prob:=prob_home+prob_draw+prob_away]
matches_with_odds2[,home_away_diff:=prob_home-prob_away]

plot(matches_with_odds2[,list(home_away_diff,prob_draw)])
matches_with_odds2[,diff_bucket:=cut(home_away_diff,cut_levels)]

result_summary2=matches_with_odds2[,list(real_draw_ratio=sum(MatchResult=='draw', na.rm = T)/.N,draw_prob_bookmaker=mean(prob_draw[MatchResult=='draw'], na.rm = T)),by=list(diff_bucket)]

#Task2.2 Bookmaker2
matches_with_odds2[,P_home:=prob_home/total_prob]
matches_with_odds2[,P_away:=prob_away/total_prob]
matches_with_odds2[,P_draw:=prob_draw/total_prob]
matches_with_odds2[,P_home_away_diff:=P_home-P_away]

P_summary_odds_by_result2=matches_with_odds2[,list(mean_home=mean(P_home),
                                                 mean_draw=mean(P_draw),mean_away=mean(P_away),.N),by=list(MatchResult)]

plot(matches_with_odds2[,list(P_home_away_diff,P_draw)])

matches_with_odds2[,P_diff_bucket:=cut(P_home_away_diff,cut_levels)]
P_result_summary2=matches_with_odds2[,list(real_draw_ratio=sum(MatchResult=='draw', na.rm = T)/.N,P_draw_prob_bookmaker=mean(P_draw[MatchResult=='draw'], na.rm = T)),by=list(diff_bucket)]


#Bookmaker 3
filtered_odds3=odds[betType=='1x2' & bookmaker=='Betsafe']
filtered_odds3[,c('betType','bookmaker','totalhandicap'):=NULL]
filtered_odds3=filtered_odds3[order(matchId, oddtype,date)]
latest_odds3=filtered_odds3[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_odds3=dcast(latest_odds3,matchId~oddtype,value.var='final_odd')

temp3=matches[,list(matchId,date_of_match,home,away,MatchResult)]
matches_with_odds3=merge(temp3,latest_odds3,by='matchId')
summary_odds_by_result3=matches_with_odds3[,list(mean_home=mean(odd1),
                                                 mean_draw=mean(oddX),mean_away=mean(odd2),.N),by=list(MatchResult)]

matches_with_odds3[,prob_home:=1/odd1]
matches_with_odds3[,prob_draw:=1/oddX]
matches_with_odds3[,prob_away:=1/odd2]

matches_with_odds3[,total_prob:=prob_home+prob_draw+prob_away]
matches_with_odds3[,home_away_diff:=prob_home-prob_away]

plot(matches_with_odds3[,list(home_away_diff,prob_draw)])
matches_with_odds3[,diff_bucket:=cut(home_away_diff,cut_levels)]

result_summary3=matches_with_odds3[,list(real_draw_ratio=sum(MatchResult=='draw', na.rm = T)/.N,draw_prob_bookmaker=mean(prob_draw[MatchResult=='draw'], na.rm = T)),by=list(diff_bucket)]

#Task2.2 Bookmaker3
matches_with_odds3[,P_home:=prob_home/total_prob]
matches_with_odds3[,P_away:=prob_away/total_prob]
matches_with_odds3[,P_draw:=prob_draw/total_prob]
matches_with_odds3[,P_home_away_diff:=P_home-P_away]

P_summary_odds_by_result3=matches_with_odds3[,list(mean_home=mean(P_home),
                                                   mean_draw=mean(P_draw),mean_away=mean(P_away),.N),by=list(MatchResult)]

plot(matches_with_odds3[,list(P_home_away_diff,P_draw)])

matches_with_odds3[,P_diff_bucket:=cut(P_home_away_diff,cut_levels)]
P_result_summary3=matches_with_odds3[,list(real_draw_ratio=sum(MatchResult=='draw', na.rm = T)/.N,P_draw_prob_bookmaker=mean(P_draw[MatchResult=='draw'], na.rm = T)),by=list(diff_bucket)]

#Bookmaker 4
filtered_odds4=odds[betType=='1x2' & bookmaker=='bet365']
filtered_odds4[,c('betType','bookmaker','totalhandicap'):=NULL]
filtered_odds4=filtered_odds4[order(matchId, oddtype,date)]
latest_odds4=filtered_odds4[,list(final_odd=odd[.N]),by=list(matchId,oddtype)]
latest_odds4=dcast(latest_odds4,matchId~oddtype,value.var='final_odd')

temp4=matches[,list(matchId,date_of_match,home,away,MatchResult)]
matches_with_odds4=merge(temp4,latest_odds4,by='matchId')
summary_odds_by_result4=matches_with_odds4[,list(mean_home=mean(odd1),
                                                 mean_draw=mean(oddX),mean_away=mean(odd2),.N),by=list(MatchResult)]

matches_with_odds4[,prob_home:=1/odd1]
matches_with_odds4[,prob_draw:=1/oddX]
matches_with_odds4[,prob_away:=1/odd2]

matches_with_odds4[,total_prob:=prob_home+prob_draw+prob_away]
matches_with_odds4[,home_away_diff:=prob_home-prob_away]

plot(matches_with_odds4[,list(home_away_diff,prob_draw)])
matches_with_odds4[,diff_bucket:=cut(home_away_diff,cut_levels)]

result_summary4=matches_with_odds4[,list(real_draw_ratio=sum(MatchResult=='draw', na.rm = T)/.N,draw_prob_bookmaker=mean(prob_draw[MatchResult=='draw'], na.rm = T)),by=list(diff_bucket)]

#Task2.2 Bookmaker4
matches_with_odds4[,P_home:=prob_home/total_prob]
matches_with_odds4[,P_away:=prob_away/total_prob]
matches_with_odds4[,P_draw:=prob_draw/total_prob]
matches_with_odds4[,P_home_away_diff:=P_home-P_away]

P_summary_odds_by_result4=matches_with_odds4[,list(mean_home=mean(P_home),
                                                   mean_draw=mean(P_draw),mean_away=mean(P_away),.N),by=list(MatchResult)]

plot(matches_with_odds4[,list(P_home_away_diff,P_draw)])

matches_with_odds4[,P_diff_bucket:=cut(P_home_away_diff,cut_levels)]
P_result_summary4=matches_with_odds4[,list(real_draw_ratio=sum(MatchResult=='draw', na.rm = T)/.N,P_draw_prob_bookmaker=mean(P_draw[MatchResult=='draw'], na.rm = T)),by=list(diff_bucket)]


#Tables
names(P_result_summary)[1]<-("Difference Bucket")
names(P_result_summary)[2]<-("Real Draw Ratio")
names(P_result_summary)[3]<-("Normalized Bookmaker Draw Ratio")

grid.table(P_result_summary)
plot(P_result_summary$`Difference Bucket`, 
     P_result_summary$`Real Draw Ratio` - P_result_summary$`Normalized Bookmaker Draw Ratio`,
     xlab ="Difference Buckets", ylab = "Real Draw Ratio - Normalized Bookmaker Draw Ratio", 
     main = "Pinnacle Ratio and Real Ratio")


names(P_result_summary2)[1]<-("Difference Bucket")
names(P_result_summary2)[2]<-("Real Draw Ratio")
names(P_result_summary2)[3]<-("Normalized Bookmaker Draw Ratio")

grid.table(P_result_summary2)
plot(P_result_summary2$`Difference Bucket`, 
     P_result_summary2$`Real Draw Ratio` - P_result_summary2$`Normalized Bookmaker Draw Ratio`,
     xlab ="Difference Buckets", ylab = "Real Draw Ratio - Normalized Bookmaker Draw Ratio", 
     main = "10Bet Ratio and Real Ratio")


names(P_result_summary3)[1]<-("Difference Bucket")
names(P_result_summary3)[2]<-("Real Draw Ratio")
names(P_result_summary3)[3]<-("Normalized Bookmaker Draw Ratio")

grid.table(P_result_summary3)
plot(P_result_summary3$`Difference Bucket`, 
     P_result_summary3$`Real Draw Ratio` - P_result_summary3$`Normalized Bookmaker Draw Ratio`,
     xlab ="Difference Buckets", ylab = "Real Draw Ratio - Normalized Bookmaker Draw Ratio", 
     main = "Betsafe Ratio and Real Ratio")


names(P_result_summary4)[1]<-("Difference Bucket")
names(P_result_summary4)[2]<-("Real Draw Ratio")
names(P_result_summary4)[3]<-("Normalized Bookmaker Draw Ratio")

grid.table(P_result_summary4)
plot(P_result_summary4$`Difference Bucket`, 
     P_result_summary4$`Real Draw Ratio` - P_result_summary4$`Normalized Bookmaker Draw Ratio`,
     xlab ="Difference Buckets", ylab = "Real Draw Ratio - Normalized Bookmaker Draw Ratio", 
     main = "bet365 Ratio and Real Ratio")

