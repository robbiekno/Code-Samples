require(RSQLite)
require(DBI)
library(pROC)
require(rethinking)
require(dplyr)

#It wasn't too difficult to figure out how to use this
drv <- dbDriver("SQLite")

db <- dbConnect(drv, dbname = "acc1819.db")
dbListTables(db)

#I can't say I am showing off my SQL skills at all with these queries.
#I definitely could have done more of the data manipulation in SQL and maybe I would have if I was
#on SQL Server, but if I'm doing the SQL in R anyway, I'd rather just do the manipulations in R
res <- dbSendQuery(db, "SELECT * FROM box_scores")
boxscores<-dbFetch(res)
str(boxscores)

res2 <- dbSendQuery(db, "SELECT * FROM games")
games<-dbFetch(res2)
str(games)

table(games$HomeTeam)
table(games$AwayTeam)

head(games)
head(games[games$NeutralSite==1,])
table(games$GameDate[games$NeutralSite==1])

head(boxscores)

#Column names starting with numbers are always a bit tricky
colnames(boxscores)[14:15]<-c("FG3M","FG3A")

sum(games$NeutralSite==1)

#Joining in the other team to get game results, and mixing up Home/Away so the join works
scoreOther<-boxscores[,c("GameId","Team","Home","Score")]
colnames(scoreOther)[c(2,4)]<-c("Opponent","OppScore")
scoreOther$Home<-with(scoreOther,ifelse(Home==1,0,1))
boxscores<-left_join(boxscores,scoreOther,by=c("GameId","Home"))

head(boxscores)

#Adding in whether a game was on a neutral site, though the sample was too small for it to matter in the
#end
boxscores<-left_join(boxscores,games[,c(1,3)],by=c("GameId"))

#Checking who won and getting the point difference, both of which will be huge in the end
boxscores$Win<-with(boxscores,ifelse(Score>OppScore,1,0))
boxscores$PointDifference<-with(boxscores,Score-OppScore)

#Glancing at the standing and other metrics and making sure I have things in a decent place
standings<-summarise(group_by(boxscores,Team),
                     Wins = sum(Win),
                     Losses = sum(Win==0),
                     WPerc = Wins/(Wins+Losses),
                     Count = n(),
                     PtDiff = sum(PointDifference),
                     PtDiffPG = mean(PointDifference),
                     PPG = mean(Score),
                     PPGAgainst = mean(OppScore)
)
standings<-standings[order(-standings$WPerc),]
head(standings)

print.data.frame(standings)

sum(standings$Wins+standings$Losses)
nrow(boxscores)

#We have some neutral site games, which in theory could be the testing, but 95% training and 5% testing
#isn't going to work for this. I'm going to assume that we're going to factor those in and try to come up
#with an ACC ranking including the conference tournament (the sort of model which could be used for teams 
#across conferences to predict NCAA tournament results or who should objectively make the tournament)

head(boxscores)

#Gave days rest a try, though it didn't pan out
head(games)
teams<-unique(games$AwayTeam)
restDF<-data.frame(Team = NA,
                   GameId = NA,
                   DaysRest = NA)
for(i in 1:length(teams)){
  toUse<-games[games$AwayTeam==teams[i]|games$HomeTeam==teams[i],]
  DFforJoin<-data.frame(Team=rep(teams[i]),
                        GameId = toUse$GameId)
  toUse$Date<-as.Date(sapply(toUse$GameDate,function(x){unlist(strsplit(as.character(x), " "))[1]}),format = "%m/%d/%y")
  DFforJoin$DaysRest<-c(NA,sapply(2:nrow(toUse),function(i){difftime(toUse$Date[i],toUse$Date[i-1],units="days")}))
  restDF<-bind_rows(restDF,DFforJoin)
}

summary(restDF$DaysRest)
restDF$DaysRest[is.na(restDF$DaysRest)]<-4

restDFAway<-restDF
colnames(restDFAway)[3]<-"DaysRestAway"
restDFHome<-restDF
colnames(restDFHome)[3]<-"DaysRestHome"
boxscores<-left_join(boxscores,restDFHome,by=c("Team","GameId"))
head(boxscores)
boxscores<-left_join(boxscores,restDFAway,by=c("Opponent"="Team","GameId"))

head(boxscores[boxscores$DaysRestHome>boxscores$DaysRestAway,])

#Which things tend to make teams better or worse than others?
#(I wish the games table had whether a game went into overtime - that throws off a few things)
#For the most part, those things are all points-based - points for you compared to points for your 
#opponents.

#Everything else just affects the odds of a worse team beating a good one -
#like the strong 3-point shooting team or the strong defensive team will still probably lose to 
#the better team, but those traits give them more of a chance
#But maybe not, thinking about the better teams - maybe some ways of winning are better than others
#Though we still may see that in terms of performance compared to where your opponents are typically

#The goal is also to have a ranking among all teams - if it was to find the odds for each team against 
#each other, that would also be more relevant

#Types of teams that might outperform or under-perform their point differential:
#Good 3-point shooting teams (% and attempts)
#Strong rebounding teams (total and ORB)
#Teams that get to the free-throw line (FTA) / allow a lot of free throws
#Teams who rarely turn the ball over or allow a lot

#Going to use Bayesian models, allowing us to gain a better conceptual understanding of the problem and
#potentially simulate later, and we also have to worry about over-fitting in this small of a sample, so
#I wouldn't love XGBoost here.
#There's also the concept that we're trying to have a ranking where each team would be better than the
#one below it, so the goal isn't to dive too much into matchup-based things that could be specific to 
#two things playing each other
#I didn't really get into specific matchup results between two teams for that reason (but definitely
#could have looked into it more for a different model)
#It's also good to more concretely test hypotheses in this case, especially because we know that point
#differential is such a powerful variable if we're looking at how good a team is generally and we need
#to be cautious adding in something about that, especially in this fairly small sample of games we have

#I decided to do things on the game level rather than the season level as to not reduce my sample size
#by any more and because season-level things were going to lead to a lot of ambiguity in terms of the
#effects of home, road, and neutral site. I did give beta-binomial Bayesian models a long look and had
#some code I was getting ready to adapt.

#Let's take some samples of the data and see which model is going to be the best.
#After some initial exploration, I did three main rounds of model comparisons: 
#1) just points-based models
#2) adding in additional statistics (threes (% and attempts), rebounds (overall and Offensive),
#FTs, and turnover), trying to get at things which may hint at team skill beyond point differential
#3) A final comparison of different models that looked interesting

#With that in mind, I ran the code below 3 times with different things commented out.
#I'll add in comments within the loop to explain a few things

# evalDF <- expand.grid(
#   Round = 1:10,
#   Model = c(2,4,5,6),
#   PSIS = rep(NA),
#   EffParam = rep(NA),
#   AUC = rep(NA),
#   RMSE = rep(NA)
# )

# evalDF <- expand.grid(
#   Round = 1:5,
#   Model = c(5,9:16),
#   PSIS = rep(NA),
#   EffParam = rep(NA),
#   AUC = rep(NA),
#   RMSE = rep(NA)
# )


evalDF <- expand.grid(
  Round = 1:10,
  Model = c(5,11,14:16),
  PSIS = rep(NA),
  EffParam = rep(NA),
  AUC = rep(NA),
  RMSE = rep(NA)
)


for(j in 1:10){
  #I realized pretty quickly that I had to sample from games rather than rows of the data or things
  #got pretty weird
  buildSample<-sample(1:length(unique(boxscores$GameId)),round(length(unique(boxscores$GameId))*0.5,0),replace=F)
  gamesToUse<-boxscores[boxscores$GameId %in% buildSample&boxscores$Home==1,]
  
  #Metrics of possible interest that I can calculate on the team level
  standingsBuild<-summarise(group_by(boxscores[boxscores$GameId %in% c(buildSample),],Team),
                            Wins = sum(Win),
                            Losses = sum(Win==0),
                            WPerc = Wins/(Wins+Losses),
                            Count = n(),
                            PtDiff = sum(PointDifference),
                            PtDiffPG = mean(PointDifference),
                            PPG = mean(Score),
                            PPGAgainst = mean(OppScore),
                            Att3PG = mean(FG3A),
                            Perc3 = sum(FG3M)/sum(FG3A),
                            RPG = mean(Rebounds),
                            ORPG = mean(ORB),
                            FTPG = mean(FTA),
                            TPG = mean(TOV),
                            PtDiffPGSplit = rep(NA)
  )
  
  standingsBuild<-standingsBuild[order(-standingsBuild$WPerc),]
  head(standingsBuild)
  
  standingsBuild$WinTeamsPtDiff<-rep(NA)
  standingsBuild$LossTeamsPtDiff<-rep(NA)
  #Initialize the columns of the dataframe to join in
  standingsForJoin<-standingsBuild[0,]
  
  for(i in 1:nrow(gamesToUse)){
    #In this loop, I get all of the relevant team information, excluding the row in question
    #I decided not to do much of anything with ordering a team's games by date and to predict each game
    #in the build based on the information from all other games in the build
    #I thought about it - especially because young teams may get better as the year goes on -
    #but there are also injuries and other things that make things more complicated.
    #We also don't have non-conference games and don't have individual player stats
    #Lastly, the prompt wasn't specific about this - if it said that each team should be a favorite over
    #the subsequent one at the end of the season, that could also potentially change things
    gamesForRow<-boxscores[boxscores$GameId %in% buildSample&boxscores$GameId!=gamesToUse$GameId[i],]
    
    standingsBuild<-summarise(group_by(gamesForRow,Team),
                              Wins = sum(Win),
                              Losses = sum(Win==0),
                              WPerc = Wins/(Wins+Losses),
                              Count = n(),
                              PtDiff = sum(PointDifference),
                              PtDiffPG = mean(PointDifference),
                              PPG = mean(Score),
                              PPGAgainst = mean(OppScore),
                              Att3PG = mean(FG3A),
                              Perc3 = sum(FG3M)/sum(FG3A),
                              RPG = mean(Rebounds),
                              ORPG = mean(ORB),
                              FTPG = mean(FTA),
                              TPG = mean(TOV),
                              PtDiffPGSplit = mean(PointDifference[Home==gamesToUse$Home[i]])
    )
    
    standingsBuild$WinTeamsPtDiff<-rep(NA)
    standingsBuild$LossTeamsPtDiff<-rep(NA)
    
    #This section of the code gets the point differential when they weren't playing team i of all teams
    #they beat and all teams they lost to. The point differential of the teams that each team lost to
    #wound up being a meaningful variable
    opponents<-gamesForRow[gamesForRow$Opponent==gamesToUse$Team[i],]
    adjustedPointDifference<-summarise(group_by(gamesForRow,Team),
                                       PointDiffAdj = sum(PointDifference[Opponent!=gamesToUse$Team[i]]),
                                       PointsAdj = sum(Score[Opponent!=gamesToUse$Team[i]]),
                                       PointsAllowedAdj = sum(OppScore[Opponent!=gamesToUse$Team[i]]),
                                       GameCount = n())
    opponents<-left_join(opponents,adjustedPointDifference,by=c("Team"))
    #Reversed because from perspective of opponent
    standingsBuild$WinTeamsPtDiff[which(standingsBuild$Team==gamesToUse$Team[i])]<-sum(opponents$PointDiffAdj[opponents$Win==0])/sum(opponents$GameCount[opponents$Win==0])
    standingsBuild$LossTeamsPtDiff[which(standingsBuild$Team==gamesToUse$Team[i])]<-sum(opponents$PointDiffAdj[opponents$Win==1])/sum(opponents$GameCount[opponents$Win==1])
    
    standingsForJoin<-bind_rows(standingsForJoin,standingsBuild[is.na(standingsBuild$LossTeamsPtDiff)==F|is.na(standingsBuild$WinTeamsPtDiff)==F,])
  }
  
  #This loop yielded one row for the correct home team in each game of the sample
  
  # This whole next section through the end of the loop did the same thing to get information for the 
  #away team
  standingsForJoinOpp<-standingsBuild[0,]
  
  colnames(gamesToUse)
  for(i in 1:nrow(gamesToUse)){
    gamesForRow<-boxscores[boxscores$GameId %in% buildSample&boxscores$GameId!=gamesToUse$GameId[i],]
    
    standingsBuild<-summarise(group_by(gamesForRow,Team),
                              Wins = sum(Win),
                              Losses = sum(Win==0),
                              WPerc = Wins/(Wins+Losses),
                              Count = n(),
                              PtDiff = sum(PointDifference),
                              PtDiffPG = mean(PointDifference),
                              PPG = mean(Score),
                              PPGAgainst = mean(OppScore),
                              Att3PG = mean(FG3A),
                              Perc3 = sum(FG3M)/sum(FG3A),
                              RPG = mean(Rebounds),
                              ORPG = mean(ORB),
                              FTPG = mean(FTA),
                              TPG = mean(TOV),
                              PtDiffPGSplit = mean(PointDifference[Home!=gamesToUse$Home[i]])
    )
    
    standingsBuild$WinTeamsPtDiff<-rep(NA)
    standingsBuild$LossTeamsPtDiff<-rep(NA)
    
    opponents<-gamesForRow[gamesForRow$Opponent==gamesToUse$Opponent[i],]
    adjustedPointDifference<-summarise(group_by(gamesForRow,Team),
                                       PointDiffAdj = sum(PointDifference[Opponent!=gamesToUse$Opponent[i]]),
                                       PointsAdj = sum(Score[Opponent!=gamesToUse$Opponent[i]]),
                                       PointsAllowedAdj = sum(OppScore[Opponent!=gamesToUse$Opponent[i]]),
                                       GameCount = n())
    opponents<-left_join(opponents,adjustedPointDifference,by=c("Team"))
    #Reversed because from perspective of opponent
    standingsBuild$WinTeamsPtDiff[which(standingsBuild$Team==gamesToUse$Opponent[i])]<-sum(opponents$PointDiffAdj[opponents$Win==0])/sum(opponents$GameCount[opponents$Win==0])
    standingsBuild$LossTeamsPtDiff[which(standingsBuild$Team==gamesToUse$Opponent[i])]<-sum(opponents$PointDiffAdj[opponents$Win==1])/sum(opponents$GameCount[opponents$Win==1])
    
    standingsForJoinOpp<-bind_rows(standingsForJoinOpp,standingsBuild[is.na(standingsBuild$LossTeamsPtDiff)==F|is.na(standingsBuild$WinTeamsPtDiff)==F,])
  }
  
  str(standingsForJoin)
  str(standingsForJoinOpp)
  str(gamesToUse)
  colnames(standingsForJoin)
  
  #Making sure I have good home and away information for the predictions later
  forLater<-bind_rows(standingsForJoin,standingsForJoinOpp)
  forLater$IsHome<-rep(c(1,0),each=74)
  
  #Joining in the home team's relevant metrics
  gamesToUse<-cbind(gamesToUse,standingsForJoin[,-c(1:3,5)])
  head(gamesToUse)
  
  #Same for the away team
  colnames(standingsForJoinOpp)[2:18]<-paste0(colnames(standingsForJoinOpp)[2:18],"_Opp")
  gamesToUse<-cbind(gamesToUse,standingsForJoinOpp[,-c(1:3,5)])
  
  head(gamesToUse)
  
  summary(gamesToUse$WinTeamsPtDiff)
  summary(gamesToUse$WinTeamsPtDiff_Opp)
  
  summary(gamesToUse$LossTeamsPtDiff)
  summary(gamesToUse$LossTeamsPtDiff_Opp)
  
  colnames(gamesToUse)
  
  #Filling some things in just in case a team didn't have a win, a loss, or a home or road games in the
  #games sampled
  gamesToUse$WinTeamsPtDiff[is.na(gamesToUse$WinTeamsPtDiff)]<-mean(gamesToUse$WinTeamsPtDiff,na.rm=T)
  gamesToUse$WinTeamsPtDiff_Opp[is.na(gamesToUse$WinTeamsPtDiff_Opp)]<-mean(gamesToUse$WinTeamsPtDiff_Opp,na.rm=T)
  gamesToUse$LossTeamsPtDiff[is.na(gamesToUse$LossTeamsPtDiff)]<-mean(gamesToUse$LossTeamsPtDiff,na.rm=T)
  gamesToUse$LossTeamsPtDiff_Opp[is.na(gamesToUse$LossTeamsPtDiff_Opp)]<-mean(gamesToUse$LossTeamsPtDiff_Opp,na.rm=T)
  gamesToUse$PtDiffPGSplit[is.na(gamesToUse$PtDiffPGSplit)]<-mean(gamesToUse$PtDiffPGSplit,na.rm=T)
  gamesToUse$PtDiffPGSplit_Opp[is.na(gamesToUse$PtDiffPGSplit_Opp)]<-mean(gamesToUse$PtDiffPGSplit_Opp,na.rm=T)
  
  #The sample of metrics that I looked into and tried, including the commented out ones on at least a
  #preliminary basis.
  bayesianData<-with(gamesToUse,list(Win = Win,
                                     PtDiffPG = scale(PtDiffPG),
                                     PtDiffPG_Opp = scale(PtDiffPG_Opp),
                                     WPerc = scale(WPerc),
                                     WPerc_Opp = scale(WPerc_Opp),
                                     # PPG = scale(PPG),
                                     # PPG_Opp = scale(PPG_Opp),
                                     # PPGAgainst = scale(PPGAgainst),
                                     # PPGAgainst_Opp = scale(PPGAgainst_Opp),
                                     Att3PG = scale(Att3PG),
                                     Perc3 = scale(Perc3),
                                     RPG = scale(RPG),
                                     ORPG = scale(ORPG),
                                     FTPG = scale(FTPG),
                                     TPG = scale(TPG),
                                     Att3PG_Opp = scale(Att3PG_Opp),
                                     Perc3_Opp = scale(Perc3_Opp),
                                     RPG_Opp = scale(RPG_Opp),
                                     ORPG_Opp = scale(ORPG_Opp),
                                     FTPG_Opp = scale(FTPG_Opp),
                                     TPG_Opp = scale(TPG_Opp),
                                     PtDiffSplit = scale(PtDiffPGSplit-PtDiffPG),
                                     PtDiffSplit_Opp = scale(PtDiffPGSplit_Opp-PtDiffPG_Opp),
                                     # WinTeamsPtDiff = scale(WinTeamsPtDiff),
                                     LossTeamsPtDiff = scale(LossTeamsPtDiff),
                                     # WinTeamsPtDiff_Opp = scale(WinTeamsPtDiff_Opp),
                                     LossTeamsPtDiff_Opp = scale(LossTeamsPtDiff_Opp),
                                     IsNeutralSite = NeutralSite,
                                     Perc3FavAdj = scale(ifelse(PtDiffPG>PtDiffPG_Opp,Perc3,Perc3_Opp)),
                                     Perc3FavAdj_Opp = scale(ifelse(PtDiffPG>PtDiffPG_Opp,Perc3_Opp,Perc3))
                                     # DaysRestHome = scale(log(DaysRestHome)),
                                     # DaysRestAway = scale(log(DaysRestAway))
  ))
  
  #Below are a bunch of the different models that I really considered.
  #As I mentioned in the Predictive Modeling file, ulam is just a wrapper for Stan.
  #I've learned how to use Stan and acan specify models in it, but these ulam modeling objects are much
  #easy to use for predictions and other comparisons
  
  # bayesianMod2 <- ulam(
  #   alist(
  #     Win ~ dbinom( 1 , p ) ,
  #     logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp,
  #     a ~ dnorm( 0 , 1.5 ) ,
  #     b ~ dnorm( 0 , 1.5 ) ,
  #     c ~ dnorm( 0 , 1.5 )
  #   ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  # precis(bayesianMod2,depth=2)
  
  # bayesianMod4 <- ulam(
  #   alist(
  #     Win ~ dbinom( 1 , p ) ,
  #     logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*WPerc + e*WPerc_Opp,
  #     a ~ dnorm( 0 , 1.5 ) ,
  #     b ~ dnorm( 0 , 1.5 ) ,
  #     c ~ dnorm( 0 , 1.5 ) ,
  #     d ~ dnorm( 0 , 1.5 ) ,
  #     e ~ dnorm( 0 , 1.5 ) 
  #   ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  # precis(bayesianMod4,depth=2)
  
  #This is the model that I zoned in on pretty early, and after several challenges, nothing could
  #really beat it
  #I used that prior of dnorm(0,1.5) for everything because too high of a standard deviation would
  #yield bizarre results with the logit link (most of the density very close to 0 or 1)
  
  #In any event, the best way to determine the results of a game was to look at the point differentials
  #of both teams and the point differentials (excluding in games against them) for the teams that
  #each team lost to
  bayesianMod5 <- ulam(
    alist(
      Win ~ dbinom( 1 , p ) ,
      logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*LossTeamsPtDiff + e*LossTeamsPtDiff_Opp,
      a ~ dnorm( 0 , 1.5 ) ,
      b ~ dnorm( 0 , 1.5 ) ,
      c ~ dnorm( 0 , 1.5 ) ,
      d ~ dnorm( 0 , 1.5 ) ,
      e ~ dnorm( 0 , 1.5 ) 
    ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  precis(bayesianMod5,depth=2)
  
  #Going back to Mod2 and Mod4, Mod2 with just point differential wasn't quite enough (though its 
  #simplicity was great) and having WP% alongside point differential over-fit a bit
  
  #This version with point differential, WP%, and point differential of who you lost to wasn't quite good
  #enough
  # bayesianMod6 <- ulam(
  #   alist(
  #     Win ~ dbinom( 1 , p ) ,
  #     logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*WPerc + e*WPerc_Opp + f*LossTeamsPtDiff + g*LossTeamsPtDiff_Opp,
  #     a ~ dnorm( 0 , 1.5 ) ,
  #     b ~ dnorm( 0 , 1.5 ) ,
  #     c ~ dnorm( 0 , 1.5 ) ,
  #     d ~ dnorm( 0 , 1.5 ) ,
  #     e ~ dnorm( 0 , 1.5 ) ,
  #     f ~ dnorm( 0 , 1.5 ) ,
  #     g ~ dnorm( 0 , 1.5 ) 
  #   ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  # precis(bayesianMod6,depth=2)
  
  #There wasn't enough of a sample to find a real effect for neutral site
  # bayesianMod9 <- ulam(
  #   alist(
  #     Win ~ dbinom( 1 , p ) ,
  #     logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*LossTeamsPtDiff + e*LossTeamsPtDiff_Opp + f*IsNeutralSite,
  #     a ~ dnorm( 0 , 1.5 ) ,
  #     b ~ dnorm( 0 , 1.5 ) ,
  #     c ~ dnorm( 0 , 1.5 ) ,
  #     d ~ dnorm( 0 , 1.5 ) ,
  #     e ~ dnorm( 0 , 1.5 ) ,
  #     f ~ dnorm( 0 , 1.5 )
  #   ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  # precis(bayesianMod9,depth=2)
  
  #3 attempts never popped, but...
  # bayesianMod10 <- ulam(
  #   alist(
  #     Win ~ dbinom( 1 , p ) ,
  #     logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*LossTeamsPtDiff + e*LossTeamsPtDiff_Opp + f*Att3PG + g*Att3PG_Opp,
  #     a ~ dnorm( 0 , 1.5 ) ,
  #     b ~ dnorm( 0 , 1.5 ) ,
  #     c ~ dnorm( 0 , 1.5 ) ,
  #     d ~ dnorm( 0 , 1.5 ) ,
  #     e ~ dnorm( 0 , 1.5 ) ,
  #     f ~ dnorm( 0 , 1.5 ) ,
  #     g ~ dnorm( 0 , 1.5 ) 
  #   ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  # precis(bayesianMod10,depth=2)
  
  #I gave this one a long look and thought it might work, but it was still just over-fitting too much
  # bayesianMod11 <- ulam(
  #   alist(
  #     Win ~ dbinom( 1 , p ) ,
  #     logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*LossTeamsPtDiff + e*LossTeamsPtDiff_Opp + f*Perc3 + g*Perc3_Opp,
  #     a ~ dnorm( 0 , 1.5 ) ,
  #     b ~ dnorm( 0 , 1.5 ) ,
  #     c ~ dnorm( 0 , 1.5 ) ,
  #     d ~ dnorm( 0 , 1.5 ) ,
  #     e ~ dnorm( 0 , 1.5 ) ,
  #     f ~ dnorm( 0 , 1.5 ) ,
  #     g ~ dnorm( 0 , 1.5 ) 
  #   ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  # precis(bayesianMod11,depth=2)
  
  #Rebounds didn't pop
  # bayesianMod12 <- ulam(
  #   alist(
  #     Win ~ dbinom( 1 , p ) ,
  #     logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*LossTeamsPtDiff + e*LossTeamsPtDiff_Opp + f*RPG + g*RPG_Opp,
  #     a ~ dnorm( 0 , 1.5 ) ,
  #     b ~ dnorm( 0 , 1.5 ) ,
  #     c ~ dnorm( 0 , 1.5 ) ,
  #     d ~ dnorm( 0 , 1.5 ) ,
  #     e ~ dnorm( 0 , 1.5 ) ,
  #     f ~ dnorm( 0 , 1.5 ) ,
  #     g ~ dnorm( 0 , 1.5 ) 
  #   ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  # precis(bayesianMod12,depth=2)
  
  #Same with ORPG
  # bayesianMod13 <- ulam(
  #   alist(
  #     Win ~ dbinom( 1 , p ) ,
  #     logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*LossTeamsPtDiff + e*LossTeamsPtDiff_Opp + f*ORPG + g*ORPG_Opp,
  #     a ~ dnorm( 0 , 1.5 ) ,
  #     b ~ dnorm( 0 , 1.5 ) ,
  #     c ~ dnorm( 0 , 1.5 ) ,
  #     d ~ dnorm( 0 , 1.5 ) ,
  #     e ~ dnorm( 0 , 1.5 ) ,
  #     f ~ dnorm( 0 , 1.5 ) ,
  #     g ~ dnorm( 0 , 1.5 ) 
  #   ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  # precis(bayesianMod13,depth=2)
  
  #Same with free throws
  # bayesianMod14 <- ulam(
  #   alist(
  #     Win ~ dbinom( 1 , p ) ,
  #     logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*LossTeamsPtDiff + e*LossTeamsPtDiff_Opp + f*FTPG + g*FTPG_Opp,
  #     a ~ dnorm( 0 , 1.5 ) ,
  #     b ~ dnorm( 0 , 1.5 ) ,
  #     c ~ dnorm( 0 , 1.5 ) ,
  #     d ~ dnorm( 0 , 1.5 ) ,
  #     e ~ dnorm( 0 , 1.5 ) ,
  #     f ~ dnorm( 0 , 1.5 ) ,
  #     g ~ dnorm( 0 , 1.5 ) 
  #   ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  # precis(bayesianMod14,depth=2)
  
  #Here, I tried adjusting the 3P FG% so that it flipped depending on whether or not the home team was the
  #favorite. In theory, if the favorite is more 3-dependent, maybe they'll just have a bad night and lose
  #to a worse team. And an underdog with some 3-shooting ability might just make 50% of their threes for a
  #game. But it was not to be.
  bayesianMod14 <- ulam(
    alist(
      Win ~ dbinom( 1 , p ) ,
      logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*LossTeamsPtDiff + e*LossTeamsPtDiff_Opp + f*Perc3FavAdj + g*Perc3FavAdj_Opp,
      a ~ dnorm( 0 , 1.5 ) ,
      b ~ dnorm( 0 , 1.5 ) ,
      c ~ dnorm( 0 , 1.5 ) ,
      d ~ dnorm( 0 , 1.5 ) ,
      e ~ dnorm( 0 , 1.5 ) ,
      f ~ dnorm( 0 , 1.5 ) ,
      g ~ dnorm( 0 , 1.5 ) 
    ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  precis(bayesianMod14,depth=2)
  
  #Turnovers didn't pop
  # bayesianMod15 <- ulam(
  #   alist(
  #     Win ~ dbinom( 1 , p ) ,
  #     logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*LossTeamsPtDiff + e*LossTeamsPtDiff_Opp + f*TPG + g*TPG_Opp,
  #     a ~ dnorm( 0 , 1.5 ) ,
  #     b ~ dnorm( 0 , 1.5 ) ,
  #     c ~ dnorm( 0 , 1.5 ) ,
  #     d ~ dnorm( 0 , 1.5 ) ,
  #     e ~ dnorm( 0 , 1.5 ) ,
  #     f ~ dnorm( 0 , 1.5 ) ,
  #     g ~ dnorm( 0 , 1.5 ) 
  #   ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  # precis(bayesianMod15,depth=2)
  
  #I gave these next two also a long look. This model looks at whether how good the away team has been 
  #on the road could be meaningful
  bayesianMod15 <- ulam(
    alist(
      Win ~ dbinom( 1 , p ) ,
      logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*LossTeamsPtDiff + e*LossTeamsPtDiff_Opp + f*PtDiffSplit_Opp,
      a ~ dnorm( 0 , 1.5 ) ,
      b ~ dnorm( 0 , 1.5 ) ,
      c ~ dnorm( 0 , 1.5 ) ,
      d ~ dnorm( 0 , 1.5 ) ,
      e ~ dnorm( 0 , 1.5 ) ,
      f ~ dnorm( 0 , 1.5 ) 
    ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  precis(bayesianMod15,depth=2)
  
  #And this one has team-specific effects for home and road, but also was over-fitting
  bayesianMod16 <- ulam(
    alist(
      Win ~ dbinom( 1 , p ) ,
      logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*LossTeamsPtDiff + e*LossTeamsPtDiff_Opp + f*PtDiffSplit + g*PtDiffSplit_Opp,
      a ~ dnorm( 0 , 1.5 ) ,
      b ~ dnorm( 0 , 1.5 ) ,
      c ~ dnorm( 0 , 1.5 ) ,
      d ~ dnorm( 0 , 1.5 ) ,
      e ~ dnorm( 0 , 1.5 ) ,
      f ~ dnorm( 0 , 1.5 ) ,
      g ~ dnorm( 0 , 1.5 )
    ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
  precis(bayesianMod16,depth=2)
  
  # comp<-compare(bayesianMod2,bayesianMod4,bayesianMod5,bayesianMod6,func=PSIS)
  # comp<-compare(bayesianMod5,bayesianMod9,bayesianMod10,bayesianMod11,bayesianMod12,bayesianMod13,
  #               bayesianMod14,bayesianMod15,bayesianMod16,func=PSIS)
  
  #I compared the models in a few ways. The first was PSIS, an efficient cross-validation methodology within
  #the build that penalizes for too many variables.
  comp<-compare(bayesianMod5,bayesianMod11,
                bayesianMod14,bayesianMod15,bayesianMod16,func=PSIS)
  
  #Getting the prediction dataframe ready.
  #I thought that doing things this way would help metrics that weren't as points-based because maybe
  #we can look at things that predict point differential beyond just point differential, but it wasn't to be
  gamesToPredict<-boxscores[boxscores$GameId %in% buildSample==F&boxscores$Home==1,]
  
  # teamInfoForJoin<-summarise(group_by(gamesToUse,Team),
  #                            PtDiffPG = mean(PtDiffPG),
  #                            WPerc = mean(WPerc),
  #                            LossTeamsPtDiff = mean(LossTeamsPtDiff))
  
  teamInfoForJoin<-summarise(group_by(forLater,Team),
                             PtDiffPG = mean(PtDiffPG),
                             WPerc = mean(WPerc),
                             LossTeamsPtDiff = mean(LossTeamsPtDiff),
                             Att3PG = mean(Att3PG),
                             Perc3 = mean(Perc3),
                             RPG = mean(RPG),
                             ORPG = mean(ORPG),
                             FTPG = mean(FTPG),
                             TPG = mean(TPG),
                             Perc3 = mean(Perc3),
                             HomeSplit = mean(PtDiffPGSplit[IsHome==1]),
                             AwaySplit = mean(PtDiffPGSplit[IsHome==0])
  )
  
  head(gamesToPredict)
  
  colnames(teamInfoForJoin)
  teamInfoForJoinHome<-teamInfoForJoin[,-12]
  colnames(teamInfoForJoinHome)[11]<-"PtDiffPGSplit"
  
  gamesToPredict<-left_join(gamesToPredict,teamInfoForJoinHome,by=c("Team"))
  
  teamInfoForJoinOpp<-teamInfoForJoin[,-11]
  colnames(teamInfoForJoinOpp)[2:10]<-paste0(colnames(teamInfoForJoinOpp)[2:10],"_Opp")
  colnames(teamInfoForJoinOpp)[11]<-"PtDiffPGSplit_Opp"
  gamesToPredict<-left_join(gamesToPredict,teamInfoForJoinOpp,by=c("Opponent"="Team"))
  head(gamesToPredict)
  
  gamesToPredict$LossTeamsPtDiff[is.na(gamesToPredict$LossTeamsPtDiff)]<-mean(gamesToPredict$LossTeamsPtDiff,na.rm=T)
  gamesToPredict$LossTeamsPtDiff_Opp[is.na(gamesToPredict$LossTeamsPtDiff_Opp)]<-mean(gamesToPredict$LossTeamsPtDiff_Opp,na.rm=T)
  gamesToPredict$PtDiffPGSplit[is.na(gamesToPredict$PtDiffPGSplit)]<-mean(gamesToPredict$PtDiffPGSplit,na.rm=T)
  gamesToPredict$PtDiffPGSplit_Opp[is.na(gamesToPredict$PtDiffPGSplit_Opp)]<-mean(gamesToPredict$PtDiffPGSplit_Opp,na.rm=T)
  
  bayesianDataForPredict<-with(gamesToPredict,list(Win = Win,
                                                   PtDiffPG = scale(PtDiffPG),
                                                   PtDiffPG_Opp = scale(PtDiffPG_Opp),
                                                   WPerc = scale(WPerc),
                                                   WPerc_Opp = scale(WPerc_Opp),
                                                   LossTeamsPtDiff = scale(LossTeamsPtDiff),
                                                   LossTeamsPtDiff_Opp = scale(LossTeamsPtDiff_Opp),
                                                   Att3PG = scale(Att3PG),
                                                   Perc3 = scale(Perc3),
                                                   RPG = scale(RPG),
                                                   ORPG = scale(ORPG),
                                                   FTPG = scale(FTPG),
                                                   TPG = scale(TPG),
                                                   Att3PG_Opp = scale(Att3PG_Opp),
                                                   Perc3_Opp = scale(Perc3_Opp),
                                                   RPG_Opp = scale(RPG_Opp),
                                                   ORPG_Opp = scale(ORPG_Opp),
                                                   FTPG_Opp = scale(FTPG_Opp),
                                                   TPG_Opp = scale(TPG_Opp),
                                                   PtDiffSplit = scale(PtDiffPGSplit-PtDiffPG),
                                                   PtDiffSplit_Opp = scale(PtDiffPGSplit_Opp-PtDiffPG_Opp),
                                                   IsNeutralSite = NeutralSite,
                                                   Perc3FavAdj = scale(ifelse(PtDiffPG>PtDiffPG_Opp,Perc3,Perc3_Opp)),
                                                   Perc3FavAdj_Opp = scale(ifelse(PtDiffPG>PtDiffPG_Opp,Perc3_Opp,Perc3))
  ))
  
  # sim.prob2 <- link( bayesianMod2 , data=bayesianDataForPredict )
  # # head(sim.prob2)
  # prob.mean2<-apply( sim.prob , 2 , mean )
  # prob.guess2<-round(prob.mean,0)
  # 
  # roc2 <- roc( as.vector(bayesianDataForPredict$Win), as.vector(prob.guess2))
  # # plot(roc2)
  # auc2<-auc(roc2)
  # 
  # Error2<-bayesianDataForPredict$Win-prob.mean2
  # rmse2<-sqrt(mean(Error2^2))
  
  # sim.prob4 <- link( bayesianMod4 , data=bayesianDataForPredict )
  # prob.mean4<-apply( sim.prob4 , 2 , mean )
  # prob.guess4<-round(prob.mean4,0)
  # 
  # roc4 <- roc( as.vector(bayesianDataForPredict$Win), as.vector(prob.guess4))
  # # plot(roc4)
  # auc4<-auc(roc4)
  # 
  # Error4<-bayesianDataForPredict$Win-prob.mean4
  # rmse4<-sqrt(mean(Error4^2))
  
  #The last 2 comparisons were auc, taking a guess for each win/loss based on the estimated probability
  #and RMSE, using the actual estimated probability, so we're getting more nuance than just win/loss
  
  #Getting the estimated probability for each matchup
  sim.prob5 <- link( bayesianMod5 , data=bayesianDataForPredict )
  prob.mean5<-apply( sim.prob5 , 2 , mean )
  prob.guess5<-round(prob.mean5,0)
  
  roc5 <- roc( as.vector(bayesianDataForPredict$Win), as.vector(prob.guess5))
  # plot(roc5)
  auc5<-auc(roc5)
  
  Error5<-bayesianDataForPredict$Win-prob.mean5
  rmse5<-sqrt(mean(Error5^2))
  
  # sim.prob6 <- link( bayesianMod6 , data=bayesianDataForPredict )
  # prob.mean6<-apply( sim.prob6 , 2 , mean )
  # prob.guess6<-round(prob.mean6,0)
  # 
  # roc6 <- roc( as.vector(bayesianDataForPredict$Win), as.vector(prob.guess6))
  # # plot(roc6)
  # auc6<-auc(roc6)
  # 
  # Error6<-bayesianDataForPredict$Win-prob.mean6
  # rmse6<-sqrt(mean(Error6^2))
  
  # sim.prob9 <- link( bayesianMod9 , data=bayesianDataForPredict )
  # prob.mean9<-apply( sim.prob9 , 2 , mean )
  # prob.guess9<-round(prob.mean9,0)
  # 
  # roc9 <- roc( as.vector(bayesianDataForPredict$Win), as.vector(prob.guess9))
  # # plot(roc9)
  # auc9<-auc(roc9)
  # 
  # Error9<-bayesianDataForPredict$Win-prob.mean9
  # rmse9<-sqrt(mean(Error9^2))
  # 
  # sim.prob10 <- link( bayesianMod10 , data=bayesianDataForPredict )
  # prob.mean10<-apply( sim.prob10 , 2 , mean )
  # prob.guess10<-round(prob.mean10,0)
  # 
  # roc10 <- roc( as.vector(bayesianDataForPredict$Win), as.vector(prob.guess10))
  # # plot(roc10)
  # auc10<-auc(roc10)
  # 
  # Error10<-bayesianDataForPredict$Win-prob.mean10
  # rmse10<-sqrt(mean(Error10^2))
  
  sim.prob11 <- link( bayesianMod11 , data=bayesianDataForPredict )
  prob.mean11<-apply( sim.prob11 , 2 , mean )
  prob.guess11<-round(prob.mean11,0)
  
  roc11 <- roc( as.vector(bayesianDataForPredict$Win), as.vector(prob.guess11))
  # plot(roc11)
  auc11<-auc(roc11)
  
  Error11<-bayesianDataForPredict$Win-prob.mean11
  rmse11<-sqrt(mean(Error11^2))
  
  # sim.prob12 <- link( bayesianMod12 , data=bayesianDataForPredict )
  # prob.mean12<-apply( sim.prob12 , 2 , mean )
  # prob.guess12<-round(prob.mean12,0)
  # 
  # roc12 <- roc( as.vector(bayesianDataForPredict$Win), as.vector(prob.guess12))
  # # plot(roc12)
  # auc12<-auc(roc12)
  # 
  # Error12<-bayesianDataForPredict$Win-prob.mean12
  # rmse12<-sqrt(mean(Error12^2))
  # 
  # sim.prob13 <- link( bayesianMod13 , data=bayesianDataForPredict )
  # prob.mean13<-apply( sim.prob13 , 2 , mean )
  # prob.guess13<-round(prob.mean13,0)
  # 
  # roc13 <- roc( as.vector(bayesianDataForPredict$Win), as.vector(prob.guess13))
  # # plot(roc13)
  # auc13<-auc(roc13)
  # 
  # Error13<-bayesianDataForPredict$Win-prob.mean13
  # rmse13<-sqrt(mean(Error13^2))
  
  sim.prob14 <- link( bayesianMod14 , data=bayesianDataForPredict )
  prob.mean14<-apply( sim.prob14 , 2 , mean )
  prob.guess14<-round(prob.mean14,0)
  
  roc14 <- roc( as.vector(bayesianDataForPredict$Win), as.vector(prob.guess14))
  # plot(roc14)
  auc14<-auc(roc14)
  
  Error14<-bayesianDataForPredict$Win-prob.mean14
  rmse14<-sqrt(mean(Error14^2))
  
  sim.prob15 <- link( bayesianMod15 , data=bayesianDataForPredict )
  prob.mean15<-apply( sim.prob15 , 2 , mean )
  prob.guess15<-round(prob.mean15,0)
  
  roc15 <- roc( as.vector(bayesianDataForPredict$Win), as.vector(prob.guess15))
  # plot(roc15)
  auc15<-auc(roc15)
  
  Error15<-bayesianDataForPredict$Win-prob.mean15
  rmse15<-sqrt(mean(Error15^2))
  
  sim.prob16 <- link( bayesianMod16 , data=bayesianDataForPredict )
  prob.mean16<-apply( sim.prob16 , 2 , mean )
  prob.guess16<-round(prob.mean16,0)
  
  roc16 <- roc( as.vector(bayesianDataForPredict$Win), as.vector(prob.guess16))
  # plot(roc16)
  auc16<-auc(roc16)
  
  Error16<-bayesianDataForPredict$Win-prob.mean16
  rmse16<-sqrt(mean(Error16^2))
  
  # evalDF[evalDF$Round==j&evalDF$Model==2,3:6]<-c(comp$PSIS[rownames(comp)=="bayesianMod2"],
  #                                                comp$pPSIS[rownames(comp)=="bayesianMod2"],
  #                                                auc2,rmse2)
  
  # evalDF[evalDF$Round==j&evalDF$Model==4,3:6]<-c(comp$PSIS[rownames(comp)=="bayesianMod4"],
  #                                                comp$pPSIS[rownames(comp)=="bayesianMod4"],
  #                                                auc4,rmse4)
  
  evalDF[evalDF$Round==j&evalDF$Model==5,3:6]<-c(comp$PSIS[rownames(comp)=="bayesianMod5"],
                                                 comp$pPSIS[rownames(comp)=="bayesianMod5"],
                                                 auc5,rmse5)
  
  # evalDF[evalDF$Round==j&evalDF$Model==6,3:6]<-c(comp$PSIS[rownames(comp)=="bayesianMod6"],
  #                                                comp$pPSIS[rownames(comp)=="bayesianMod6"],
  #                                                auc6,rmse6)
  # 
  # evalDF[evalDF$Round==j&evalDF$Model==9,3:6]<-c(comp$PSIS[rownames(comp)=="bayesianMod9"],
  #                                                comp$pPSIS[rownames(comp)=="bayesianMod9"],
  #                                                auc9,rmse9)
  # 
  # evalDF[evalDF$Round==j&evalDF$Model==10,3:6]<-c(comp$PSIS[rownames(comp)=="bayesianMod10"],
  #                                                comp$pPSIS[rownames(comp)=="bayesianMod10"],
  #                                                auc10,rmse10)
  
  evalDF[evalDF$Round==j&evalDF$Model==11,3:6]<-c(comp$PSIS[rownames(comp)=="bayesianMod11"],
                                                  comp$pPSIS[rownames(comp)=="bayesianMod11"],
                                                  auc11,rmse11)
  
  # evalDF[evalDF$Round==j&evalDF$Model==12,3:6]<-c(comp$PSIS[rownames(comp)=="bayesianMod12"],
  #                                                comp$pPSIS[rownames(comp)=="bayesianMod12"],
  #                                                auc12,rmse12)
  # 
  # evalDF[evalDF$Round==j&evalDF$Model==13,3:6]<-c(comp$PSIS[rownames(comp)=="bayesianMod13"],
  #                                                comp$pPSIS[rownames(comp)=="bayesianMod13"],
  #                                                auc13,rmse13)
  
  evalDF[evalDF$Round==j&evalDF$Model==14,3:6]<-c(comp$PSIS[rownames(comp)=="bayesianMod14"],
                                                  comp$pPSIS[rownames(comp)=="bayesianMod14"],
                                                  auc14,rmse14)
  
  evalDF[evalDF$Round==j&evalDF$Model==15,3:6]<-c(comp$PSIS[rownames(comp)=="bayesianMod15"],
                                                  comp$pPSIS[rownames(comp)=="bayesianMod15"],
                                                  auc15,rmse15)
  
  evalDF[evalDF$Round==j&evalDF$Model==16,3:6]<-c(comp$PSIS[rownames(comp)=="bayesianMod16"],
                                                  comp$pPSIS[rownames(comp)=="bayesianMod16"],
                                                  auc16,rmse16)
}


summarise(group_by(evalDF,Model),
          PSIS = mean(PSIS),
          EffParam = mean(EffParam),
          AUC = mean(AUC),
          RMSE = mean(RMSE),
          PSISNoParam = mean(PSIS-EffParam))

write.excel(evalDF)

#We're still sticking with Model 5 - nothing has been impressive enough to beat it out.
str(gamesToUse)

#Now getting things for the whole dataframe to make final predictions
buildSample<-1:149
gamesToUse<-boxscores[boxscores$GameId %in% buildSample&boxscores$Home==1,]

standingsBuild<-summarise(group_by(boxscores[boxscores$GameId %in% c(buildSample),],Team),
                          Wins = sum(Win),
                          Losses = sum(Win==0),
                          WPerc = Wins/(Wins+Losses),
                          Count = n(),
                          PtDiff = sum(PointDifference),
                          PtDiffPG = mean(PointDifference),
                          PPG = mean(Score),
                          PPGAgainst = mean(OppScore),
                          Att3PG = mean(FG3A),
                          Perc3 = sum(FG3M)/sum(FG3A),
                          RPG = mean(Rebounds),
                          ORPG = mean(ORB),
                          FTPG = mean(FTA),
                          TPG = mean(TOV),
                          PtDiffPGSplit = rep(NA)
)

standingsBuild<-standingsBuild[order(-standingsBuild$WPerc),]
head(standingsBuild)

standingsBuild$WinTeamsPtDiff<-rep(NA)
standingsBuild$LossTeamsPtDiff<-rep(NA)
standingsForJoin<-standingsBuild[0,]

for(i in 1:nrow(gamesToUse)){
  gamesForRow<-boxscores[boxscores$GameId %in% buildSample&boxscores$GameId!=gamesToUse$GameId[i],]
  
  standingsBuild<-summarise(group_by(gamesForRow,Team),
                            Wins = sum(Win),
                            Losses = sum(Win==0),
                            WPerc = Wins/(Wins+Losses),
                            Count = n(),
                            PtDiff = sum(PointDifference),
                            PtDiffPG = mean(PointDifference),
                            PPG = mean(Score),
                            PPGAgainst = mean(OppScore),
                            Att3PG = mean(FG3A),
                            Perc3 = sum(FG3M)/sum(FG3A),
                            RPG = mean(Rebounds),
                            ORPG = mean(ORB),
                            FTPG = mean(FTA),
                            TPG = mean(TOV),
                            PtDiffPGSplit = mean(PointDifference[Home==gamesToUse$Home[i]])
  )
  
  standingsBuild$WinTeamsPtDiff<-rep(NA)
  standingsBuild$LossTeamsPtDiff<-rep(NA)
  
  opponents<-gamesForRow[gamesForRow$Opponent==gamesToUse$Team[i],]
  adjustedPointDifference<-summarise(group_by(gamesForRow,Team),
                                     PointDiffAdj = sum(PointDifference[Opponent!=gamesToUse$Team[i]]),
                                     PointsAdj = sum(Score[Opponent!=gamesToUse$Team[i]]),
                                     PointsAllowedAdj = sum(OppScore[Opponent!=gamesToUse$Team[i]]),
                                     GameCount = n())
  opponents<-left_join(opponents,adjustedPointDifference,by=c("Team"))
  #Reversed because from perspective of opponent
  standingsBuild$WinTeamsPtDiff[which(standingsBuild$Team==gamesToUse$Team[i])]<-sum(opponents$PointDiffAdj[opponents$Win==0])/sum(opponents$GameCount[opponents$Win==0])
  standingsBuild$LossTeamsPtDiff[which(standingsBuild$Team==gamesToUse$Team[i])]<-sum(opponents$PointDiffAdj[opponents$Win==1])/sum(opponents$GameCount[opponents$Win==1])
  
  standingsForJoin<-bind_rows(standingsForJoin,standingsBuild[is.na(standingsBuild$LossTeamsPtDiff)==F|is.na(standingsBuild$WinTeamsPtDiff)==F,])
}

nrow(standingsForJoin)
print.data.frame(head(standingsForJoin))

table(standingsForJoin$WinTeamsPtDiff)
table(standingsForJoin$LossTeamsPtDiff)

standingsForJoinOpp<-standingsBuild[0,]

colnames(gamesToUse)
for(i in 1:nrow(gamesToUse)){
  gamesForRow<-boxscores[boxscores$GameId %in% buildSample&boxscores$GameId!=gamesToUse$GameId[i],]
  
  standingsBuild<-summarise(group_by(gamesForRow,Team),
                            Wins = sum(Win),
                            Losses = sum(Win==0),
                            WPerc = Wins/(Wins+Losses),
                            Count = n(),
                            PtDiff = sum(PointDifference),
                            PtDiffPG = mean(PointDifference),
                            PPG = mean(Score),
                            PPGAgainst = mean(OppScore),
                            Att3PG = mean(FG3A),
                            Perc3 = sum(FG3M)/sum(FG3A),
                            RPG = mean(Rebounds),
                            ORPG = mean(ORB),
                            FTPG = mean(FTA),
                            TPG = mean(TOV),
                            PtDiffPGSplit = mean(PointDifference[Home!=gamesToUse$Home[i]])
  )
  
  standingsBuild$WinTeamsPtDiff<-rep(NA)
  standingsBuild$LossTeamsPtDiff<-rep(NA)
  
  opponents<-gamesForRow[gamesForRow$Opponent==gamesToUse$Opponent[i],]
  adjustedPointDifference<-summarise(group_by(gamesForRow,Team),
                                     PointDiffAdj = sum(PointDifference[Opponent!=gamesToUse$Opponent[i]]),
                                     PointsAdj = sum(Score[Opponent!=gamesToUse$Opponent[i]]),
                                     PointsAllowedAdj = sum(OppScore[Opponent!=gamesToUse$Opponent[i]]),
                                     GameCount = n())
  opponents<-left_join(opponents,adjustedPointDifference,by=c("Team"))
  #Reversed because from perspective of opponent
  standingsBuild$WinTeamsPtDiff[which(standingsBuild$Team==gamesToUse$Opponent[i])]<-sum(opponents$PointDiffAdj[opponents$Win==0])/sum(opponents$GameCount[opponents$Win==0])
  standingsBuild$LossTeamsPtDiff[which(standingsBuild$Team==gamesToUse$Opponent[i])]<-sum(opponents$PointDiffAdj[opponents$Win==1])/sum(opponents$GameCount[opponents$Win==1])
  
  standingsForJoinOpp<-bind_rows(standingsForJoinOpp,standingsBuild[is.na(standingsBuild$LossTeamsPtDiff)==F|is.na(standingsBuild$WinTeamsPtDiff)==F,])
}

str(standingsForJoin)
str(standingsForJoinOpp)
str(gamesToUse)
colnames(standingsForJoin)

gamesToUse<-cbind(gamesToUse,standingsForJoin[,-c(1:3,5)])
head(gamesToUse)

colnames(standingsForJoinOpp)[2:18]<-paste0(colnames(standingsForJoinOpp)[2:18],"_Opp")
gamesToUse<-cbind(gamesToUse,standingsForJoinOpp[,-c(1:3,5)])

head(gamesToUse)

gamesToUse$WinTeamsPtDiff[is.na(gamesToUse$WinTeamsPtDiff)]<-mean(gamesToUse$WinTeamsPtDiff,na.rm=T)
gamesToUse$WinTeamsPtDiff_Opp[is.na(gamesToUse$WinTeamsPtDiff_Opp)]<-mean(gamesToUse$WinTeamsPtDiff_Opp,na.rm=T)
gamesToUse$LossTeamsPtDiff[is.na(gamesToUse$LossTeamsPtDiff)]<-mean(gamesToUse$LossTeamsPtDiff,na.rm=T)
gamesToUse$LossTeamsPtDiff_Opp[is.na(gamesToUse$LossTeamsPtDiff_Opp)]<-mean(gamesToUse$LossTeamsPtDiff_Opp,na.rm=T)
gamesToUse$PtDiffPGSplit[is.na(gamesToUse$PtDiffPGSplit)]<-mean(gamesToUse$PtDiffPGSplit,na.rm=T)
gamesToUse$PtDiffPGSplit_Opp[is.na(gamesToUse$PtDiffPGSplit_Opp)]<-mean(gamesToUse$PtDiffPGSplit_Opp,na.rm=T)

bayesianData<-with(gamesToUse,list(Win = Win,
                                   PtDiffPG = scale(PtDiffPG),
                                   PtDiffPG_Opp = scale(PtDiffPG_Opp),
                                   WPerc = scale(WPerc),
                                   WPerc_Opp = scale(WPerc_Opp),
                                   # PPG = scale(PPG),
                                   # PPG_Opp = scale(PPG_Opp),
                                   # PPGAgainst = scale(PPGAgainst),
                                   # PPGAgainst_Opp = scale(PPGAgainst_Opp),
                                   Att3PG = scale(Att3PG),
                                   Perc3 = scale(Perc3),
                                   RPG = scale(RPG),
                                   ORPG = scale(ORPG),
                                   FTPG = scale(FTPG),
                                   TPG = scale(TPG),
                                   Att3PG_Opp = scale(Att3PG_Opp),
                                   Perc3_Opp = scale(Perc3_Opp),
                                   RPG_Opp = scale(RPG_Opp),
                                   ORPG_Opp = scale(ORPG_Opp),
                                   FTPG_Opp = scale(FTPG_Opp),
                                   TPG_Opp = scale(TPG_Opp),
                                   PtDiffSplit = scale(PtDiffPGSplit-PtDiffPG),
                                   PtDiffSplit_Opp = scale(PtDiffPGSplit_Opp-PtDiffPG_Opp),
                                   # WinTeamsPtDiff = scale(WinTeamsPtDiff),
                                   LossTeamsPtDiff = scale(LossTeamsPtDiff),
                                   # WinTeamsPtDiff_Opp = scale(WinTeamsPtDiff_Opp),
                                   LossTeamsPtDiff_Opp = scale(LossTeamsPtDiff_Opp),
                                   IsNeutralSite = NeutralSite,
                                   Perc3FavAdj = scale(ifelse(PtDiffPG>PtDiffPG_Opp,Perc3,Perc3_Opp)),
                                   Perc3FavAdj_Opp = scale(ifelse(PtDiffPG>PtDiffPG_Opp,Perc3_Opp,Perc3))
                                   # DaysRestHome = scale(log(DaysRestHome)),
                                   # DaysRestAway = scale(log(DaysRestAway))
))

bayesianMod5Final <- ulam(
  alist(
    Win ~ dbinom( 1 , p ) ,
    logit(p) <- a + b*PtDiffPG + c*PtDiffPG_Opp + d*LossTeamsPtDiff + e*LossTeamsPtDiff_Opp,
    a ~ dnorm( 0 , 1.5 ) ,
    b ~ dnorm( 0 , 1.5 ) ,
    c ~ dnorm( 0 , 1.5 ) ,
    d ~ dnorm( 0 , 1.5 ) ,
    e ~ dnorm( 0 , 1.5 ) 
  ) , data=bayesianData , chains=4, cores=4, log_lik = TRUE )
precis(bayesianMod5Final,depth=2)

#Setting up a dataframe with one game for each team against each other
forRanking<-expand.grid(
  HomeTeam = teams,
  AwayTeam = teams
)

#Removing teams playing themselves
forRanking<-forRanking[forRanking$HomeTeam!=forRanking$AwayTeam,]

#Recycling code from above to get the opponent-based stats easily
buildSample<-1:149
gamesToUse<-boxscores

finalTeamInfo<-summarise(group_by(boxscores[boxscores$GameId %in% c(buildSample),],Team),
                         Wins = sum(Win),
                         Losses = sum(Win==0),
                         WPerc = Wins/(Wins+Losses),
                         Count = n(),
                         PtDiff = sum(PointDifference),
                         PtDiffPG = mean(PointDifference),
                         PPG = mean(Score),
                         PPGAgainst = mean(OppScore),
                         Att3PG = mean(FG3A),
                         Perc3 = sum(FG3M)/sum(FG3A),
                         RPG = mean(Rebounds),
                         ORPG = mean(ORB),
                         FTPG = mean(FTA),
                         TPG = mean(TOV),
                         PtDiffPGSplit = rep(NA)
)

finalTeamInfo<-finalTeamInfo[order(-finalTeamInfo$WPerc),]
head(finalTeamInfo)

finalTeamInfo$WinTeamsPtDiff<-rep(NA)
finalTeamInfo$LossTeamsPtDiff<-rep(NA)

for(i in 1:nrow(finalTeamInfo)){
  gamesForRow<-gamesToUse
  opponents<-gamesForRow[gamesForRow$Opponent==finalTeamInfo$Team[i],]
  adjustedPointDifference<-summarise(group_by(gamesForRow,Team),
                                     PointDiffAdj = sum(PointDifference[Opponent!=finalTeamInfo$Team[i]]),
                                     PointsAdj = sum(Score[Opponent!=finalTeamInfo$Team[i]]),
                                     PointsAllowedAdj = sum(OppScore[Opponent!=finalTeamInfo$Team[i]]),
                                     GameCount = n())
  opponents<-left_join(opponents,adjustedPointDifference,by=c("Team"))
  #Reversed because from perspective of opponent
  finalTeamInfo$WinTeamsPtDiff[i]<-sum(opponents$PointDiffAdj[opponents$Win==0])/sum(opponents$GameCount[opponents$Win==0])
  finalTeamInfo$LossTeamsPtDiff[i]<-sum(opponents$PointDiffAdj[opponents$Win==1])/sum(opponents$GameCount[opponents$Win==1])
}

nrow(finalTeamInfo)
print.data.frame(head(finalTeamInfo))

colnames(finalTeamInfo)

finalTeamInfoForJoin<-finalTeamInfo[,c(1,7,18)]
head(finalTeamInfoForJoin)

finalTeamInfoForJoinOpp<-finalTeamInfoForJoin
colnames(finalTeamInfoForJoinOpp)[2:3]<-c("PtDiffPG_Opp","LossTeamsPtDiff_Opp")

forRanking<-left_join(forRanking,finalTeamInfoForJoin,by=c("HomeTeam"="Team"))
forRanking<-left_join(forRanking,finalTeamInfoForJoinOpp,by=c("AwayTeam"="Team"))

head(forRanking)

bayesianDataFinal<-with(forRanking,list(PtDiffPG = scale(PtDiffPG),
                                        LossTeamsPtDiff = scale(LossTeamsPtDiff),
                                        PtDiffPG_Opp = scale(PtDiffPG_Opp),
                                        LossTeamsPtDiff_Opp = scale(LossTeamsPtDiff_Opp)
))

precis(bayesianMod5Final)

#Predicting thing on the dataframe of teams playing each other
sim.probFinal <- link( bayesianMod5Final , data=bayesianDataFinal )
prob.meanFinal<-apply( sim.probFinal , 2 , mean )
prob.guessFinal<-round(prob.meanFinal,0)

forRanking$WinProb<-prob.meanFinal
forRanking$WinGuess<-ifelse(prob.guessFinal==1,0,1)

#This is for each team on the road
accRanking<-summarise(group_by(forRanking,AwayTeam),
                      EstWinPerc = mean(1-WinProb),
                      GameCount = n()
)
accRanking<-accRanking[order(-accRanking$EstWinPerc),]

accRanking<-left_join(accRanking,finalTeamInfo[,c("Team","WPerc","PtDiffPG","LossTeamsPtDiff")],by=c("AwayTeam"="Team"))

print.data.frame(accRanking)

with(accRanking,cor(EstWinPerc,PtDiffPG,method = "spearman"))

#This is for each team at home - less relevant, but we'll use it in a sec.
accRankingAlt<-summarise(group_by(forRanking,HomeTeam),
                         EstWinPerc = mean(WinProb),
                         GameCount = n()
)
accRankingAlt<-accRankingAlt[order(-accRankingAlt$EstWinPerc),]

head(forRanking[forRanking$HomeTeam=="Virginia Cavaliers"&forRanking$AwayTeam=="Duke Blue Devils",])
head(forRanking[forRanking$AwayTeam=="Virginia Cavaliers"&forRanking$HomeTeam=="Duke Blue Devils",])

#Combining home and road to get neutral site
roadMean<-mean(accRanking$EstWinPerc)
homeMean<-mean(accRankingAlt$EstWinPerc)
roadSD<-sd(accRanking$EstWinPerc)
homeSD<-sd(accRankingAlt$EstWinPerc)

finalMean<-mean(c(roadMean,homeMean))
finalSD<-mean(c(roadSD,homeSD))

#Getting the final mean neutral site WP% against all teams
accRanking$MeanNeutralSiteWinPerc<-with(accRanking,ifelse(EstWinPerc>roadMean,(EstWinPerc-roadMean)/roadSD*finalSD+finalMean,
                                                          (EstWinPerc-roadMean)/roadSD*finalSD+finalMean))

accRanking$Rank<-1:15

colnames(accRanking)[1]<-"Team"

#Publishing the rankings
write.csv(accRanking[,c("Rank","Team","MeanNeutralSiteWinPerc")],file="ACCRanking1819.csv")
