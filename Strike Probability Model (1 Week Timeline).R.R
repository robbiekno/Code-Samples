require(xgboost)
require(caret)
require(dplyr)

pitchesDF<-read.csv(file="pitch_data.csv",header=T)

#Exploring and Pre-Processing the Data
str(pitchesDF)

nrow(pitchesDF)#106077 - a decent number of pitches, but certainly a small fraction of what would be used for
#the real version of this model and something to be cognizant of in my modeling.
#It will help with runtime at least.
table(pitchesDF$is_strike)
table(pitchesDF$is_swing)
#It doesn't make sense that there are more swings than strikes - every swing should be a strike
sum(pitchesDF$is_strike==0&pitchesDF$is_swing==1)#18496 pitches are not a strike but are a swing
#Regardless, we're focusing on taken pitches here, so that doesn't matter.
table(pitchesDF$is_strike[pitchesDF$is_swing==0])
mean(pitchesDF$is_strike[pitchesDF$is_swing==0])

#There's a 31% Strike% among called pitches. Let's make sure that makes sense.
#Baseball-Reference has a 64.4% Strike% in MLB this season. 
#They say that 48.0% of pitches are swung at, and those should all be strikes.
#So that leaves 16.4% of strikes that were taken for strikes and 35.6% that were taken for balls,
#Dividing the two is 31.5%, so it looks like the part of the data for taken pitches lines up with reality.
#We're also at a 64% Strike% overall if we add in the swings that weren't listed as strikes to all
#the pitches that were listed as strikes, so that's good to see.

table(pitchesDF$inning)#A bunch of extra-inning pitches going up to the 18th inning.
table(pitchesDF$is_bottom)
table(pitchesDF$balls)
table(pitchesDF$strikes)
table(pitchesDF$outs_before)
table(pitchesDF$is_lhp)
table(pitchesDF$is_lhb)
table(pitchesDF$pitch_type)
#Aside from the main pitch types listed in the Word document, there are 1082 cutters, 306 splitters, and 2 OT.
#Definitely could use a look for some more pre-processing.
table(pitchesDF$bat_score_before)
table(pitchesDF$field_score)
pitchesDF$Score_Difference<-with(pitchesDF,bat_score_before-field_score)
summary(pitchesDF$Score_Difference)#Score difference is likely going to be more meaningful than the individual scores
#Though you never know if there could be some strike probability change in a blowout vs. a pitchers' duel
table(pitchesDF$basecode_before)
#At the very least, that variable needs to be a factor, but definitely some meat on the bone beyond that.
pitchesDF$RunnersOn<-factor(with(pitchesDF,ifelse(basecode_before!=0,1,0)))
table(pitchesDF$RunnersOn)
pitchesDF$IsRunnerOn1st<-factor(with(pitchesDF,ifelse(basecode_before %in% c(1,3,5,7),1,0)))
table(pitchesDF$IsRunnerOn1st)
pitchesDF$IsRunnerOn2nd<-factor(with(pitchesDF,ifelse(basecode_before %in% c(2,3,6,7),1,0)))
table(pitchesDF$IsRunnerOn2nd)
pitchesDF$IsRunnerOn3rd<-factor(with(pitchesDF,ifelse(basecode_before %in% c(4,5,6,7),1,0)))
table(pitchesDF$IsRunnerOn3rd)
pitchesDF$basecode_before<-as.factor(pitchesDF$basecode_before)


#As discussed in the Word document, I'm not going to use batterid, pitcherid, cid, or hp_umpid. I'm 
#just inviting over-fitting if I do that, among other issues (especially given the smaller sample size).
#It would be possible to build a second-level 
#model afterwards, a glmer, to value catcher framing or any other strike probability effects desired.

summary(pitchesDF$plate_location_x)
summary(pitchesDF$plate_location_z)
summary(pitchesDF$rel_speed)
summary(pitchesDF$rel_speed[pitchesDF$pitch_type %in% c("FF","FT")])#93.2 MPH average for fastballs
#Maybe a few position players pitching in there, for what it's worth.
sum(pitchesDF$spin_rate=="NULL")#Yeah, that's how a variable like spin rate becomes a character, when there are NULLs.
pitchesDF$spin_rate[pitchesDF$spin_rate=="NULL"]<-NA
pitchesDF$spin_rate<-as.numeric(pitchesDF$spin_rate)
summary(pitchesDF$spin_rate)#Much better, now we're good to use this variable.

summary(pitchesDF$induced_vert_break)
summary(pitchesDF$induced_vert_break[pitchesDF$pitch_type %in% c("FF","FT")])

summary(pitchesDF$horizontal_break)
summary(pitchesDF$horizontal_break[pitchesDF$pitch_type %in% c("FF","FT")])
summary(pitchesDF$horizontal_break[pitchesDF$is_lhp==1])
summary(pitchesDF$horizontal_break[pitchesDF$is_lhp==0])

#Let's go back to the pitch types
table(pitchesDF$pitch_type)
#I know some front offices have major feelings about dplyr. I'm happy to switch over to whatever package
#you would rather I use. 
summarise(group_by(pitchesDF,pitch_type,is_lhp),
          Velo = mean(rel_speed,na.rm=T),
          Spin = mean(spin_rate,na.rm=T),
          PfxZ = mean(induced_vert_break/2.2,na.rm=T),
          PfxX = mean(-1*horizontal_break/2.2,na.rm=T),
          Count = n())

summary(pitchesDF$horizontal_break[pitchesDF$pitch_type %in% c("CH")&pitchesDF$is_lhp==0])
summary(pitchesDF$horizontal_break[pitchesDF$pitch_type %in% c("FS")&pitchesDF$is_lhp==0])

summary(pitchesDF$spin_rate[pitchesDF$pitch_type %in% c("CH")&pitchesDF$is_lhp==0])
summary(pitchesDF$spin_rate[pitchesDF$pitch_type %in% c("FS")&pitchesDF$is_lhp==0])

#The splitters have a bit less spin and horizontal break than the changeups, but they're close enough to put together

head(pitchesDF[pitchesDF$pitch_type=="OT",])
#I could just get rid of these, but both of them look pretty two-seamer-ish, so we can probably just stick them there.
summary(pitchesDF$induced_vert_break[pitchesDF$pitch_type %in% c("FT")])
summary(pitchesDF$spin_rate[pitchesDF$pitch_type %in% c("FT")])

summary(pitchesDF$induced_vert_break[pitchesDF$pitch_type %in% c("FC")])
summary(pitchesDF$rel_speed[pitchesDF$pitch_type %in% c("FC")])

summary(pitchesDF$induced_vert_break[pitchesDF$pitch_type %in% c("FF")])
summary(pitchesDF$rel_speed[pitchesDF$pitch_type %in% c("FF")])

summary(pitchesDF$horizontal_break[pitchesDF$pitch_type %in% c("FC")&pitchesDF$is_lhp==0])
summary(pitchesDF$horizontal_break[pitchesDF$pitch_type %in% c("FF")&pitchesDF$is_lhp==0])

#I actually did a whole project in the past for another franchise about whether a cutter is more 
#fastball-like or cutter-like (and thus whether it should be evaluated like a fastball).
#I'm not going to recreate that methodology for 1082 pitches, but I can use some broad takeaways from it
#to look into classifying these cutters between FF and SL given the small sample size of them.
#The pitches that are harder and have less vertical break are the ones that are more fastball-like.


pitcherCutterCheck<-summarise(group_by(pitchesDF,pitcherid),
                              FBVelo = mean(rel_speed[pitch_type %in% c("FF","FT")],na.rm=T),
                              FBPfxZ = mean(induced_vert_break[pitch_type %in% c("FF","FT")]/2.2,na.rm=T),
                              FBCount = sum(pitch_type %in% c("FF","FT")),
                              CTVelo = mean(rel_speed[pitch_type=="FC"],na.rm=T),
                              CTPfxZ = mean(induced_vert_break[pitch_type=="FC"]/2.2,na.rm=T),
                              CTCount = sum(pitch_type=="FC"))

head(pitcherCutterCheck[pitcherCutterCheck$CTCount>=10,])

pitcherCutterCheck$CTasFB<-with(pitcherCutterCheck,ifelse(CTCount>0&FBVelo-CTVelo<2,1,0))
pitcherCutterCheck$CTasFB<-with(pitcherCutterCheck,ifelse(CTCount>0&FBPfxZ-CTPfxZ<2,1,CTasFB))
pitcherCutterCheck$CTasFB<-with(pitcherCutterCheck,ifelse(FBCount==0,1,CTasFB))
pitcherCutterCheck$CTasFB<-with(pitcherCutterCheck,ifelse(CTCount>0&FBCount>0&FBVelo-CTVelo>5,0,CTasFB))
summary(pitcherCutterCheck$CTasFB)
sum(pitcherCutterCheck$CTCount[pitcherCutterCheck$CTasFB==1])
sum(pitcherCutterCheck$CTCount[pitcherCutterCheck$CTasFB==0])

summary(pitcherCutterCheck$CTVelo[pitcherCutterCheck$CTasFB==1])
summary(pitcherCutterCheck$CTVelo[pitcherCutterCheck$CTasFB==0])

summary(pitcherCutterCheck$CTPfxZ[pitcherCutterCheck$CTasFB==1])
summary(pitcherCutterCheck$CTPfxZ[pitcherCutterCheck$CTasFB==0])

#We can see which pitch type is better, but if I do want to put cutters in with four-seamers, that should be fine
#This didn't wind up mattering at all in the final model.

colnames(pitcherCutterCheck)
cuttersforjoin<-pitcherCutterCheck[,c("pitcherid","CTasFB")]

pitchesDF<-left_join(pitchesDF,cuttersforjoin,by=c("pitcherid"))

head(pitchesDF)
summary(pitchesDF$CTasFB)

pitchesDF$PitchTypeFinal<-with(pitchesDF,ifelse(pitch_type=="FS","CH",pitch_type))#As discussed splitters a decent enough comp
pitchesDF$PitchTypeFinal<-with(pitchesDF,ifelse(pitch_type=="OT","FT",PitchTypeFinal))#The 2 OT pitches happened to look like two-seamers

pitchesDF$PitchTypeFinalWithCTChange<-with(pitchesDF,ifelse(PitchTypeFinal=="FC",ifelse(CTasFB==1,"FF","SL"),
                                                            PitchTypeFinal))#The 2 OT pitches happened to look like two-seamers
table(pitchesDF$pitch_type)
table(pitchesDF$PitchTypeFinal)
table(pitchesDF$PitchTypeFinalWithCTChange)

colnames(pitchesDF)

# head(pitchesDF$is_strike)
# head(pitchesDF[,1])

#Making the categorical variables factors
colnames(pitchesDF)
for(i in c(2:10,29:31)){
pitchesDF[,i]<-factor(pitchesDF[,i])
}

#Scaling the continuous variables
scaleDF<-pitchesDF[,c(11:12,18:24)]
scaledVariables<-colnames(scaleDF)

scaleDF<-scale(scaleDF)
summary(scaleDF)

colnames(scaleDF)<-paste0(colnames(scaleDF),"_Scaled")

head(scaleDF)

scaleDF<-as.data.frame(scaleDF)

pitchesDF<-bind_cols(pitchesDF,scaleDF)

head(pitchesDF)

tail(pitchesDF)

pitchesDF[302,]

mean(pitchesDF$is_strike[between(pitchesDF$plate_location_x,0.758,0.858)&between(pitchesDF$plate_location_z,2.368,3.368)&pitchesDF$is_lhb==1])

columnsToRemove<-c("batterid","pitcherid","cid","hp_umpid","pitch_type","CTasFB","is_swing",scaledVariables)

strikeProbDF<-pitchesDF[pitchesDF$is_swing==0,colnames(pitchesDF)[colnames(pitchesDF) %in% columnsToRemove==F]]
#Only taken pitches, removing the ID columns, the original pitch type column
#and my CTasFB column which is irrelevant for non-cutter pitches. And just sticking with the scaled variables

str(strikeProbDF)

#Dividing into Training and Testing
splitToUse<-createDataPartition(strikeProbDF$is_strike, p = 0.8, list = FALSE)
training = strikeProbDF[splitToUse,-1]
testing = strikeProbDF[-splitToUse,-1]

trainingResponse<-strikeProbDF$is_strike[splitToUse]
testingResponse<-strikeProbDF$is_strike[-splitToUse]

summary(training)

summary(strikeProbDF$is_strike)

#Building the first version of the XGBoost model
strikeProbMod0 <- xgboost::xgboost(data = data.matrix(training),
                         label = trainingResponse,
                         booster = "gbtree",
                         objective = "binary:logistic",
                         nrounds = 100,
                         verbose = 0)

predictions<-predict(strikeProbMod0,data.matrix(testing),type="response")
testing$Prediction<-predictions
choice<-ifelse(predictions>=0.5,1,0)
accuracy<-sum(choice==testingResponse)/length(testingResponse)#93.17507%

summary(predictions)
summary(testingResponse)
table(testingResponse)

#Seeing how the predictions look on the full dataframes (just taken and all pitches)
strikeProbDF$PredictionTemp<-predict(strikeProbMod0,data.matrix(strikeProbDF[,-1]),type="response")
head(strikeProbDF)

pitchesDF$PredictionTemp<-predict(strikeProbMod0,data.matrix(pitchesDF[,which(colnames(pitchesDF) %in% colnames(testing))]),type="response")
summary(pitchesDF$PredictionTemp)

#Building a heat map to see what the predictions look like on the strike zone

#Roughly estimated rectangular strike zone (a bit too wide, especially in the corners)
topKzone <- 3.5
botKzone <- 1.5
inKzone <- -0.95
outKzone <- 0.95
kZone <- data.frame(
  platex=c(inKzone, inKzone, outKzone, outKzone, inKzone),
  platez=c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

# summary(pitchesDF$plate_location_x)
# sd(pitchesDF$plate_location_x)
# 
# summary(pitchesDF$plate_location_z)
# sd(pitchesDF$plate_location_z)
# 
# smoothingDF<-strikeProbDF
# smoothingDF$platex<-with(smoothingDF,plate_location_x_Scaled*0.8473901-0.03853)
# summary(smoothingDF$platex)
# smoothingDF$platez<-with(smoothingDF,plate_location_z_Scaled*0.9579876+2.271)
# summary(smoothingDF$platez)

smoothingDF<-pitchesDF[pitchesDF$is_lhb==1,]
smoothingDF$platex<-smoothingDF$plate_location_x
smoothingDF$platez<-smoothingDF$plate_location_z

smoothFit <- loess(PredictionTemp ~ platex*platez,span=0.3,data=smoothingDF)

# find predicted probabilities over a 50 x 50 grid
x <- seq(-1.5, 1.5, length.out=100)
y <- seq(0.5, 5, length.out=100)
data.predict <- data.frame(platex = c(outer(x, y * 0 + 1)),
                           platez = c(outer(x * 0 + 1, y)))
data.predict$StrikeProbability <- predict(smoothFit, data.predict)
summary(data.predict$StrikeProbability)
data.predict$StrikeProbability[data.predict$StrikeProbability>1]<-1
data.predict$StrikeProbability[data.predict$StrikeProbability<0]<-0

# construct the heat map
ggplot(kZone, aes(platex, platez)) +
  geom_tile(data=data.predict, 
            aes(x=platex, y=platez, fill= StrikeProbability)) +
  scale_fill_distiller(palette = "Spectral") +
  geom_path(lwd=1.5, col="black") +
  coord_fixed() +
  ggtitle("Interim Strike Prob Test Vs. LHH")

#Overall, this is looking pretty good, though I could certainly smooth this heat map out more.

#The variable importance passes the sniff test. I did try seeing if anything would change if I removed
#a few variables correlated with other ones, but it didn't seem to help so I'll move forward with the full list.
importance_matrix <- xgb.importance(colnames(training), model = strikeProbMod0)
xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")

#This was the longer list of variables I tried removing
# colnames(pitchesDF)
# columnsToRemove<-c("batterid","pitcherid","cid","hp_umpid","pitch_type","CTasFB","is_swing",scaledVariables,
#                    "PredictionTemp","field_score_scaled","PitchTypeFinalWithCTChange",
#                    "RunnersOn","IsRunnerOn1st","IsRunnerOn2nd","IsRunnerOn3rd")
#Don't need batting and fielding team score, the pitch types with CT grouped in didn't do as well
#And let's just try sticking to the basecodes

#Now onto tuning. 

tuning_grid <- expand.grid(
  nrounds = c(50,100,200),
  eta = c(0.025, 0.05, 0.1),
  max_depth = c(2,4,6),
  gamma = c(0, 1, 2),
  colsample_bytree = c(0.5, 0.75, 1.0),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.5,0.75,1)
)

tune_options <- trainControl(
  method = "cv", # cross-validation
  number = 10, #10-fold CV
  # repeats = 5,
  verboseIter = TRUE,
  allowParallel = FALSE,
  search = "random"
)

#The Caret and XGBoost packages wanted the response in different forms
trainingResponse<-factor(trainingResponse)
str(trainingResponse)

#This took a long time to run, so I'm commenting it out in case you want to run this script
# optimal_params <- train(
#   x = data.matrix(training),
#   y = trainingResponse,
#   trControl = tune_options,
#   tuneGrid = tuning_grid,
#   method = "xgbTree",
#   verbose = FALSE
# )

# optimal_params$bestTune
# save(optimal_params,file="Final Strike Prob Model Parameters.RDA")
#Saving these so you don't need to run the above
load(file="Final Strike Prob Model Parameters.RDA")

trainingResponse<-as.numeric(as.character(trainingResponse))

summary(trainingResponse)
table(trainingResponse)

str(training)

strikeProbModFinal <- xgboost::xgboost(data = data.matrix(training),
                       label = trainingResponse,
                       booster = "gbtree",
                       objective = "binary:logistic",
                       nrounds = optimal_params$bestTune$nrounds,
                       max_depth=optimal_params$bestTune$max_depth,
                       colsample_bytree=optimal_params$bestTune$colsample_bytree,
                       min_child_weight=optimal_params$bestTune$min_child_weight,
                       subsample=optimal_params$bestTune$subsample,
                       eta=optimal_params$bestTune$eta,
                       gamma=optimal_params$bestTune$gamma,
                       verbose = 0)

predictionsFinal<-predict(strikeProbModFinal,data.matrix(testing[1:23]),type="response")
choiceFinal<-ifelse(predictionsFinal>=0.5,1,0)
accuracyFinal<-sum(choiceFinal==testingResponse)/length(testingResponse)#92.246471%
BrierFinal<-sum((choiceFinal-testingResponse)^2)/length(testingResponse)#92.60858%

importance_matrixFinal <- xgb.importance(colnames(training), model = strikeProbMod0.1)
xgb.plot.importance(importance_matrixFinal, rel_to_first = TRUE, xlab = "Relative importance")


#Seeing how the predictions look on the full dataframes (just taken and all pitches)
strikeProbDF$PredictionFinal<-predict(strikeProbModFinal,data.matrix(strikeProbDF[,-c(1,25:26)]),type="response")
head(strikeProbDF)

pitchesDF$PredictionFinal<-predict(strikeProbModFinal,data.matrix(pitchesDF[,which(colnames(pitchesDF) %in% colnames(testing)[1:23])]),type="response")
summary(pitchesDF$PredictionFinal)

#Building a heat map to see what the predictions look like on the strike zone

#Roughly estimated rectangular strike zone (a bit too wide, especially in the corners)
topKzone <- 3.5
botKzone <- 1.5
inKzone <- -0.95
outKzone <- 0.95
kZone <- data.frame(
  platex=c(inKzone, inKzone, outKzone, outKzone, inKzone),
  platez=c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

smoothingDF<-pitchesDF[pitchesDF$is_lhb==1,]
smoothingDF$platex<-smoothingDF$plate_location_x
smoothingDF$platez<-smoothingDF$plate_location_z

smoothFit <- loess(PredictionTemp ~ platex*platez,span=0.3,data=smoothingDF)

# find predicted probabilities over a 50 x 50 grid
x <- seq(-1.5, 1.5, length.out=100)
y <- seq(0.5, 5, length.out=100)
data.predict <- data.frame(platex = c(outer(x, y * 0 + 1)),
                           platez = c(outer(x * 0 + 1, y)))
data.predict$StrikeProbability <- predict(smoothFit, data.predict)
summary(data.predict$StrikeProbability)
data.predict$StrikeProbability[data.predict$StrikeProbability>1]<-1
data.predict$StrikeProbability[data.predict$StrikeProbability<0]<-0

# construct the heat map
ggplot(kZone, aes(platex, platez)) +
  geom_tile(data=data.predict, 
            aes(x=platex, y=platez, fill= StrikeProbability)) +
  scale_fill_distiller(palette = "Spectral") +
  geom_path(lwd=1.5, col="black") +
  coord_fixed() +
  # ggtitle("Final Strike Prob Test Vs. RHH")
  ggtitle("Final Strike Prob Test Vs. LHH")
#Definitely some things that would have to be smoothed out if we were presenting
#these heat maps to players or coaches, but those heat maps look good overall.

predictionsAll<-strikeProbDF$PredictionFinal
choiceAll<-ifelse(predictionsAll>=0.5,1,0)
accuracyAll<-sum(choiceAll==strikeProbDF$is_strike)/nrow(strikeProbDF)#94.84039%
BrierAll<-sum((choiceAll-strikeProbDF$is_strike)^2)/nrow(strikeProbDF)#5.159608%

#Checking how well calibrated the model is...it's looking good.
with(strikeProbDF,mean(PredictionFinal[PredictionFinal>=0.95]))
with(strikeProbDF,mean(is_strike[PredictionFinal>=0.95]))

with(strikeProbDF,mean(PredictionFinal[between(PredictionFinal,0.9,0.95)]))
with(strikeProbDF,mean(is_strike[between(PredictionFinal,0.9,0.95)]))

with(strikeProbDF,mean(PredictionFinal[between(PredictionFinal,0.75,0.9)]))
with(strikeProbDF,mean(is_strike[between(PredictionFinal,0.75,0.9)]))

with(strikeProbDF,mean(PredictionFinal[between(PredictionFinal,0.6,0.75)]))
with(strikeProbDF,mean(is_strike[between(PredictionFinal,0.6,0.75)]))

with(strikeProbDF,mean(PredictionFinal[between(PredictionFinal,0.4,0.6)]))
with(strikeProbDF,mean(is_strike[between(PredictionFinal,0.4,0.6)]))

with(strikeProbDF,mean(PredictionFinal[between(PredictionFinal,0.25,0.4)]))
with(strikeProbDF,mean(is_strike[between(PredictionFinal,0.25,0.4)]))

with(strikeProbDF,mean(PredictionFinal[between(PredictionFinal,0.1,0.25)]))
with(strikeProbDF,mean(is_strike[between(PredictionFinal,0.1,0.25)]))
     
with(strikeProbDF,mean(PredictionFinal[between(PredictionFinal,0,0.1)]))
with(strikeProbDF,mean(is_strike[between(PredictionFinal,0,0.1)]))

head(pitchesDF)
head(pitchesDF[pitchesDF$is_strike==0&pitchesDF$is_swing==0&pitchesDF$PredictionFinal>=0.9,])

#The pitches considered very likely to be strikes were consistently within the known
#(circular) strike zone
with(pitchesDF,summary(plate_location_z[between(PredictionFinal,0.9,1)]))

with(pitchesDF,summary(plate_location_x[between(PredictionFinal,0.9,1)]))

with(pitchesDF,summary(plate_location_z[between(PredictionFinal,0.9,1)&abs(plate_location_x)>0.75]))

#Generating the final output
colnames(pitchesDF)

outputDF<-pitchesDF[,c(1:23,43)]
colnames(outputDF)[24]<-"PredictedStrikeProb"
outputDF$BrierScore<-rep(BrierFinal)

BrierFinal
#To be very clear, that's the out-of-sample Brier score of 0.074.
#The  Brier score for all taken pitches was 0.052
head(outputDF)

# write.csv(outputDF,file="pitch_data_with_predictions.csv")

###Question 2
require(randomForest)
require(pre)
require(dplyr)

strikeoutDF<-read.csv(file="pitcher_strikeout_data.csv",header=T)

str(strikeoutDF)

#Every pitcher has data for all 5 years
byPitcher<-summarise(group_by(strikeoutDF,pitcher_id),
                     NumYears = length(unique(year)),
                     Count = n())

summary(byPitcher$Count)
summary(byPitcher$NumYears)

head(strikeoutDF)

summary(strikeoutDF$stuff)
sd(strikeoutDF$stuff)

#Get stuff into a more easily understandable form
strikeoutDF$Stuff28<-with(strikeoutDF,(-1)*(stuff-0.002449)/0.01311424*10+50)

table(strikeoutDF$age)

#Grouping by season and expanding our sample from 200 to 1000
pitchersBySeason<-summarise(group_by(strikeoutDF,pitcher_id,year),
                            KRate = mean(is_strikeout),
                            BBRate = mean(is_walk),
                            Stuff28 = mean(Stuff28),
                            # StuffSD = sd(Stuff28), #Stuff is the same for each pitcher season
                            # StuffPerc75 = quantile(Stuff28,0.75),
                            # StuffPerc90 = quantile(Stuff28,0.9),
                            Age = mean(age),
                            BFCount = n())

head(pitchersBySeason)
nrow(pitchersBySeason)

#Joining in the prior seasons for each player
colnames(pitchersBySeason)
bySeasonPrior<-pitchersBySeason[,c(1:5,7)]
bySeasonPrior$year<-bySeasonPrior$year+1
colnames(bySeasonPrior)[3:6]<-paste0(colnames(bySeasonPrior)[3:6],"Prior")
head(bySeasonPrior)

pitchersBySeason<-left_join(pitchersBySeason,bySeasonPrior,by=c("pitcher_id","year"))

head(pitchersBySeason)

bySeason2Prior<-pitchersBySeason[,c(1:5,7)]
bySeason2Prior$year<-bySeason2Prior$year+2
colnames(bySeason2Prior)[3:6]<-paste0(colnames(bySeason2Prior)[3:6],"2Prior")
head(bySeason2Prior)

pitchersBySeason<-left_join(pitchersBySeason,bySeason2Prior,by=c("pitcher_id","year"))

bySeason3Prior<-pitchersBySeason[,c(1:5,7)]
bySeason3Prior$year<-bySeason3Prior$year+3
colnames(bySeason3Prior)[3:6]<-paste0(colnames(bySeason3Prior)[3:6],"3Prior")
head(bySeason3Prior)

pitchersBySeason<-left_join(pitchersBySeason,bySeason3Prior,by=c("pitcher_id","year"))

print.data.frame(head(pitchersBySeason))

#Joining in the following season's K%
colnames(pitchersBySeason)
bySeasonNext<-pitchersBySeason[,c(1:3)]
bySeasonNext$year<-bySeasonNext$year-1
colnames(bySeasonNext)[3]<-paste0(colnames(bySeasonNext)[3],"Next")
head(bySeasonNext)

pitchersBySeason<-left_join(pitchersBySeason,bySeasonNext,by=c("pitcher_id","year"))

print.data.frame(head(pitchersBySeason))

summary(pitchersBySeason$KRateNext)
summary(pitchersBySeason$KRateNext[pitchersBySeason$year<2024])

summary(pitchersBySeason)

#Filling in BFCounts of 0 to facilitate calculations across years
pitchersBySeason$BFCountPrior[is.na(pitchersBySeason$BFCountPrior)]<-0
pitchersBySeason$BFCount2Prior[is.na(pitchersBySeason$BFCount2Prior)]<-0
pitchersBySeason$BFCount3Prior[is.na(pitchersBySeason$BFCount3Prior)]<-0

#Given the multiple years of data with different sample sizes for each year, the best prediction
#will feature versions of K%, BB%, and Stuff that are a weighted average on a per-BF basis of each metric
#across all of the seasons, with the possibility of a recency weighting that increases the importance of the 
#current season and/or decreases the relative weight of prior seasons. Ideally, we would be able to use different
#weights depending on the player's situation - e.g. maybe the most recent season matters more for K% if a pitcher
#shows a significant difference in stuff from the prior season. However, for a machine learning model that's taking
#in every variable, the collinearity between all of these different versions of each metric is not ideal.
#It especially may mess up the variable importance graph. I'm going to proceed with caution, but it will be
#important to have some sort of baseline of a simpler model where I can try to just pick the best versions
#of each variable.

pitchersBySeason$KRatePrior[pitchersBySeason$BFCountPrior==0]<-(-300)
pitchersBySeason$BBRatePrior[pitchersBySeason$BFCountPrior==0]<-(-300)
pitchersBySeason$Stuff28Prior[pitchersBySeason$BFCountPrior==0]<-(-300)

print.data.frame(head(pitchersBySeason))

#Filling in dummy values to facilitate calculations and make errors obvious
pitchersBySeason$KRate2Prior[pitchersBySeason$BFCount2Prior==0]<-(-300)
pitchersBySeason$BBRate2Prior[pitchersBySeason$BFCount2Prior==0]<-(-300)
pitchersBySeason$Stuff282Prior[pitchersBySeason$BFCount2Prior==0]<-(-300)

pitchersBySeason$KRate3Prior[pitchersBySeason$BFCount3Prior==0]<-(-300)
pitchersBySeason$BBRate3Prior[pitchersBySeason$BFCount3Prior==0]<-(-300)
pitchersBySeason$Stuff283Prior[pitchersBySeason$BFCount3Prior==0]<-(-300)

pitchersBySeason$TotalBF<-with(pitchersBySeason,BFCount+BFCountPrior+BFCount2Prior+BFCount3Prior)
summary(pitchersBySeason$TotalBF)
sum(pitchersBySeason$TotalBF<60)
pitchersBySeason$TotalBFLast3Years<-with(pitchersBySeason,BFCount+BFCountPrior+BFCount2Prior)
pitchersBySeason$TotalBFLast2Years<-with(pitchersBySeason,BFCount+BFCountPrior)

pitchersBySeason$KRateWeightedEven<-with(pitchersBySeason,BFCount/TotalBF*KRate+BFCountPrior/TotalBF*KRatePrior+
                                           BFCount2Prior/TotalBF*KRate2Prior+BFCount3Prior/TotalBF*KRate3Prior)
summary(pitchersBySeason$KRateWeightedEven)
summary(pitchersBySeason$KRate)

#60 is the stabilization point of K%, 0.2332 is the mean K% above
pitchersBySeason$KRateWeightedEvenRegressed<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*KRateWeightedEven+(1-TotalBF/60)*0.2332,KRateWeightedEven))
summary(pitchersBySeason$KRateWeightedEvenRegressed)
#That looks like a much better distribution on that edges. We should probably use that for everyone

pitchersBySeason$BBRateWeightedEven<-with(pitchersBySeason,BFCount/TotalBF*BBRate+BFCountPrior/TotalBF*BBRatePrior+
                                            BFCount2Prior/TotalBF*BBRate2Prior+BFCount3Prior/TotalBF*BBRate3Prior)
summary(pitchersBySeason$BBRateWeightedEven)
summary(pitchersBySeason$BBRate)

#The stabilization point for BB% is 120, so I could jump to that, but if I were doing this on real data, 
#it wouldn't be good to throw out an extreme BB% in either direction in a pretty decent sample
pitchersBySeason$BBRateWeightedEven<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*BBRateWeightedEven+(1-TotalBF/60)*0.08153,BBRateWeightedEven))

pitchersBySeason$Stuff28WeightedEven<-with(pitchersBySeason,BFCount/TotalBF*Stuff28+BFCountPrior/TotalBF*Stuff28Prior+
                                             BFCount2Prior/TotalBF*Stuff282Prior+BFCount3Prior/TotalBF*Stuff283Prior)
summary(pitchersBySeason$Stuff28WeightedEven)
summary(pitchersBySeason$Stuff28)

#Stuff28 should not be regressed given its much faster stability, though prior seasons are certainly of note,
#especially for a player with a lower sample.

pitchersBySeason$TotalBFRecent5x<-with(pitchersBySeason,5*BFCount+BFCountPrior+BFCount2Prior+BFCount3Prior)

pitchersBySeason$KRateRecent5x<-with(pitchersBySeason,5*BFCount/TotalBFRecent5x*KRate+BFCountPrior/TotalBFRecent5x*KRatePrior+
                                       BFCount2Prior/TotalBFRecent5x*KRate2Prior+BFCount3Prior/TotalBFRecent5x*KRate3Prior)
summary(pitchersBySeason$KRateRecent5x)
summary(pitchersBySeason$KRate)

#60 is the stabilization point of K%, 0.2332 is the mean K% above
pitchersBySeason$KRateRecent5xRegressed<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*KRateRecent5x+(1-TotalBF/60)*0.2332,KRateRecent5x))
summary(pitchersBySeason$KRateRecent5xRegressed)

pitchersBySeason$BBRateRecent5x<-with(pitchersBySeason,5*BFCount/TotalBFRecent5x*BBRate+BFCountPrior/TotalBFRecent5x*BBRatePrior+
                                        BFCount2Prior/TotalBFRecent5x*BBRate2Prior+BFCount3Prior/TotalBFRecent5x*BBRate3Prior)
summary(pitchersBySeason$BBRateRecent5x)
summary(pitchersBySeason$BBRate)

#The stabilization point for BB% is 120, so I could jump to that, but if I were doing this on real data, 
#it wouldn't be good to throw out an extreme BB% in either direction in a pretty decent sample
pitchersBySeason$BBRateRecent5x<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*BBRateRecent5x+(1-TotalBF/60)*0.08153,BBRateRecent5x))

pitchersBySeason$Stuff28Recent5x<-with(pitchersBySeason,5*BFCount/TotalBFRecent5x*Stuff28+BFCountPrior/TotalBFRecent5x*Stuff28Prior+
                                         BFCount2Prior/TotalBFRecent5x*Stuff282Prior+BFCount3Prior/TotalBFRecent5x*Stuff283Prior)
summary(pitchersBySeason$Stuff28Recent5x)
summary(pitchersBySeason$Stuff28)

#The minimum weightings to get a ton of combinations:
#Even all, 5x recent (the rest the same), 33% decay, 5x 33% decay

#A lot of copy-and-paste for all of these different weightings.

pitchersBySeason$TotalBFDecay0.33<-with(pitchersBySeason,BFCount+0.67*BFCountPrior+0.34*BFCount2Prior+0.01*BFCount3Prior)

pitchersBySeason$KRateDecay0.33<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.33*KRate+0.67*BFCountPrior/TotalBFDecay0.33*KRatePrior+
                                        0.34*BFCount2Prior/TotalBFDecay0.33*KRate2Prior+0.01*BFCount3Prior/TotalBFDecay0.33*KRate3Prior)
summary(pitchersBySeason$KRateDecay0.33)
summary(pitchersBySeason$KRate)

#60 is the stabilization point of K%, 0.2332 is the mean K% above
pitchersBySeason$KRateDecay0.33Regressed<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*KRateDecay0.33+(1-TotalBF/60)*0.2332,KRateDecay0.33))
summary(pitchersBySeason$KRateDecay0.33Regressed)

pitchersBySeason$BBRateDecay0.33<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.33*BBRate+0.67*BFCountPrior/TotalBFDecay0.33*BBRatePrior+
                                         0.34*BFCount2Prior/TotalBFDecay0.33*BBRate2Prior+0.01*BFCount3Prior/TotalBFDecay0.33*BBRate3Prior)
summary(pitchersBySeason$BBRateDecay0.33)
summary(pitchersBySeason$BBRate)

pitchersBySeason$BBRateDecay0.33<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*BBRateDecay0.33+(1-TotalBF/60)*0.08153,BBRateDecay0.33))

pitchersBySeason$Stuff28Decay0.33<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.33*Stuff28+0.67*BFCountPrior/TotalBFDecay0.33*Stuff28Prior+
                                          0.34*BFCount2Prior/TotalBFDecay0.33*Stuff282Prior+0.01*BFCount3Prior/TotalBFDecay0.33*Stuff283Prior)
summary(pitchersBySeason$Stuff28Decay0.33)
summary(pitchersBySeason$Stuff28)


pitchersBySeason$TotalBFRecent5xDecay0.33<-with(pitchersBySeason,5*BFCount+0.67*BFCountPrior+0.34*BFCount2Prior+0.01*BFCount3Prior)

pitchersBySeason$KRateRecent5xDecay0.33<-with(pitchersBySeason,5*BFCount/TotalBFRecent5xDecay0.33*KRate+0.67*BFCountPrior/TotalBFRecent5xDecay0.33*KRatePrior+
                                                0.34*BFCount2Prior/TotalBFRecent5xDecay0.33*KRate2Prior+0.01*BFCount3Prior/TotalBFRecent5xDecay0.33*KRate3Prior)
summary(pitchersBySeason$KRateRecent5xDecay0.33)
summary(pitchersBySeason$KRate)

#60 is the stabilization point of K%, 0.2332 is the mean K% above
pitchersBySeason$KRateRecent5xDecay0.33Regressed<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*KRateRecent5xDecay0.33+(1-TotalBF/60)*0.2332,KRateRecent5xDecay0.33))
summary(pitchersBySeason$KRateRecent5xDecay0.33Regressed)

pitchersBySeason$BBRateRecent5xDecay0.33<-with(pitchersBySeason,5*BFCount/TotalBFRecent5xDecay0.33*BBRate+0.67*BFCountPrior/TotalBFRecent5xDecay0.33*BBRatePrior+
                                                 0.34*BFCount2Prior/TotalBFRecent5xDecay0.33*BBRate2Prior+0.01*BFCount3Prior/TotalBFRecent5xDecay0.33*BBRate3Prior)
summary(pitchersBySeason$BBRateRecent5xDecay0.33)
summary(pitchersBySeason$BBRate)

pitchersBySeason$BBRateRecent5xDecay0.33<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*BBRateRecent5xDecay0.33+(1-TotalBF/60)*0.08153,BBRateRecent5xDecay0.33))

pitchersBySeason$Stuff28Recent5xDecay0.33<-with(pitchersBySeason,5*BFCount/TotalBFRecent5xDecay0.33*Stuff28+0.67*BFCountPrior/TotalBFRecent5xDecay0.33*Stuff28Prior+
                                                  0.34*BFCount2Prior/TotalBFRecent5xDecay0.33*Stuff282Prior+0.01*BFCount3Prior/TotalBFRecent5xDecay0.33*Stuff283Prior)
summary(pitchersBySeason$Stuff28Recent5xDecay0.33)
summary(pitchersBySeason$Stuff28)

summary(pitchersBySeason$KRate)
summary(pitchersBySeason$KRateWeightedEven)
summary(pitchersBySeason$KRateRecent5x)
summary(pitchersBySeason$KRateDecay0.33)
summary(pitchersBySeason$KRateRecent5xDecay0.33)

summary(pitchersBySeason$KRateRegressed)
summary(pitchersBySeason$KRateWeightedEvenRegressed)
summary(pitchersBySeason$KRateRecent5xRegressed)
summary(pitchersBySeason$KRateDecay0.33Regressed)
summary(pitchersBySeason$KRateRecent5xDecay0.33Regressed)

pitchersBySeason$KRateRegressed<-with(pitchersBySeason,ifelse(BFCount<60,
                                                              ifelse(TotalBF>=60,BFCount/60*KRate+(1-BFCount/60)*KRateWeightedEven,
                                                                     BFCount/60*KRate+(1-BFCount/60)*0.2332),KRate))
summary(pitchersBySeason$KRateRegressed)
summary(pitchersBySeason$KRate)

pitchersBySeason$BBRateRegressed<-with(pitchersBySeason,ifelse(BFCount<60,
                                                               ifelse(TotalBF>=60,BFCount/60*BBRate+(1-BFCount/60)*BBRateWeightedEven,
                                                                      BFCount/60*BBRate+(1-BFCount/60)*0.08153),BBRate))
summary(pitchersBySeason$BBRateRegressed)
summary(pitchersBySeason$BBRate)

pitchersBySeason$KEvenDiff<-with(pitchersBySeason,KRateWeightedEvenRegressed-KRateRegressed)
summary(pitchersBySeason$KEvenDiff)

pitchersBySeason$KRecent5xDiff<-with(pitchersBySeason,KRateRecent5xRegressed-KRateRegressed)
summary(pitchersBySeason$KRecent5xDiff)

pitchersBySeason$KDecay0.33Diff<-with(pitchersBySeason,KRateDecay0.33Regressed-KRateRegressed)
summary(pitchersBySeason$KDecay0.33Diff)

pitchersBySeason$KRecent5xDecay0.33Diff<-with(pitchersBySeason,KRateRecent5xDecay0.33Regressed-KRateRegressed)
summary(pitchersBySeason$KRecent5xDecay0.33Diff)

pitchersBySeason$BBEvenDiff<-with(pitchersBySeason,BBRateWeightedEven-BBRateRegressed)
summary(pitchersBySeason$BBEvenDiff)

pitchersBySeason$BBRecent5xDiff<-with(pitchersBySeason,BBRateRecent5x-BBRateRegressed)
summary(pitchersBySeason$BBRecent5xDiff)

pitchersBySeason$BBDecay0.33Diff<-with(pitchersBySeason,BBRateDecay0.33-BBRateRegressed)
summary(pitchersBySeason$BBDecay0.33Diff)

pitchersBySeason$BBRecent5xDecay0.33Diff<-with(pitchersBySeason,BBRateRecent5xDecay0.33-BBRateRegressed)
summary(pitchersBySeason$BBRecent5xDecay0.33Diff)

#Not using regressed stuff because of the faster stabilization
pitchersBySeason$StuffEvenDiff<-with(pitchersBySeason,Stuff28WeightedEven-Stuff28)
summary(pitchersBySeason$StuffEvenDiff)

pitchersBySeason$StuffRecent5xDiff<-with(pitchersBySeason,Stuff28Recent5x-Stuff28)
summary(pitchersBySeason$StuffRecent5xDiff)

pitchersBySeason$StuffDecay0.33Diff<-with(pitchersBySeason,Stuff28Decay0.33-Stuff28)
summary(pitchersBySeason$StuffDecay0.33Diff)

pitchersBySeason$StuffRecent5xDecay0.33Diff<-with(pitchersBySeason,Stuff28Recent5xDecay0.33-Stuff28)
summary(pitchersBySeason$StuffRecent5xDecay0.33Diff)

print.data.frame(head(pitchersBySeason))

bySeasonToBuild<-pitchersBySeason[pitchersBySeason$year<2024,]
#Sample size of 800 for the model build, we have no next season for 2024.
#Obviously, we'll use 2024 for the predict.

colnames(bySeasonToBuild)
bySeasonToUse<-bySeasonToBuild[,c(20,5:7,21,43:56)]
#That's the version with the difference terms but no weightings
#I also used the regressed K% and BB% after confirming that they make more sense
#in initial testing

#I also tried a version with all the weightings, but it didn't do as well


head(bySeasonToUse)

round(cor(bySeasonToUse), digits = 2L)
#checking the correlation matrix - certainly the differences are correlated with each other, but at least
#they're not correlated with the main terms

#I could do this with caret again, but doing something different this time
#I'm also using a Random Forest rather than an XGBoost - I probably like XGBoost more 
#overall, but especially without categorical variables, this is a good use case for Random Forest.
division <- sample(2, nrow(bySeasonToUse), replace = TRUE, prob = c(0.8, 0.2))
training <- bySeasonToUse[division==1,]
testing <- bySeasonToUse[division==2,]

head(training)

KRateMod0 <- randomForest(KRateNext~., data=training, proximity=TRUE) 
print(KRateMod0)

testing$Prediction<-predict(KRateMod0,testing,type="response")

print.data.frame(head(testing))

summary(testing$KRateNext)
summary(testing$Prediction)

#I gave this function from the Boruta package a try, but it wasn't helpful
# Importance <- Boruta(KRateNext~ ., data = training)
# print(Importance)

imp<-as.data.frame(randomForest::importance(KRateMod0))
imp$Variables<-0
imp[order(-imp$IncNodePurity),]

# imp2<-as.data.frame(randomForest::importance(KRateMod0.1))
# imp2$Variables<-0
# imp2[order(-imp2$IncNodePurity),]

#Using a penalizing methodology to try to get a better feel for variable importance
penalizedModel <- pre(KRateNext ~ ., data = training, mtry = 6, alpha = 0)
# penalizedModel0.1 <- pre(KRateNext ~ ., data = training, mtry = 6, alpha = 0)

#Putting the variable importances next to each other, unpenalized and penalized
rf_imp <- randomForest::importance(KRateMod0) 
par(mfrow = c(1,2))
barplot(t(rf_imp), main = "random forest")
pre::importance(penalizedModel, main = "prediction rule ensemble")

training<-as.data.frame(training)

#Tuning the mtry term in the model and fitting the optimal model
tunedModel <- tuneRF(training[,-1], training[,1],
                     stepFactor = 2,
                     plot = TRUE,
                     ntreeTry = 500,
                     trace = TRUE,
                     improve = 0.05,
                     doBest = TRUE)
print(tunedModel)

testing$PredictionTuned<-predict(tunedModel,testing,type="response")

print.data.frame(head(testing))

summary(testing$KRateNext)
summary(testing$Prediction)
summary(testing$PredictionTuned)

#The tuning doesn't seem to be having a major effect
with(testing,caret::RMSE(pred=Prediction,obs = KRateNext))
with(testing,caret::RMSE(pred=PredictionTuned,obs = KRateNext))

#Checking the variable importance once more time
impFinal<-as.data.frame(randomForest::importance(tunedModel))
impFinal$Variables<-0#Just to make the dataframe display better
impFinal[order(-impFinal$IncNodePurity),]

penalizedModelFinal <- pre(KRateNext ~ ., data = training, mtry = 12, alpha = 0)
pre::importance(penalizedModelFinal, main = "prediction rule ensemble")


#Checking the 2025 predictions
bySeasonToPredict<-pitchersBySeason[pitchersBySeason$year==2024,c(3:7,21,43:56)]
summary(bySeasonToPredict)

head(bySeasonToPredict)

bySeasonToPredict$Prediction<-predict(tunedModel,bySeasonToPredict,type="response")

print.data.frame(head(bySeasonToPredict))

with(bySeasonToPredict,summary(Prediction-KRateRegressed))
with(bySeasonToPredict,summary(Prediction-KRate))

#Small sample sizes and just really low numbers
print.data.frame(head(bySeasonToPredict[with(bySeasonToPredict,Prediction-KRate>.05),]))

#Small samples, really high K% marks, and players with declining stuff
print.data.frame(head(bySeasonToPredict[with(bySeasonToPredict,Prediction-KRateRegressed<(-.05)),]))

#This model is probably good, but I'm curious to compare it to a simpler methodology that I can go deeper into

nrow(bySeasonToBuild)

#Adding a few more variables to explore with the LM
pitchersBySeason$KRateRecent2.5xRegressed<-with(pitchersBySeason,0.5*KRateRecent5xRegressed+0.5*KRateWeightedEvenRegressed)
summary(pitchersBySeason$KRateRecent2.5xRegressed)
summary(pitchersBySeason$KRateRecent5xRegressed)
summary(pitchersBySeason$KRateWeightedEvenRegressed)

pitchersBySeason$KRateRecent1.5xRegressed<-with(pitchersBySeason,0.3*KRateRecent5xRegressed+0.7*KRateWeightedEvenRegressed)
summary(pitchersBySeason$KRateRecent1.5xRegressed)
summary(pitchersBySeason$KRateRecent5xRegressed)
summary(pitchersBySeason$KRateWeightedEvenRegressed)

pitchersBySeason$KRateRecent2.5xDecay0.33Regressed<-with(pitchersBySeason,0.5*KRateRecent5xDecay0.33Regressed+0.5*KRateDecay0.33Regressed)
summary(pitchersBySeason$KRateRecent2.5xDecay0.33Regressed)
summary(pitchersBySeason$KRateRecent5xDecay0.33Regressed)
summary(pitchersBySeason$KRateDecay0.33Regressed)

pitchersBySeason$KRateRecent1.5xDecay0.33Regressed<-with(pitchersBySeason,0.3*KRateRecent5xDecay0.33Regressed+0.7*KRateDecay0.33Regressed)
summary(pitchersBySeason$KRateRecent1.5xDecay0.33Regressed)
summary(pitchersBySeason$KRateRecent5xDecay0.33Regressed)
summary(pitchersBySeason$KRateDecay0.33Regressed)

pitchersBySeason$TotalBFDecay0.1<-with(pitchersBySeason,BFCount+0.9*BFCountPrior+0.8*BFCount2Prior+0.7*BFCount3Prior)

pitchersBySeason$KRateDecay0.1<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.1*KRate+0.9*BFCountPrior/TotalBFDecay0.1*KRatePrior+
                                       0.8*BFCount2Prior/TotalBFDecay0.1*KRate2Prior+0.7*BFCount3Prior/TotalBFDecay0.1*KRate3Prior)
summary(pitchersBySeason$KRateDecay0.1)
summary(pitchersBySeason$KRate)

pitchersBySeason$KRateDecay0.1Regressed<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*KRateDecay0.1+(1-TotalBF/60)*0.2332,KRateDecay0.1))
summary(pitchersBySeason$KRateDecay0.1Regressed)

pitchersBySeason$BBRateDecay0.1<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.1*BBRate+0.9*BFCountPrior/TotalBFDecay0.1*BBRatePrior+
                                        0.8*BFCount2Prior/TotalBFDecay0.1*BBRate2Prior+0.7*BFCount3Prior/TotalBFDecay0.1*BBRate3Prior)
summary(pitchersBySeason$BBRateDecay0.1)
summary(pitchersBySeason$BBRate)

pitchersBySeason$BBRateDecay0.1<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*BBRateDecay0.1+(1-TotalBF/60)*0.08153,BBRateDecay0.1))

pitchersBySeason$Stuff28Decay0.1<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.1*Stuff28+0.9*BFCountPrior/TotalBFDecay0.1*Stuff28Prior+
                                         0.8*BFCount2Prior/TotalBFDecay0.1*Stuff282Prior+0.7*BFCount3Prior/TotalBFDecay0.1*Stuff283Prior)
summary(pitchersBySeason$Stuff28Decay0.1)
summary(pitchersBySeason$Stuff28)

pitchersBySeason$TotalBFDecay0.25<-with(pitchersBySeason,BFCount+0.75*BFCountPrior+0.5*BFCount2Prior+0.25*BFCount3Prior)

pitchersBySeason$KRateDecay0.25<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.25*KRate+0.75*BFCountPrior/TotalBFDecay0.25*KRatePrior+
                                        0.5*BFCount2Prior/TotalBFDecay0.25*KRate2Prior+0.25*BFCount3Prior/TotalBFDecay0.25*KRate3Prior)
summary(pitchersBySeason$KRateDecay0.25)
summary(pitchersBySeason$KRate)

pitchersBySeason$KRateDecay0.25Regressed<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*KRateDecay0.25+(1-TotalBF/60)*0.2332,KRateDecay0.25))
summary(pitchersBySeason$KRateDecay0.25Regressed)

pitchersBySeason$BBRateDecay0.25<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.25*BBRate+0.75*BFCountPrior/TotalBFDecay0.25*BBRatePrior+
                                         0.5*BFCount2Prior/TotalBFDecay0.25*BBRate2Prior+0.25*BFCount3Prior/TotalBFDecay0.25*BBRate3Prior)
summary(pitchersBySeason$BBRateDecay0.25)
summary(pitchersBySeason$BBRate)

pitchersBySeason$BBRateDecay0.25<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*BBRateDecay0.25+(1-TotalBF/60)*0.08153,BBRateDecay0.25))

pitchersBySeason$Stuff28Decay0.25<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.25*Stuff28+0.75*BFCountPrior/TotalBFDecay0.25*Stuff28Prior+
                                          0.5*BFCount2Prior/TotalBFDecay0.25*Stuff282Prior+0.25*BFCount3Prior/TotalBFDecay0.25*Stuff283Prior)
summary(pitchersBySeason$Stuff28Decay0.25)
summary(pitchersBySeason$Stuff28)

pitchersBySeason$TotalBFDecay0.5<-with(pitchersBySeason,BFCount+0.5*BFCountPrior+0*BFCount2Prior+0*BFCount3Prior)

pitchersBySeason$KRateDecay0.5<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.5*KRate+0.5*BFCountPrior/TotalBFDecay0.5*KRatePrior+
                                       0*BFCount2Prior/TotalBFDecay0.5*KRate2Prior+0*BFCount3Prior/TotalBFDecay0.5*KRate3Prior)
summary(pitchersBySeason$KRateDecay0.5)
summary(pitchersBySeason$KRate)

pitchersBySeason$KRateDecay0.5Regressed<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*KRateDecay0.5+(1-TotalBF/60)*0.2332,KRateDecay0.5))
summary(pitchersBySeason$KRateDecay0.5Regressed)

pitchersBySeason$BBRateDecay0.5<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.5*BBRate+0.5*BFCountPrior/TotalBFDecay0.5*BBRatePrior+
                                        0*BFCount2Prior/TotalBFDecay0.5*BBRate2Prior+0*BFCount3Prior/TotalBFDecay0.5*BBRate3Prior)
summary(pitchersBySeason$BBRateDecay0.5)
summary(pitchersBySeason$BBRate)

pitchersBySeason$BBRateDecay0.5<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*BBRateDecay0.5+(1-TotalBF/60)*0.08153,BBRateDecay0.5))

pitchersBySeason$Stuff28Decay0.5<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.5*Stuff28+0.5*BFCountPrior/TotalBFDecay0.5*Stuff28Prior+
                                         0*BFCount2Prior/TotalBFDecay0.5*Stuff282Prior+0*BFCount3Prior/TotalBFDecay0.5*Stuff283Prior)
summary(pitchersBySeason$Stuff28Decay0.5)
summary(pitchersBySeason$Stuff28)

pitchersBySeason$TotalBFDecay0.45<-with(pitchersBySeason,BFCount+0.55*BFCountPrior+0.1*BFCount2Prior+0*BFCount3Prior)

pitchersBySeason$KRateDecay0.45<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.45*KRate+0.55*BFCountPrior/TotalBFDecay0.45*KRatePrior+
                                        0.1*BFCount2Prior/TotalBFDecay0.45*KRate2Prior+0*BFCount3Prior/TotalBFDecay0.45*KRate3Prior)
summary(pitchersBySeason$KRateDecay0.45)
summary(pitchersBySeason$KRate)

pitchersBySeason$KRateDecay0.45Regressed<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*KRateDecay0.45+(1-TotalBF/60)*0.2332,KRateDecay0.45))
summary(pitchersBySeason$KRateDecay0.45Regressed)

pitchersBySeason$BBRateDecay0.45<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.45*BBRate+0.55*BFCountPrior/TotalBFDecay0.45*BBRatePrior+
                                         0.1*BFCount2Prior/TotalBFDecay0.45*BBRate2Prior+0*BFCount3Prior/TotalBFDecay0.45*BBRate3Prior)
summary(pitchersBySeason$BBRateDecay0.45)
summary(pitchersBySeason$BBRate)

pitchersBySeason$BBRateDecay0.45<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*BBRateDecay0.45+(1-TotalBF/60)*0.08153,BBRateDecay0.45))

pitchersBySeason$Stuff28Decay0.45<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.45*Stuff28+0.55*BFCountPrior/TotalBFDecay0.45*Stuff28Prior+
                                          0.1*BFCount2Prior/TotalBFDecay0.45*Stuff282Prior+0*BFCount3Prior/TotalBFDecay0.45*Stuff283Prior)
summary(pitchersBySeason$Stuff28Decay0.45)
summary(pitchersBySeason$Stuff28)

pitchersBySeason$TotalBFDecay0.5Lim0.1<-with(pitchersBySeason,BFCount+0.5*BFCountPrior+0.1*BFCount2Prior+0.1*BFCount3Prior)

pitchersBySeason$KRateDecay0.5Lim0.1<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.5Lim0.1*KRate+0.5*BFCountPrior/TotalBFDecay0.5Lim0.1*KRatePrior+
                                             0.1*BFCount2Prior/TotalBFDecay0.5Lim0.1*KRate2Prior+0.1*BFCount3Prior/TotalBFDecay0.5Lim0.1*KRate3Prior)
summary(pitchersBySeason$KRateDecay0.5Lim0.1)
summary(pitchersBySeason$KRate)

pitchersBySeason$KRateDecay0.5Lim0.1Regressed<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*KRateDecay0.5Lim0.1+(1-TotalBF/60)*0.2332,KRateDecay0.5Lim0.1))
summary(pitchersBySeason$KRateDecay0.5Lim0.1Regressed)

pitchersBySeason$BBRateDecay0.5Lim0.1<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.5Lim0.1*BBRate+0.5*BFCountPrior/TotalBFDecay0.5Lim0.1*BBRatePrior+
                                              0.1*BFCount2Prior/TotalBFDecay0.5Lim0.1*BBRate2Prior+0.1*BFCount3Prior/TotalBFDecay0.5Lim0.1*BBRate3Prior)
summary(pitchersBySeason$BBRateDecay0.5Lim0.1)
summary(pitchersBySeason$BBRate)

pitchersBySeason$BBRateDecay0.5Lim0.1<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*BBRateDecay0.5Lim0.1+(1-TotalBF/60)*0.08153,BBRateDecay0.5Lim0.1))

pitchersBySeason$Stuff28Decay0.5Lim0.1<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.5Lim0.1*Stuff28+0.5*BFCountPrior/TotalBFDecay0.5Lim0.1*Stuff28Prior+
                                               0.1*BFCount2Prior/TotalBFDecay0.5Lim0.1*Stuff282Prior+0.1*BFCount3Prior/TotalBFDecay0.5Lim0.1*Stuff283Prior)
summary(pitchersBySeason$Stuff28Decay0.5Lim0.1)
summary(pitchersBySeason$Stuff28)

pitchersBySeason$TotalBFDecayHalfEachTime<-with(pitchersBySeason,BFCount+0.5*BFCountPrior+0.25*BFCount2Prior+0.125*BFCount3Prior)

pitchersBySeason$KRateDecayHalfEachTime<-with(pitchersBySeason,1*BFCount/TotalBFDecayHalfEachTime*KRate+0.5*BFCountPrior/TotalBFDecayHalfEachTime*KRatePrior+
                                                0.25*BFCount2Prior/TotalBFDecayHalfEachTime*KRate2Prior+0.125*BFCount3Prior/TotalBFDecayHalfEachTime*KRate3Prior)
summary(pitchersBySeason$KRateDecayHalfEachTime)
summary(pitchersBySeason$KRate)

pitchersBySeason$KRateDecayHalfEachTimeRegressed<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*KRateDecayHalfEachTime+(1-TotalBF/60)*0.2332,KRateDecayHalfEachTime))
summary(pitchersBySeason$KRateDecayHalfEachTimeRegressed)

pitchersBySeason$BBRateDecayHalfEachTime<-with(pitchersBySeason,1*BFCount/TotalBFDecayHalfEachTime*BBRate+0.5*BFCountPrior/TotalBFDecayHalfEachTime*BBRatePrior+
                                                 0.25*BFCount2Prior/TotalBFDecayHalfEachTime*BBRate2Prior+0.125*BFCount3Prior/TotalBFDecayHalfEachTime*BBRate3Prior)
summary(pitchersBySeason$BBRateDecayHalfEachTime)
summary(pitchersBySeason$BBRate)

pitchersBySeason$BBRateDecayHalfEachTime<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*BBRateDecayHalfEachTime+(1-TotalBF/60)*0.08153,BBRateDecayHalfEachTime))

pitchersBySeason$Stuff28DecayHalfEachTime<-with(pitchersBySeason,1*BFCount/TotalBFDecayHalfEachTime*Stuff28+0.5*BFCountPrior/TotalBFDecayHalfEachTime*Stuff28Prior+
                                                  0.25*BFCount2Prior/TotalBFDecayHalfEachTime*Stuff282Prior+0.125*BFCount3Prior/TotalBFDecayHalfEachTime*Stuff283Prior)
summary(pitchersBySeason$Stuff28DecayHalfEachTime)
summary(pitchersBySeason$Stuff28)

pitchersBySeason$TotalBFDecay0.45Lim0.05<-with(pitchersBySeason,BFCount+0.45*BFCountPrior+0.1*BFCount2Prior+0.05*BFCount3Prior)

pitchersBySeason$KRateDecay0.45Lim0.05<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.45Lim0.05*KRate+0.45*BFCountPrior/TotalBFDecay0.45Lim0.05*KRatePrior+
                                               0.1*BFCount2Prior/TotalBFDecay0.45Lim0.05*KRate2Prior+0.05*BFCount3Prior/TotalBFDecay0.45Lim0.05*KRate3Prior)
summary(pitchersBySeason$KRateDecay0.45Lim0.05)
summary(pitchersBySeason$KRate)

pitchersBySeason$KRateDecay0.45Lim0.05Regressed<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*KRateDecay0.45Lim0.05+(1-TotalBF/60)*0.2332,KRateDecay0.45Lim0.05))
summary(pitchersBySeason$KRateDecay0.45Lim0.05Regressed)

pitchersBySeason$BBRateDecay0.45Lim0.05<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.45Lim0.05*BBRate+0.45*BFCountPrior/TotalBFDecay0.45Lim0.05*BBRatePrior+
                                                0.1*BFCount2Prior/TotalBFDecay0.45Lim0.05*BBRate2Prior+0.05*BFCount3Prior/TotalBFDecay0.45Lim0.05*BBRate3Prior)
summary(pitchersBySeason$BBRateDecay0.45Lim0.05)
summary(pitchersBySeason$BBRate)

pitchersBySeason$BBRateDecay0.45Lim0.05<-with(pitchersBySeason,ifelse(TotalBF<60,TotalBF/60*BBRateDecay0.45Lim0.05+(1-TotalBF/60)*0.08153,BBRateDecay0.45Lim0.05))

pitchersBySeason$Stuff28Decay0.45Lim0.05<-with(pitchersBySeason,1*BFCount/TotalBFDecay0.45Lim0.05*Stuff28+0.45*BFCountPrior/TotalBFDecay0.45Lim0.05*Stuff28Prior+
                                                 0.1*BFCount2Prior/TotalBFDecay0.45Lim0.05*Stuff282Prior+0.05*BFCount3Prior/TotalBFDecay0.45Lim0.05*Stuff283Prior)
summary(pitchersBySeason$Stuff28Decay0.45Lim0.05)
summary(pitchersBySeason$Stuff28)

pitchersBySeason$TotalBFRecent10x<-with(pitchersBySeason,10*BFCount+BFCountPrior+BFCount2Prior+BFCount3Prior)

pitchersBySeason$Stuff28Recent10x<-with(pitchersBySeason,10*BFCount/TotalBFRecent10x*Stuff28+BFCountPrior/TotalBFRecent10x*Stuff28Prior+
                                          BFCount2Prior/TotalBFRecent10x*Stuff282Prior+BFCount3Prior/TotalBFRecent10x*Stuff283Prior)
summary(pitchersBySeason$Stuff28Recent10x)
summary(pitchersBySeason$Stuff28Recent5x)
summary(pitchersBySeason$Stuff28)

pitchersBySeason$TotalBFRecent50x<-with(pitchersBySeason,50*BFCount+BFCountPrior+BFCount2Prior+BFCount3Prior)


pitchersBySeason$Stuff28Recent50x<-with(pitchersBySeason,50*BFCount/TotalBFRecent50x*Stuff28+BFCountPrior/TotalBFRecent50x*Stuff28Prior+
                                          BFCount2Prior/TotalBFRecent50x*Stuff282Prior+BFCount3Prior/TotalBFRecent50x*Stuff283Prior)
summary(pitchersBySeason$Stuff28Recent50x)
summary(pitchersBySeason$Stuff28Recent5x)
summary(pitchersBySeason$Stuff28)

pitchersBySeason$TotalBFRecent100x<-with(pitchersBySeason,100*BFCount+BFCountPrior+BFCount2Prior+BFCount3Prior)

pitchersBySeason$Stuff28Recent100x<-with(pitchersBySeason,100*BFCount/TotalBFRecent100x*Stuff28+BFCountPrior/TotalBFRecent100x*Stuff28Prior+
                                           BFCount2Prior/TotalBFRecent100x*Stuff282Prior+BFCount3Prior/TotalBFRecent100x*Stuff283Prior)
summary(pitchersBySeason$Stuff28Recent100x)
summary(pitchersBySeason$Stuff28Recent5x)
summary(pitchersBySeason$Stuff28)

pitchersBySeason$Stuff28Perc75<-with(pitchersBySeason,0.75*Stuff28+0.25*Stuff28WeightedEven)
pitchersBySeason$Stuff28Perc90<-with(pitchersBySeason,0.9*Stuff28+0.1*Stuff28WeightedEven)
pitchersBySeason$Stuff28Perc95<-with(pitchersBySeason,0.95*Stuff28+0.05*Stuff28WeightedEven)
pitchersBySeason$Stuff28Perc99<-with(pitchersBySeason,0.99*Stuff28+0.01*Stuff28WeightedEven)
summary(pitchersBySeason$Stuff28)
summary(pitchersBySeason$Stuff28Perc75)
summary(pitchersBySeason$Stuff28Perc90)
summary(pitchersBySeason$Stuff28Perc95)
summary(pitchersBySeason$Stuff28Perc99)

summary(pitchersBySeason$KRateDecayHalfEachTime)
summary(pitchersBySeason$KRateDecay0.5)
summary(pitchersBySeason$KRateDecay0.5Lim0.1)
summary(pitchersBySeason$KRateDecay0.45)
summary(pitchersBySeason$KRateDecay0.45Lim0.05)
summary(pitchersBySeason$KRateDecay0.33)
summary(pitchersBySeason$KRateDecay0.25)
summary(pitchersBySeason$KRateDecay0.1)

pitchersBySeason$KDecay0.5Diff<-with(pitchersBySeason,KRateDecay0.5Regressed-KRateRegressed)
summary(pitchersBySeason$KDecay0.5Diff)
summary(pitchersBySeason$KDecay0.33Diff)

pitchersBySeason$KDecay0.5DiffVsEven<-with(pitchersBySeason,KRateDecay0.5Regressed-KRateWeightedEvenRegressed)
summary(pitchersBySeason$KDecay0.5Diff)
summary(pitchersBySeason$KDecay0.5DiffVsEven)


bySeasonToBuild<-pitchersBySeason[pitchersBySeason$year<2024,]


#Setting up My LM cross-validation involving 10-fold cross-validation with bootstrapping
#Comparing 10 models at a time.
#Unlike the Random Forest models, I'll pick things myself as we go and see if I can find something that beats the ML model.
#At the very least, I do some feature engineering before trying the Random Forest again

lmbootstrapcheckpreset<-function(fullDF,response,model,adjustment = 0){
  df1<-fullDF[sample1,]
  df1$Prediction<-predict(model,df1,type="response")
  df1$Error<-df1[,response]-df1$Prediction-adjustment
  # colnames(df1)[length(colnames(df1))]<-"Error"
  RMSEfor1<-sqrt(mean(df1$Error[[1]]^2))
  
  df2<-fullDF[sample2,]
  df2$Prediction<-predict(model,df2,type="response")
  df2$Error<-df2[,response]-df2$Prediction-adjustment
  RMSEfor2<-sqrt(mean(df2$Error[[1]]^2))
  
  df3<-fullDF[sample3,]
  df3$Prediction<-predict(model,df3,type="response")
  df3$Error<-df3[,response]-df3$Prediction-adjustment
  RMSEfor3<-sqrt(mean(df3$Error[[1]]^2))
  
  df4<-fullDF[sample4,]
  df4$Prediction<-predict(model,df4,type="response")
  df4$Error<-df4[,response]-df4$Prediction-adjustment
  RMSEfor4<-sqrt(mean(df4$Error[[1]]^2))
  
  df5<-fullDF[sample5,]
  df5$Prediction<-predict(model,df5,type="response")
  df5$Error<-df5[,response]-df5$Prediction-adjustment
  RMSEfor5<-sqrt(mean(df5$Error[[1]]^2))
  
  df6<-fullDF[sample6,]
  df6$Prediction<-predict(model,df6,type="response")
  df6$Error<-df6[,response]-df6$Prediction-adjustment
  RMSEfor6<-sqrt(mean(df6$Error[[1]]^2))
  
  df7<-fullDF[sample7,]
  df7$Prediction<-predict(model,df7,type="response")
  df7$Error<-df7[,response]-df7$Prediction-adjustment
  RMSEfor7<-sqrt(mean(df7$Error[[1]]^2))
  
  df8<-fullDF[sample8,]
  df8$Prediction<-predict(model,df8,type="response")
  df8$Error<-df8[,response]-df8$Prediction-adjustment
  RMSEfor8<-sqrt(mean(df8$Error[[1]]^2))
  
  df9<-fullDF[sample9,]
  df9$Prediction<-predict(model,df9,type="response")
  df9$Error<-df9[,response]-df9$Prediction-adjustment
  RMSEfor9<-sqrt(mean(df9$Error[[1]]^2))
  
  df10<-fullDF[sample10,]
  df10$Prediction<-predict(model,df10,type="response")
  df10$Error<-df10[,response]-df10$Prediction-adjustment
  RMSEfor10<-sqrt(mean(df10$Error[[1]]^2))
  
  output<-c(RMSEfor1,RMSEfor2,RMSEfor3,RMSEfor4,RMSEfor5,RMSEfor6,RMSEfor7,RMSEfor8,RMSEfor9,RMSEfor10)
  
}

#This was run quite a few times comparing a bunch of different models.
#See the Excel sheet Comparing 2025 Strikeout Rate Models

# 
# toSelect<-sample(1:nrow(bySeasonToBuild),round(0.8*nrow(bySeasonToBuild)),replace=T)
# strikeoutBuild<-bySeasonToBuild[toSelect,]
# print.data.frame(head(strikeoutBuild))
# 
# fit0<-lm(KRateNext~KRate,data=strikeoutBuild)
# summary(fit0)
# 
# fit1<-lm(KRateNext~KRateDecay0.5Regressed,data=strikeoutBuild)
# summary(fit1)
# 
# fit2<-lm(KRateNext~KRateDecay0.5Regressed+Stuff28,data=strikeoutBuild)
# summary(fit2)
# 
# fit3<-lm(KRateNext~KRateDecay0.5Regressed+Stuff28*BFCount,data=strikeoutBuild)
# summary(fit3)
# 
# fit4<-lm(KRateNext~KRateDecay0.5Regressed+Stuff28*BFCount+BBRateRecent5x,data=strikeoutBuild)
# summary(fit4)
# 
# fit5<-lm(KRateNext~KRateDecay0.5Regressed+Stuff28*BFCount+StuffEvenDiff,data=strikeoutBuild)
# summary(fit5)
# 
# fit6<-lm(KRateNext~KRateDecay0.5Regressed+Stuff28*BFCount+Age,data=strikeoutBuild)
# summary(fit6)
# 
# fit7<-lm(KRateNext~KRateDecay0.5Regressed+Stuff28*BFCount+Age+Age:Stuff28,data=strikeoutBuild)
# summary(fit7)
# 
# fit8<-lm(KRateNext~KRateDecay0.5Regressed+Stuff28*BFCount+Age*StuffEvenDiff,data=strikeoutBuild)
# summary(fit8)
# 
# fit9<-lm(KRateNext~KRateDecay0.5Regressed+BBRateRecent5x+Stuff28*BFCount+Age*StuffEvenDiff,data=strikeoutBuild)
# summary(fit9)
# 
# sample1<-sample(1:nrow(bySeasonToBuild),round(0.2*nrow(bySeasonToBuild)),replace=T)
# sample2<-sample(1:nrow(bySeasonToBuild),round(0.2*nrow(bySeasonToBuild)),replace=T)
# sample3<-sample(1:nrow(bySeasonToBuild),round(0.2*nrow(bySeasonToBuild)),replace=T)
# sample4<-sample(1:nrow(bySeasonToBuild),round(0.2*nrow(bySeasonToBuild)),replace=T)
# sample5<-sample(1:nrow(bySeasonToBuild),round(0.2*nrow(bySeasonToBuild)),replace=T)
# sample6<-sample(1:nrow(bySeasonToBuild),round(0.2*nrow(bySeasonToBuild)),replace=T)
# sample7<-sample(1:nrow(bySeasonToBuild),round(0.2*nrow(bySeasonToBuild)),replace=T)
# sample8<-sample(1:nrow(bySeasonToBuild),round(0.2*nrow(bySeasonToBuild)),replace=T)
# sample9<-sample(1:nrow(bySeasonToBuild),round(0.2*nrow(bySeasonToBuild)),replace=T)
# sample10<-sample(1:nrow(bySeasonToBuild),round(0.2*nrow(bySeasonToBuild)),replace=T)
# 
# 
# check0<-lmbootstrapcheckpreset(fullDF = bySeasonToBuild,response = "KRateNext",model=fit0)
# check1<-lmbootstrapcheckpreset(fullDF = bySeasonToBuild,response = "KRateNext",model=fit1)
# check2<-lmbootstrapcheckpreset(fullDF = bySeasonToBuild,response = "KRateNext",model=fit2)
# check3<-lmbootstrapcheckpreset(fullDF = bySeasonToBuild,response = "KRateNext",model=fit3)
# check4<-lmbootstrapcheckpreset(fullDF = bySeasonToBuild,response = "KRateNext",model=fit4)
# check5<-lmbootstrapcheckpreset(fullDF = bySeasonToBuild,response = "KRateNext",model=fit5)
# check6<-lmbootstrapcheckpreset(fullDF = bySeasonToBuild,response = "KRateNext",model=fit6)
# check7<-lmbootstrapcheckpreset(fullDF = bySeasonToBuild,response = "KRateNext",model=fit7)
# check8<-lmbootstrapcheckpreset(fullDF = bySeasonToBuild,response = "KRateNext",model=fit8)
# check9<-lmbootstrapcheckpreset(fullDF = bySeasonToBuild,response = "KRateNext",model=fit9)
# 
# m0<-mean(check0)
# m1<-mean(check1)
# m2<-mean(check2)
# m3<-mean(check3)
# m4<-mean(check4)
# m5<-mean(check5)
# m6<-mean(check6)
# m7<-mean(check7)
# m8<-mean(check8)
# m9<-mean(check9)
# 
# d0<-median(check0)
# d1<-median(check1)
# d2<-median(check2)
# d3<-median(check3)
# d4<-median(check4)
# d5<-median(check5)
# d6<-median(check6)
# d7<-median(check7)
# d8<-median(check8)
# d9<-median(check9)
# 
# toPaste<-c(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,d0,d1,d2,d3,d4,d5,d6,d7,d8,d9)
# 
# writeClipboard(as.character(toPaste))
#Pasting the results into the aforementioned Excel file.

#Fit 9 above is the best model. I'm sure I could work more on this, but that seems like a good baseline for the Random Forest models.
#And one takeaway to try to improve the Random Forest model is that having a term for decay of 0.5 per year could help. 
#Let's redo things one more time.


print.data.frame(head(pitchersBySeason))

bySeasonToBuild<-pitchersBySeason[pitchersBySeason$year<2024,]
#Sample size of 800 for the model build, we have no next season for 2024.
#Obviously, we'll use 2024 for the predict.

colnames(bySeasonToBuild)
bySeasonToUse<-bySeasonToBuild[,c(20,5:7,21,43:56,106:107)]
#Adding in the 0.5 diff terms vs. the in-season K% and versus the even weighting

#I tried some other subsets of the columns, but the Random Forest didn't do as well
#with fewer terms, such as just the terms that popped in the LM, or with too many terms
#and more collinearity.

bySeasonToUse[,-1]<-scale(bySeasonToUse[,-1])
str(bySeasonToUse)
summary(bySeasonToUse)

head(bySeasonToUse)

bySeasonToUse<-as.data.frame(bySeasonToUse)

division <- sample(2, nrow(bySeasonToUse), replace = TRUE, prob = c(0.8, 0.2))
training <- bySeasonToUse[division==1,]
testing <- bySeasonToUse[division==2,]

head(training)

KRateMod <- randomForest(KRateNext~., data=training, proximity=TRUE,ntree=500) 
print(KRateMod)

testing$Prediction<-predict(KRateMod,testing,type="response")

print.data.frame(head(testing))

summary(testing$KRateNext)
summary(testing$Prediction)

imp<-as.data.frame(randomForest::importance(KRateMod0))
imp$Variables<-0
imp[order(-imp$IncNodePurity),]

training<-as.data.frame(training)

#Tuning the mtry term in the model and fitting the optimal model
# tunedModel <- tuneRF(training[,-1], training[,1],
#                      stepFactor = 2,
#                      plot = TRUE,
#                      ntreeTry = 500,
#                      trace = TRUE,
#                      improve = 0.05,
#                      doBest = TRUE)

#The mtry tended to be fine and this just seemed to over-fit.

summary(testing$KRateNext)
summary(testing$Prediction)

with(testing,caret::RMSE(pred=Prediction,obs = KRateNext))

#Checking the 2025 predictions
bySeasonToPredict<-pitchersBySeason[pitchersBySeason$year==2024,c(1:7,21,43:56,106:107)]
summary(bySeasonToPredict)

head(bySeasonToPredict)

bySeasonToPredict$Predicted2025KRate<-predict(KRateMod,scale(bySeasonToPredict),type="response")

summary(bySeasonToPredict$Predicted2025KRate)

print.data.frame(head(bySeasonToPredict))

with(bySeasonToPredict,summary(Predicted2025KRate-KRateRegressed))

#Small sample sizes and just really low numbers
print.data.frame(head(bySeasonToPredict[with(bySeasonToPredict,Predicted2025KRate-KRateRegressed>.05),]))

#Small samples, really high K% marks, and players with declining stuff
print.data.frame(head(bySeasonToPredict[with(bySeasonToPredict,Predicted2025KRate-KRateRegressed<(-.05)),]))

strikeoutPredictions<-bySeasonToPredict[,c("pitcher_id","Predicted2025KRate")]

write.csv(strikeoutPredictions,file="pitcher_strikeout_projections.csv")
