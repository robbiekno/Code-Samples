require(rethinking)
require(caret)
require(xgboost)
require(ggplot2)
require(Ckmeans.1d.dp)
require(dplyr)

#Load in the data
bipDF<-read.csv(file="data_sample.csv",header=T)
str(bipDF)
table(bipDF$year)

summary(bipDF$hit_distance)#this is what we're trying to get a feel for, so we'll have to remove the rows that are NA.

summary(bipDF$hit_vertical_angle)#these will all be populated once the distances are non-null
summary(bipDF$hit_exit_speed)#same

#Some of these events don't look appropriate for this question, let's look into them

table(bipDF$event_result)
print.data.frame(bipDF[bipDF$event_result %in% c("balk","hit_by_pitch","passed_ball","pickoff_caught_stealing_2b","stolen_base_3b","strikeout","walk","wild_pitch"),])
summary(bipDF$hit_exit_speed[bipDF$event_result %in% c("balk","hit_by_pitch","passed_ball","pickoff_caught_stealing_2b","stolen_base_3b","strikeout","walk","wild_pitch")])
summary(bipDF$hit_exit_speed[bipDF$event_result %in% c("balk","hit_by_pitch","passed_ball","pickoff_caught_stealing_2b","stolen_base_3b","walk","wild_pitch")==F])

summary(bipDF$hit_bearing[bipDF$event_result %in% c("balk","hit_by_pitch","passed_ball","pickoff_caught_stealing_2b","stolen_base_3b","strikeout","walk","wild_pitch")])
summary(bipDF$hit_bearing[bipDF$event_result %in% c("balk","hit_by_pitch","passed_ball","pickoff_caught_stealing_2b","stolen_base_3b","strikeout","walk","wild_pitch")==F])

#Yeah, those events should be removed. A lot of NA's, many low exit speeds in line with non-batted ball events, 
#many extreme bearings towards foul territory, and a few clear data errors with distances over 200

summary(bipDF$hit_spinrate)

print.data.frame(bipDF[bipDF$event_result %in% c("sac_bunt"),])
summary(bipDF$hit_exit_speed[bipDF$event_result %in% c("sac_bunt")])
#Sac bunts also don't fit in well here. A lot of NA's for major metrics, very low exit velocities.
#It wouldn't make sense to keep the few bunts that are complete for more of the variables given the high rate that aren't

bipDF<-bipDF[is.na(bipDF$hit_distance)==F,]
bipDF<-bipDF[bipDF$event_result %in% c("balk","hit_by_pitch","passed_ball","pickoff_caught_stealing_2b","stolen_base_3b","strikeout","walk","wild_pitch","sac_bunt")==F,]
str(bipDF)

table(bipDF$event_result)#Definitely a few weird things may have snuck in, but this should be fine to use

summary(bipDF)
#Now things are complete aside from hit_spinrate

summary(bipDF[is.na(bipDF$hit_spinrate),])
summary(bipDF[is.na(bipDF$hit_spinrate)==F,])

#Definitely pretty different groups of batted balls - the ones with no hit spin rate don't go much of anywhere and
#have a very low hit_vertical_angle.
#We should try to get these in. Let's take a sample of the data and use it to build a one-second model to estimate
#hit_spinrate


toFillSpinRate<-bipDF[sample(1:nrow(bipDF),round(nrow(bipDF)*0.5,0),replace=T),]
str(toFillSpinRate)

with(toFillSpinRate,cor(hit_spinrate,hit_vertical_angle,use = "complete.obs"))
with(toFillSpinRate,cor(hit_spinrate,hit_bearing,use = "complete.obs"))
with(toFillSpinRate,cor(hit_spinrate,hit_bearing^2,use = "complete.obs"))

#Let's just build a one-second model for hit_spinrate, because it will play into things
SpinRateFit<-lm(hit_spinrate~hit_vertical_angle+hit_bearing+I(hit_bearing^2),data=toFillSpinRate)
summary(SpinRateFit)

bipDF$hit_spinrate_filled<-predict(SpinRateFit,bipDF)
summary(bipDF$hit_spinrate_filled)
summary(bipDF$hit_spinrate)
#The filled one ismuch lower incorporating these lower vertical angles and more extreme bearings

bipDF$hit_spinrate<-with(bipDF,ifelse(is.na(hit_spinrate),hit_spinrate_filled,hit_spinrate))
summary(bipDF$hit_spinrate)#Using the actual ones where we can

str(bipDF)
with(bipDF,cor(release_speed,plate_speed))
#These are hugely correlated, r=0.9687765
#We know that a higher effective velocity (indicating higher extension) is correlated with having 
#a better fastball movement grade, if we don't have all the other Trackman/Hawkeye variables, so a difference term
#could be useful. plate_speed is more what the batter is impacting, but the difference between plate and release could
#have some value.
#No point having them both in there as is given the high correlation, meaning that the incremental value of release_speed
#is very low and it may mess with variable importance calculations.

with(bipDF,summary(release_speed-plate_speed))
#Around 7 MPH is what I recall being the average for this difference, with lower being better.

#Let's create a difference metric which is plate_speed-release_speed + 7 as some proxy for pitch movement quality

bipDF$veloDifferenceFromRelease<-with(bipDF,(plate_speed-release_speed+7))
summary(bipDF$veloDifferenceFromRelease)

head(bipDF)

str(bipDF)

#That leads me to my last major question before really beginning this analysis in earnest:
#We think of the potentially juiced baseballs in terms of making flyballs go farther. Would the change in drag
#be much of a factor at all on groundballs or very high flyballs? If we include those variables in our analysis,
#are we just adding in a bunch of noise?
#And maybe not all groundballs are created equal - ones with launch angles pretty close to zero are still in the air
#for a while while ones much lower than that almost go straight into the ground

#Let's take a sample of the data and try to measure this in a way that is markedly different from the year effect
#that we're looking for.
#Let's take a sample of the data and then build a basic Bayesian model for distance using vertical angle and exit speed.
#Then we can have intercepts for whether it's the summer months (June, July, August) and different buckets of 
#vertical angle

gbTestDF<-bipDF[sample(1:nrow(bipDF),round(nrow(bipDF)*0.5,0),replace=T),]
str(gbTestDF)

table(round(gbTestDF$hit_vertical_angle,-1))

#Let's create vertical angle groups from -30 to 60
colnames(gbTestDF)

gbTestDF$VerticalAngleGroupTemp<-with(gbTestDF,round(hit_vertical_angle,-1))
gbTestDF$VerticalAngleGroupTemp<-with(gbTestDF,ifelse(VerticalAngleGroupTemp<=(-20),-20,
                                                  ifelse(VerticalAngleGroupTemp>=50,50,VerticalAngleGroupTemp)))
table(gbTestDF$VerticalAngleGroupTemp)
gbTestDF$VerticalAngleGroupTemp<-as.factor(gbTestDF$VerticalAngleGroupTemp)

levels<-levels(gbTestDF$VerticalAngleGroupTemp)
which(levels=="-20")

gbTestDF$VerticalAngleGroup<-sapply(1:nrow(gbTestDF), function(x){which(levels==gbTestDF$VerticalAngleGroupTemp[x])})
table(gbTestDF$VerticalAngleGroup)

gbTestDF$Summer<-with(gbTestDF,ifelse(month %in% c(6,7,8),2,1))
table(gbTestDF$Summer)

gbTestDF$GroupsToTest<-with(gbTestDF,ifelse(Summer==1,VerticalAngleGroup,VerticalAngleGroup+8))
table(gbTestDF$GroupsToTest)

str(gbTestDF)

colnames(gbTestDF)
forBayesian<-with(gbTestDF,list(hit_exit_speed = as.vector(scale(hit_exit_speed)),
                                hit_vertical_angle = as.vector(scale(hit_vertical_angle)),
                                GroupsToTest = GroupsToTest,
                                hit_distance = as.vector(scale(hit_distance))))
str(forBayesian)

#Predicting hit distance from hit_exit_speed, hit_vertical_angle, hit_vertical_angle^2, and
#an interaction term between hit_exit_speed and hit_vertical_angle
baselineMod <- stan(file = 'Baseline Distance Mod.stan', data = forBayesian,chains=4,cores=4)
precis(baselineMod,depth=2)

forBayesian$Distance_Pred<-with(forBayesian,0.15+0.24*hit_exit_speed+0.90*hit_vertical_angle-
                                0.19*(hit_vertical_angle^2)+0.38*hit_exit_speed*hit_vertical_angle)
summary(forBayesian$Distance_Pred)
summary(forBayesian$hit_distance)
#The upper half of distance seems good, the bottom half less so, for what that's worth

#Seeing the impact of summer on top of the previous model, dividing into groups of 
#10 degrees of vertical angle with grouping at the ends, see VerticalAngleGroup above
angleMod <- stan(file = 'Vertical Angle Check Model.stan', data = forBayesian,chains=4,cores=4)
precis(angleMod,depth=2)

#Looking at the outputs of this model, there doesn't appear to be any summer difference
#for batted balls at a launch angle of zero or less.
#Conceptually, I would have thought that the impact of something like drag or heat would be
#all about flyball leaving the yard, and that appears to be the case. We need to focus
#our analysis just on balls in the air, let's see 0 degrees or higher

analysisDF<-bipDF[bipDF$hit_vertical_angle>=0,]
str(analysisDF)#3139 rows
table(analysisDF$year)#a bit over 600 per year

#Making month, throws, bat_side, and pitch_type into factors
colnames(analysisDF)
for(i in 2:5){
  analysisDF[,i]<-as.factor(analysisDF[,i])
}
str(analysisDF)

#Year, though, shouldn't be a factor because there may have been differences within the year
table(analysisDF$month)

analysisDF$YearContinuous<-with(analysisDF,year+(as.numeric(as.character(month))-2)/10)
table(analysisDF$YearContinuous)

str(analysisDF)

#Now let's get ready to build an XGBoost model to really answer the question.
colnames(analysisDF)

split<-createDataPartition(analysisDF$hit_distance, p = 0.8, list = FALSE)

#Creating training and testing sets, 
#removing year and month (we made the continuous year),
#hit_spinrate_filled (we filled it into the main variable),
#release_speed (we have plate_speed and the difference variable),
#and event_result in addition to the response
trainingGB = analysisDF[split,-c(1:2,6,12:14)]
testingGB = analysisDF[-split,-c(1:2,6,12:14)]

str(trainingGB)
#Scaling the numeric columns
for(i in 1:ncol(trainingGB)){
  if(is.numeric(trainingGB[,i])){
    trainingGB[,i]<-scale(trainingGB[,i])
  }
}
str(trainingGB)

summary(trainingGB)

colnames(testingGB)

#Scaling the testing according to the training
for(i in 1:ncol(testingGB)){
  if(is.numeric(testingGB[,i])){
    testingGB[,i]<-(testingGB[,i]-attributes(trainingGB[,i])[[2]])/attributes(trainingGB[,i])[[3]]
  }
}

trainingResponse<-scale(analysisDF$hit_distance[split])
testingResponse<-analysisDF$hit_distance[-split]
testingResponse<-(testingResponse-attributes(trainingResponse)[[2]])/attributes(trainingResponse)[[3]]

trainingResponse<-as.numeric(trainingResponse)

#Building the initial XGBoost model
boostingMod0 <- xgboost::xgboost(data = data.matrix(trainingGB),
                                 label = trainingResponse,
                                 booster = "gbtree",
                                 objective = "reg:squarederror",
                                 nrounds = 100,
                                 verbose = 0)
summary(boostingMod0)

#Constructing the tuning grid. I'm using the simpler version of XG Boost due to time.
#
tuning_grid<-expand.grid(nrounds = c(500,1000,2500,5000),
                         eta = c(0.01, 0.025, 0.05),
                         lambda = 1,
                         alpha = c(0,0.5,1))

#And now the tune options. I'm doing 5 folds rather than more just due to time.
tune_options <- trainControl(
  method = "cv", 
  number = 5,
  verboseIter = FALSE,
  allowParallel = FALSE,
  search = "random"
)

# save(optimal_params,file="Optimal Parameters for Drag Coefficient Analysis.RDA")
#Doing the train across the grid of parameters
optimal_params = train(x = data.matrix(trainingGB),
                    y = trainingResponse,
                    trControl = tune_options,
                    tuneGrid = tuning_grid,
                    method = "xgbLinear")

optimal_params$bestTune

#Training the model using the optimal parameters
boostingModFinal <- xgboost::xgboost(data = data.matrix(trainingGB),
                                 label = trainingResponse,
                                 booster = "gbtree",
                                 objective = "reg:squarederror",
                                 nrounds = optimal_params$bestTune$nrounds,
                                 eta=optimal_params$bestTune$eta,
                                 alpha=optimal_params$bestTune$alpha,
                                 lambda=optimal_params$bestTune$lambda,
                                 verbose = 0)

colnames(trainingGB)

#Getting variable importance for the model
importanceForBoosting = xgb.importance(feature_names = colnames(trainingGB), model = boostingModFinal)
head(importanceForBoosting)

#Now plotting the top of it.
#Interesting what's at the top, particularly that overall usage wasn't too helpful.
importanceBoostingPlot = xgb.ggplot.importance(importanceForBoosting)
print(importanceBoostingPlot) 
#No surprise, vertical angle and exit speed are the most important, interesting that
#hit_spinrate is so up there as well.

#Now let's see what the model is saying about the effect of year on batted ball distance
#Let's look at 95, 100, and 105 MPH BIP at launch angles ranging from 0 to 50 
#and see what happens based on year
#I'll have to fill in means for everything else due to time

#Getting scaling ready to use in a second
forScaling = analysisDF[split,-c(1:2,6,13:14)]

str(forScaling)
#Scaling the numeric columns
for(i in 1:ncol(forScaling)){
  if(is.numeric(forScaling[,i])){
    forScaling[,i]<-scale(forScaling[,i])
  }
}

str(forScaling)

anglesToUse<-seq(from=0,to=50,by=5)

speedsToUse<-seq(from=95,to=105,by=5)

colnames(trainingGB)
predictionTest<-with(trainingGB,expand.grid(pitcher_throws = "R",
                            bat_side = "R",
                            pitch_type = "FF",
                            plate_speed = mean(plate_speed),
                            hit_exit_speed = (speedsToUse-attributes(forScaling$hit_exit_speed)[[2]])/attributes(forScaling$hit_exit_speed)[[3]],
                            hit_spinrate = mean(hit_spinrate),
                            hit_vertical_angle = (anglesToUse-attributes(forScaling$hit_vertical_angle)[[2]])/attributes(forScaling$hit_vertical_angle)[[3]],
                            hit_bearing = mean(hit_bearing),
                            veloDifferenceFromRelease = mean(veloDifferenceFromRelease),
                            YearContinuous = unique(trainingGB$YearContinuous)
                            )
                    )

predictionTest$PredictedDistance<-predict(boostingModFinal,data.matrix(predictionTest),type="response")*attributes(forScaling$hit_distance)[[3]]+attributes(forScaling$hit_distance)[[2]]

head(predictionTest)

predictionTest$vertical_angleUnscaled<-predictionTest$hit_vertical_angle*attributes(forScaling$hit_vertical_angle)[[3]]+attributes(forScaling$hit_vertical_angle)[[2]]
predictionTest$Year<-predictionTest$YearContinuous*attributes(forScaling$YearContinuous)[[3]]+attributes(forScaling$YearContinuous)[[2]]
# predictionTest$exit_speedUnscaled<-rep(speedsToUse)
predictionTest$exit_speedUnscaled<-predictionTest$hit_exit_speed*attributes(forScaling$hit_exit_speed)[[3]]+attributes(forScaling$hit_exit_speed)[[2]]

#Definitely some noise and I'm running out of time, but a few of the plots tell a coherent
#story.

with(predictionTest[predictionTest$vertical_angleUnscaled==30&predictionTest$exit_speedUnscaled==95,],plot(Year,PredictedDistance,main="Distance by Year at 30 degrees and 95 MPH"))
with(predictionTest[predictionTest$vertical_angleUnscaled==30&predictionTest$exit_speedUnscaled==100,],plot(Year,PredictedDistance,main="Distance by Year at 30 degrees and 100 MPH"))
with(predictionTest[predictionTest$vertical_angleUnscaled==30&predictionTest$exit_speedUnscaled==105,],plot(Year,PredictedDistance,main="Distance by Year at 30 degrees and 105 MPH"))


#Overall, it looks like the balls increased from their prior level in 2016, remained high
#for all 2017, started going down in 2018, and remained low through 2019.

with(predictionTest[predictionTest$vertical_angleUnscaled==30&predictionTest$exit_speedUnscaled==95,],plot(Year,PredictedDistance,main="30 degrees, 95 MPH"))
with(predictionTest[predictionTest$vertical_angleUnscaled==30&predictionTest$exit_speedUnscaled==100,],plot(Year,PredictedDistance,main="30 degrees, 100 MPH"))

forTesting<-trainingGB[between(trainingGB$hit_vertical_angle*attributes(forScaling$hit_vertical_angle)[[3]]+attributes(forScaling$hit_vertical_angle)[[2]],10,30)&
                        trainingGB$hit_exit_speed*attributes(forScaling$hit_exit_speed)[[3]]+attributes(forScaling$hit_exit_speed)[[2]]>=95,]
forTesting$PredictedDistance<-predict(boostingModFinal,data.matrix(forTesting),type="response")*attributes(forScaling$hit_distance)[[3]]+attributes(forScaling$hit_distance)[[2]]
summary(forTesting$PredictedDistance)#332.6 for all

forTesting<-trainingGB[between(trainingGB$hit_vertical_angle*attributes(forScaling$hit_vertical_angle)[[3]]+attributes(forScaling$hit_vertical_angle)[[2]],10,30)&
                        trainingGB$hit_exit_speed*attributes(forScaling$hit_exit_speed)[[3]]+attributes(forScaling$hit_exit_speed)[[2]]>=95,]
forTesting$YearContinuous<-2019.2*attributes(forScaling$YearContinuous)[[3]]+attributes(forScaling$YearContinuous)[[2]]
forTesting$PredictedDistance<-predict(boostingModFinal,data.matrix(forTesting),type="response")*attributes(forScaling$hit_distance)[[3]]+attributes(forScaling$hit_distance)[[2]]
summary(forTesting$PredictedDistance)#331.9 for 2019

forTesting<-trainingGB[between(trainingGB$hit_vertical_angle*attributes(forScaling$hit_vertical_angle)[[3]]+attributes(forScaling$hit_vertical_angle)[[2]],10,30)&
                         trainingGB$hit_exit_speed*attributes(forScaling$hit_exit_speed)[[3]]+attributes(forScaling$hit_exit_speed)[[2]]>=95,]
forTesting$YearContinuous<-2016.2*attributes(forScaling$YearContinuous)[[3]]+attributes(forScaling$YearContinuous)[[2]]
forTesting$PredictedDistance<-predict(boostingModFinal,data.matrix(forTesting),type="response")*attributes(forScaling$hit_distance)[[3]]+attributes(forScaling$hit_distance)[[2]]
summary(forTesting$PredictedDistance)#331.9 for 2019

head(forTesting$YearContinuous)

forTesting<-testingGB[between(testingGB$hit_vertical_angle*attributes(forScaling$hit_vertical_angle)[[3]]+attributes(forScaling$hit_vertical_angle)[[2]],10,30)&
                        testingGB$hit_exit_speed*attributes(forScaling$hit_exit_speed)[[3]]+attributes(forScaling$hit_exit_speed)[[2]]>=95,]
forTesting$YearContinuous<-2018.7
forTesting$PredictedDistance<-predict(boostingModFinal,data.matrix(forTesting),type="response")*attributes(forScaling$hit_distance)[[3]]+attributes(forScaling$hit_distance)[[2]]
summary(forTesting$PredictedDistance)#334.3 for all

forTesting<-testingGB
forTesting$YearContinuous<-2018.2
forTesting$PredictedDistance<-predict(boostingModFinal,data.matrix(forTesting),type="response")*attributes(forScaling$hit_distance)[[3]]+attributes(forScaling$hit_distance)[[2]]
summary(forTesting$PredictedDistance)#251.78 for 2015


forTesting$YearContinuous<-forTesting$YearContinuous+1
forTesting$PredictedDistance<-predict(boostingModFinal,data.matrix(forTesting[,-ncol(forTesting)]),type="response")*attributes(forScaling$hit_distance)[[3]]+attributes(forScaling$hit_distance)[[2]]
summary(forTesting$PredictedDistance)#251.67 for 2016
