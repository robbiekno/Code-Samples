require(caret)
require(xgboost)
require(rstan)
require(rethinking)
require(dplyr)

#Bringing in the data
buildDF<-read.csv(file="PredictiveModelingAssessmentData.csv",header=T)
head(buildDF)

#Doing a quick train-test split, starting with 0.5 since the test data at the end
#is equal in size to the build data we have
split<-createDataPartition(buildDF$y, p = 0.5, list = FALSE)
training = buildDF[split,]
testing = buildDF[-split,]

#Starting with a linear model just to get a baseline. I didn't look at the model summaries
#because I don't want to bias any later analysis
linearMod0<-lm(y~x1+x2,data=training)
testing$Prediction0<-predict(linearMod0,testing,type="response")
testing$Error0<-with(testing,y-Prediction0)
RMSE0<-sqrt(mean(testing$Error0^2))#1.398572...still a lot more work to do
linearMod1<-lm(y~x1*x2,data=training)
testing$Prediction1<-predict(linearMod1,testing,type="response")
testing$Error1<-with(testing,y-Prediction1)
RMSE1<-sqrt(mean(testing$Error1^2))#1.39853, interaction didn't help at all

#I know nothing about the data, so let's take a small sample and give it a glance to see
#what's going on without biasing ourselves.
#We have 4000 rows in the training dataframe - let's take 1% of it, or 40, use it to explore the
#data a bit, and then we'll build the model without it.

toExplore<-sample(1:nrow(training),40,replace=F)
exploration<-training[toExplore,]
summary(exploration)

#Once I just scrolled through the CSV, I assumed that everything was standardized.
#Now I'm not so sure. x1 looks pretty standardized around 0, though potentially with a bit of skew
#While y has a mean around 1 and x2 is closer to 1.5 with no values below 0.
#Let's see the densities

with(exploration,dens(x1))#Looks right-skewed, though won't read too much into that
with(exploration,dens(x2))#All above 0, also looks a bit skewed
with(exploration,dens(y))#Wide range of values, potentially a bit left-skewed but within range of
#normal

#Preparing to build some Bayesian models
newTraining<-training[-toExplore,]
str(newTraining)

#Scaling things for the Bayesian model
forBayesian<-list(y = scale(newTraining$y),x1 = scale(newTraining$x1),
                  x2 = scale(newTraining$x2))

#Ulam is a wrapping for Hamiltonian Monte Carlo in Stan. Honestly, I'd be fine just using rStan for
#such a simple model, but ulam is much better for predicting from the model.
bayesianMod0 <- ulam(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a + b*x1 + c*x2 ,
    a ~ dnorm( 0 , 5 ) ,
    b ~ dnorm( 0 , 1 ) ,
    c ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( 1 )
  ) , data=forBayesian , chains=4, cores=4, log_lik = TRUE )
precis(bayesianMod0)
#Pretty interesting that a = 0, meaning that y is completely average when the values of x1 and x2
#are zero

#Let's see what things look like when we plot 500 points
plotSample<-sample(1:nrow(newTraining),500,replace=T)
toPlot<-list(y = forBayesian$y[plotSample],
             x1 = forBayesian$x1[plotSample],
             x2 = forBayesian$x2[plotSample])

sim.y <- sim( bayesianMod0 , data=toPlot )#simulating from the posterior for each row, one of the functions
#that works much better with ulam than Stan
y.mean<-apply( sim.y , 2 , mean )#mean of sims
str(sim.y)
y.PI <- apply( sim.y , 2 , PI , prob=0.95 )#95% percentile interval
head(y.PI)

attributes(forBayesian$y)

plot( x=toPlot$x2,y=toPlot$y ,
      pch=16 )
points( toPlot$x2 , y.mean )
for ( i in 1:length(plotSample) ) lines( c(toPlot$x2[i],toPlot$x2[i]) , y.PI[,i] )
#It looks pretty good. Biggest quibble would be the lowest values being a bit too high

plot( x=toPlot$x1,y=toPlot$y ,
      pch=16 )
points( toPlot$x1 , y.mean )
for ( i in 1:length(plotSample) ) lines( c(toPlot$x1[i],toPlot$x1[i]) , y.PI[,i] )
#The values of y are highest in the middle of x1, but it looks like there's a quadratic effect
#Let's give that a shot.

newTraining<-training[-toExplore,]
str(newTraining)

forBayesian<-list(y = scale(newTraining$y),x1 = scale(newTraining$x1),
                  x2 = scale(newTraining$x2))
forBayesian$x1_sq<-forBayesian$x1^2
#Adding in a squared term for x1

bayesianMod1 <- ulam(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a + b*x1 + c*x2 + d*x1_sq,
    a ~ dnorm( 0 , 5 ) ,
    b ~ dnorm( 0 , 1 ) ,
    c ~ dnorm( 0 , 1 ) ,
    d ~ dnorm( 0,  1 ) ,
    sigma ~ dexp( 1 )
  ) , data=forBayesian , chains=4, cores=4, log_lik = TRUE )
precis(bayesianMod1)
precis(bayesianMod0)

# trankplot(bayesianMod1)#generally looks fine

plotSample<-sample(1:nrow(newTraining),500,replace=T)
toPlot<-list(y = forBayesian$y[plotSample],
             x1 = forBayesian$x1[plotSample],
             x2 = forBayesian$x2[plotSample],
             x1_sq = forBayesian$x1_sq[plotSample])

sim.y0 <- sim( bayesianMod0 , data=toPlot )
y.mean0<-apply( sim.y0 , 2 , mean )
# str(sim.y)
# y.PI <- apply( sim.y , 2 , PI , prob=0.95 )
# head(y.PI)

sim.y1 <- sim( bayesianMod1 , data=toPlot )
y.mean1<-apply( sim.y1 , 2 , mean )


summary(toPlot$x2)

par(mfrow = c(2, 2))

#Now drawing plots comparing the new model and the old model
plot( x=toPlot$x2,y=toPlot$y ,
      pch=16, main="Model 1 vs. Actual")
points( toPlot$x2 , y.mean0,col="red" )

plot( x=toPlot$x2,y=toPlot$y ,
      pch=16, main="Model 2 vs. Actual" )
points( toPlot$x2 , y.mean1,col="blue" )
# for ( i in 1:nrow(toPlot) ) lines( c(toPlot$x2[i],toPlot$x2[i]) , y.PI[,i] )

plot( x=toPlot$x1,y=toPlot$y ,
      pch=16, main="Model 1 vs. Actual" )
points( toPlot$x1 , y.mean0,col="red" )

plot( x=toPlot$x1,y=toPlot$y ,
      pch=16, main="Model 2 vs. Actual" )
points( toPlot$x1 , y.mean1,col="blue" )
# for ( i in 1:nrow(toPlot) ) lines( c(toPlot$x1[i],toPlot$x1[i]) , y.PI[,i] )

par(mfrow = c(1,1))

compare(bayesianMod0,bayesianMod1,func=PSIS)
#Mod1 is superior to a major degree according to PSIS, which is an efficient cross-validation

#All of this said, it didn't seem like the Bayesian models were ever going to fully capture the
#variation in the data, and knowing nothing about the variables, there's no value to the additional
#conceptual understanding I may have from a Bayesian model.
#Especially with a good sample size, it made sense to look into a more black box model, and gradient boosting
#is the one I prefer.

#Dividing into Training and Testing
trainingGB = data.frame(scale(newTraining[,-1]))
testingGB = testing[,c(2,3)]

#Scaling the testing set using the scaling from the training (as calculated above)
testingGB$x1<-with(testingGB,(x1-attributes(forBayesian$x1)[[2]])/attributes(forBayesian$x1)[[3]])
testingGB$x2<-with(testingGB,(x2-attributes(forBayesian$x2)[[2]])/attributes(forBayesian$x2)[[3]])

trainingResponse<-as.numeric(scale(newTraining$y))
testingResponse<-testing$y
#Scaling the testing response based on the training response
testingResponse<-(testingResponse-attributes(forBayesian$y)[[2]])/attributes(forBayesian$y)[[3]]

summary(trainingResponse)
summary(testingResponse)

#Building the first version of the XGBoost model
boostingMod0 <- xgboost::xgboost(data = data.matrix(trainingGB),
                                 label = trainingResponse,
                                 booster = "gbtree",
                                 objective = "reg:squarederror",
                                 nrounds = 100,
                                 verbose = 0)
summary(boostingMod0)

predictions<-predict(boostingMod0,data.matrix(testingGB),type="response")
predictionsUnscaled<-predictions*attributes(forBayesian$y)[[3]]+attributes(forBayesian$y)[[2]]

testingGB$ErrorG0<-predictionsUnscaled-testing$y
RMSEG0<-sqrt(mean(testingGB$ErrorG0^2))
#A bit better than the Bayesian models I looked at, but let's see how good it gets with tuning

#Constructinga  tuning grid with plausible values for each parameter
tuning_grid <- expand.grid(
  nrounds = c(500,1000,2000),
  eta = c(0.025, 0.05, 0.1),
  max_depth = c(2,4,6),
  gamma = c(0, 1, 2),
  colsample_bytree = c(1.0),#there are only 2 variables, so can't do much with this, would try 0.5 and 0.75 as well
  min_child_weight = c(1, 3, 5),
  subsample = c(0.5,0.75,1)
)

#Starting with 2-fold cross-validation given that I'm early in this process and want to make sure
#everything works right
tune_options <- trainControl(
  method = "cv", 
  number = 2,
  verboseIter = FALSE,
  allowParallel = FALSE,
  search = "random"
)


Sys.time()#doing this to check runtime, which wasn't so bad (40 minutes)
optimal_params <- train(
  x = data.matrix(trainingGB),
  y = trainingResponse,
  trControl = tune_options,
  tuneGrid = tuning_grid,
  method = "xgbTree",
  verbose = FALSE,
  verbosity = 0
)
Sys.time()

optimal_params$bestTune

summary(optimal_params)

#Building the model with the first round fo tuning
boostingMod1 <- xgboost::xgboost(data = data.matrix(trainingGB),
                                 label = trainingResponse,
                                 booster = "gbtree",
                                 objective = "reg:squarederror",
                                 nrounds = optimal_params$bestTune$nrounds,
                                 max_depth=optimal_params$bestTune$max_depth,
                                 colsample_bytree=optimal_params$bestTune$colsample_bytree,
                                 min_child_weight=optimal_params$bestTune$min_child_weight,
                                 subsample=optimal_params$bestTune$subsample,
                                 eta=optimal_params$bestTune$eta,
                                 gamma=optimal_params$bestTune$gamma,
                                 verbose = 0)

predictionsNew<-predict(boostingMod1,data.matrix(testingGB),type="response")
predictionsNewUnscaled<-predictionsNew*attributes(forBayesian$y)[[3]]+attributes(forBayesian$y)[[2]]

testingGB$ErrorG1<-predictionsNewUnscaled-testing$y
RMSEG1<-sqrt(mean(testingGB$ErrorG1^2))#Pretty darn close to 1
RMSEG0

summary(predictionsUnscaled)
summary(predictionsNewUnscaled)
summary(testing$y)

importance_matrixFinal <- xgb.importance(colnames(trainingGB), model = boostingMod1)
xgb.plot.importance(importance_matrixFinal, rel_to_first = TRUE, xlab = "Relative importance")
#x2 is over double as importance as x1, for the limited amount that's worth

#Seeing how things look plotting on a random sample of the training
plotSample<-sample(1:nrow(newTraining),500,replace=T)
toPlot<-list(y = forBayesian$y[plotSample],
             x1 = forBayesian$x1[plotSample],
             x2 = forBayesian$x2[plotSample],
             x1_sq = forBayesian$x1_sq[plotSample],
             x2_sq = forBayesian$x2_sq[plotSample],
             x2_cu = forBayesian$x2_cu[plotSample])


y.meanGB1<-predict(boostingMod1,data.matrix(trainingGB[plotSample,]),type="response")

# par(mfrow = c(1, 2))

plot( x=toPlot$x2,y=toPlot$y ,
      pch=16, main="GB Mod1 vs. Actual" )
points( toPlot$x2 , y.meanGB1,col="blue" )
# for ( i in 1:nrow(toPlot) ) lines( c(toPlot$x2[i],toPlot$x2[i]) , y.PI[,i] )

plot( x=toPlot$x1,y=toPlot$y ,
      pch=16, main="GB Mod1 vs. Actual" )
points( toPlot$x1 , y.meanGB1,col="blue" )

#Now an out of Sample Check
plotSample<-sample(1:nrow(testing),500,replace=T)
toPlot<-list(y = testingResponse[plotSample],
             x1 = testingBayesian$x1[plotSample],
             x2 = testingBayesian$x2[plotSample],
             x1_sq = testingBayesian$x1_sq[plotSample],
             x2_sq = testingBayesian$x2_sq[plotSample],
             x2_cu = testingBayesian$x2_cu[plotSample])

y.meanGB1<-predict(boostingMod1,data.matrix(testingGB[plotSample,1:2]),type="response")


plot( x=toPlot$x2,y=toPlot$y ,
      pch=16, main="GB Mod1 vs. Actual" )
points( toPlot$x2 , y.meanGB1,col="blue" )

plot( x=toPlot$x1,y=toPlot$y ,
      pch=16, main="GB Mod1 vs. Actual" )
points( toPlot$x1 , y.meanGB1,col="blue" )

#Playing around with parameter tuning one more time given the chosen values.

tuning_gridUpdate <- expand.grid(
  nrounds = c(500,1000,2000),
  eta = c(0.001, 0.010, 0.025),#Picked the lowest value, worth trying a few lower
  max_depth = c(2,4,6),
  gamma = c(0, 1, 2),
  colsample_bytree = c(1.0),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.25,0.5,0.75)#Can try subsampling a bit more
)

Sys.time()
optimal_paramsV2 <- train(
  x = data.matrix(trainingGB),
  y = trainingResponse,
  trControl = tune_options,
  tuneGrid = tuning_gridUpdate,
  method = "xgbTree",
  verbose = FALSE,
  verbosity = 0
)
Sys.time()

optimal_paramsV2$bestTune
optimal_params$bestTune

summary(optimal_paramsV2)

#Built a model based on the optimal values from this run
boostingMod2 <- xgboost::xgboost(data = data.matrix(trainingGB),
                                 label = trainingResponse,
                                 booster = "gbtree",
                                 objective = "reg:squarederror",
                                 nrounds = optimal_paramsV2$bestTune$nrounds,
                                 max_depth=optimal_paramsV2$bestTune$max_depth,
                                 colsample_bytree=optimal_paramsV2$bestTune$colsample_bytree,
                                 min_child_weight=optimal_paramsV2$bestTune$min_child_weight,
                                 subsample=optimal_paramsV2$bestTune$subsample,
                                 eta=optimal_paramsV2$bestTune$eta,
                                 gamma=optimal_paramsV2$bestTune$gamma,
                                 verbose = 0)

predictionsNewV2<-predict(boostingMod2,data.matrix(testingGB[,1:2]),type="response")
predictionsNewV2Unscaled<-predictionsNewV2*attributes(forBayesian$y)[[3]]+attributes(forBayesian$y)[[2]]

testingGB$ErrorG2<-predictionsNewV2Unscaled-testing$y
RMSEG2<-sqrt(mean(testingGB$ErrorG2^2))#Slightly closer to 1, not a huge deal
RMSEG1
RMSEG0

#Final Comparison:
#Take off 20% of the data (1000 rows)
#Among the other 40%, divide into half train and half test
#Then tune the XGBoost with 5-fold cross-validation

head(buildDF)
buildDF$rand<-rnorm(5000,0,5)
buildDF<-buildDF[order(buildDF$rand),]

lastValidation<-buildDF[1:1000,]
lastBuild<-buildDF[1001:3000,]
lastTest<-buildDF[3001:5000,]
nrow(lastBuild)
nrow(lastTest)

#Just using this to scale the data at this point
forBayesianFinal<-list(y = scale(lastBuild$y),x1 = scale(lastBuild$x1),
                       x2 = scale(lastBuild$x2))

#Dividing into Training and Testing
trainingGBFinal = data.frame(scale(lastBuild[,-c(1,4)]))
testingGBFinal = lastTest[,c(2,3)]

#Rescaling testing based on training
testingGBFinal$x1<-with(testingGBFinal,(x1-attributes(forBayesianFinal$x1)[[2]])/attributes(forBayesianFinal$x1)[[3]])
testingGBFinal$x2<-with(testingGBFinal,(x2-attributes(forBayesianFinal$x2)[[2]])/attributes(forBayesianFinal$x2)[[3]])

trainingResponseFinal<-as.numeric(scale(lastBuild$y))
testingResponseFinal<-lastTest$y
testingResponseFinal<-(testingResponseFinal-attributes(forBayesianFinal$y)[[2]])/attributes(forBayesianFinal$y)[[3]]

summary(trainingResponseFinal)
summary(testingResponseFinal)

#Continuing to try to pick more intelligent values based on the first two tunign runs
tuning_grid <- expand.grid(
  nrounds = c(500,1000,2000),
  eta = c(0.01, 0.025, 0.05),
  max_depth = c(2,4,6),
  gamma = c(0, 1, 2),
  colsample_bytree = c(1.0),#there are only 2 variables, so can't do much with this, would try 0.5 and 0.75 as well
  min_child_weight = c(1, 3, 5),
  subsample = c(0.25,0.5,0.75)
)

#5-fold this time
tune_options <- trainControl(
  method = "cv", 
  number = 5,
  verboseIter = FALSE,
  allowParallel = TRUE,
  search = "random"
)

Sys.time()
optimal_paramsV3 <- train(
  x = data.matrix(trainingGBFinal),
  y = trainingResponseFinal,
  trControl = tune_options,
  tuneGrid = tuning_grid,
  method = "xgbTree",
  verbose = FALSE,
  verbosity = 0
)
Sys.time()
#3 hours and change

optimal_paramsV3$bestTune
optimal_paramsV2$bestTune
optimal_params$bestTune

#Last steps: fit models with all 3 versions of optimal parameters
#check them and the Bayesian model for the testing set and validation set
#Fit whichever model is best and call it a day

boostingMod1Final <- xgboost::xgboost(data = data.matrix(trainingGBFinal),
                                      label = trainingResponseFinal,
                                      booster = "gbtree",
                                      objective = "reg:squarederror",
                                      nrounds = optimal_params$bestTune$nrounds,
                                      max_depth=optimal_params$bestTune$max_depth,
                                      colsample_bytree=optimal_params$bestTune$colsample_bytree,
                                      min_child_weight=optimal_params$bestTune$min_child_weight,
                                      subsample=optimal_params$bestTune$subsample,
                                      eta=optimal_params$bestTune$eta,
                                      gamma=optimal_params$bestTune$gamma,
                                      verbose = 0)

boostingMod2Final <- xgboost::xgboost(data = data.matrix(trainingGBFinal),
                                      label = trainingResponseFinal,
                                      booster = "gbtree",
                                      objective = "reg:squarederror",
                                      nrounds = optimal_paramsV2$bestTune$nrounds,
                                      max_depth=optimal_paramsV2$bestTune$max_depth,
                                      colsample_bytree=optimal_paramsV2$bestTune$colsample_bytree,
                                      min_child_weight=optimal_paramsV2$bestTune$min_child_weight,
                                      subsample=optimal_paramsV2$bestTune$subsample,
                                      eta=optimal_paramsV2$bestTune$eta,
                                      gamma=optimal_paramsV2$bestTune$gamma,
                                      verbose = 0)

boostingMod3Final <- xgboost::xgboost(data = data.matrix(trainingGBFinal),
                                      label = trainingResponseFinal,
                                      booster = "gbtree",
                                      objective = "reg:squarederror",
                                      nrounds = optimal_paramsV3$bestTune$nrounds,
                                      max_depth=optimal_paramsV3$bestTune$max_depth,
                                      colsample_bytree=optimal_paramsV3$bestTune$colsample_bytree,
                                      min_child_weight=optimal_paramsV3$bestTune$min_child_weight,
                                      subsample=optimal_paramsV3$bestTune$subsample,
                                      eta=optimal_paramsV3$bestTune$eta,
                                      gamma=optimal_paramsV3$bestTune$gamma,
                                      verbose = 0)

predictionsFinalV1<-predict(boostingMod1Final,data.matrix(testingGBFinal[,1:2]),type="response")
predictionsFinalV1Unscaled<-predictionsFinalV1*attributes(forBayesianFinal$y)[[3]]+attributes(forBayesianFinal$y)[[2]]

testingGBFinal$ErrorG1<-predictionsFinalV1Unscaled-lastTest$y
RMSEG1Final<-sqrt(mean(testingGBFinal$ErrorG1^2))#1.041828

predictionsFinalV2<-predict(boostingMod2Final,data.matrix(testingGBFinal[,1:2]),type="response")
predictionsFinalV2Unscaled<-predictionsFinalV2*attributes(forBayesianFinal$y)[[3]]+attributes(forBayesianFinal$y)[[2]]

testingGBFinal$ErrorG2<-predictionsFinalV2Unscaled-lastTest$y
RMSEG2Final<-sqrt(mean(testingGBFinal$ErrorG2^2))#1.039234

predictionsFinalV3<-predict(boostingMod3Final,data.matrix(testingGBFinal[,1:2]),type="response")
predictionsFinalV3Unscaled<-predictionsFinalV3*attributes(forBayesianFinal$y)[[3]]+attributes(forBayesianFinal$y)[[2]]

testingGBFinal$ErrorG3<-predictionsFinalV3Unscaled-lastTest$y
RMSEG3Final<-sqrt(mean(testingGBFinal$ErrorG3^2))#1.038726

forValidation<-lastValidation[,1:3]
forValidation$x1<-with(forValidation,(x1-attributes(forBayesianFinal$x1)[[2]])/attributes(forBayesianFinal$x1)[[3]])
forValidation$x2<-with(forValidation,(x2-attributes(forBayesianFinal$x2)[[2]])/attributes(forBayesianFinal$x2)[[3]])

predictionsValidationV1<-predict(boostingMod1Final,data.matrix(forValidation[,2:3]),type="response")
predictionsValidationV1Unscaled<-predictionsValidationV1*attributes(forBayesianFinal$y)[[3]]+attributes(forBayesianFinal$y)[[2]]

forValidation$ErrorG1<-predictionsValidationV1Unscaled-forValidation$y
RMSEG1Validation<-sqrt(mean(forValidation$ErrorG1^2))#1.055896

predictionsValidationV2<-predict(boostingMod2Final,data.matrix(forValidation[,2:3]),type="response")
predictionsValidationV2Unscaled<-predictionsValidationV2*attributes(forBayesianFinal$y)[[3]]+attributes(forBayesianFinal$y)[[2]]

forValidation$ErrorG2<-predictionsValidationV2Unscaled-forValidation$y
RMSEG2Validation<-sqrt(mean(forValidation$ErrorG2^2))#1.049998

predictionsValidationV3<-predict(boostingMod3Final,data.matrix(forValidation[,2:3]),type="response")
predictionsValidationV3Unscaled<-predictionsValidationV3*attributes(forBayesianFinal$y)[[3]]+attributes(forBayesianFinal$y)[[2]]

forValidation$ErrorG3<-predictionsValidationV3Unscaled-forValidation$y
RMSEG3Validation<-sqrt(mean(forValidation$ErrorG3^2))#1.053849

#Controversially, the best metric were the second one from before with 2-fold CV rather than the most
#recent one with 5-fold CV
#But it made much more sense when I saw that those same tuning values from the second one were actually
#second in the third one in RMSE by the smallest of margins while winning in r-Squared.
#That was enough validation to pick those tuning values
check<-optimal_paramsV3$results
check<-check[order(check$RMSE),]
check<-check[order(-check$Rsquared),]

head(check)

#OK, so we're using the V2 metric.
#The only question is what % of the data to use for prediction.
#Let's start with 20%, 40%, 60%, 80%, and 100%.
#We'll use the same procedure, predicting on 1000 rows, but we'll do it 10 times.

sampleSizeComparison <- expand.grid(
  Round = 1:10,
  PercentOfData = seq(from=0.2,to=1,by=0.2),
  ValidationRMSE = rep(NA)
)

for(i in 1:10){
  head(buildDF)
  buildDF$rand<-rnorm(5000,0,5)
  buildDF<-buildDF[order(buildDF$rand),]
  
  valCheck<-buildDF[1:1000,]
  restCheck<-buildDF[1001:5000,]
  
  sampleFor20<-sample(1:nrow(restCheck),0.20*nrow(restCheck),replace=F)
  
  build20<-restCheck[sampleFor20,-4]
  head(build20)
  
  scaling20<-list(y = scale(build20$y),x1 = scale(build20$x1),
                  x2 = scale(build20$x2))
  str(scaling20)
  
  variables20<-as.data.frame(scale(build20[,2:3]))
  str(variables20)
  
  response20<-as.vector(scale(build20[,1]))
  str(response20)
  
  boostingMod2For20 <- xgboost::xgboost(data = data.matrix(variables20),
                                        label = response20,
                                        booster = "gbtree",
                                        objective = "reg:squarederror",
                                        nrounds = optimal_paramsV2$bestTune$nrounds,
                                        max_depth=optimal_paramsV2$bestTune$max_depth,
                                        colsample_bytree=optimal_paramsV2$bestTune$colsample_bytree,
                                        min_child_weight=optimal_paramsV2$bestTune$min_child_weight,
                                        subsample=optimal_paramsV2$bestTune$subsample,
                                        eta=optimal_paramsV2$bestTune$eta,
                                        gamma=optimal_paramsV2$bestTune$gamma,
                                        verbose = 0)
  
  val20<-valCheck[,1:3]
  val20$x1<-with(val20,(x1-attributes(scaling20$x1)[[2]])/attributes(scaling20$x1)[[3]])
  val20$x2<-with(val20,(x2-attributes(scaling20$x2)[[2]])/attributes(scaling20$x2)[[3]])
  
  predictions20<-predict(boostingMod2For20,data.matrix(val20[,2:3]),type="response")
  predictions20Unscaled<-predictions20*attributes(scaling20$y)[[3]]+attributes(scaling20$y)[[2]]
  
  val20$Error<-predictions20Unscaled-val20$y
  RMSE20<-sqrt(mean(val20$Error^2))
  
  sampleSizeComparison$ValidationRMSE[sampleSizeComparison$Round==i&sampleSizeComparison$PercentOfData==0.2]<-RMSE20
  
  sampleFor40<-sample(1:nrow(restCheck),0.40*nrow(restCheck),replace=F)
  
  build40<-restCheck[sampleFor40,-4]
  head(build40)
  
  scaling40<-list(y = scale(build40$y),x1 = scale(build40$x1),
                  x2 = scale(build40$x2))
  str(scaling40)
  
  variables40<-as.data.frame(scale(build40[,2:3]))
  str(variables40)
  
  response40<-as.vector(scale(build40[,1]))
  str(response40)
  
  boostingMod2For40 <- xgboost::xgboost(data = data.matrix(variables40),
                                        label = response40,
                                        booster = "gbtree",
                                        objective = "reg:squarederror",
                                        nrounds = optimal_paramsV2$bestTune$nrounds,
                                        max_depth=optimal_paramsV2$bestTune$max_depth,
                                        colsample_bytree=optimal_paramsV2$bestTune$colsample_bytree,
                                        min_child_weight=optimal_paramsV2$bestTune$min_child_weight,
                                        subsample=optimal_paramsV2$bestTune$subsample,
                                        eta=optimal_paramsV2$bestTune$eta,
                                        gamma=optimal_paramsV2$bestTune$gamma,
                                        verbose = 0)
  
  val40<-valCheck[,1:3]
  val40$x1<-with(val40,(x1-attributes(scaling40$x1)[[2]])/attributes(scaling40$x1)[[3]])
  val40$x2<-with(val40,(x2-attributes(scaling40$x2)[[2]])/attributes(scaling40$x2)[[3]])
  
  predictions40<-predict(boostingMod2For40,data.matrix(val40[,2:3]),type="response")
  predictions40Unscaled<-predictions40*attributes(scaling40$y)[[3]]+attributes(scaling40$y)[[2]]
  
  val40$Error<-predictions40Unscaled-val40$y
  RMSE40<-sqrt(mean(val40$Error^2))
  
  sampleSizeComparison$ValidationRMSE[sampleSizeComparison$Round==i&sampleSizeComparison$PercentOfData==0.4]<-RMSE40
  
  sampleFor60<-sample(1:nrow(restCheck),0.60*nrow(restCheck),replace=F)
  
  build60<-restCheck[sampleFor60,-4]
  head(build60)
  
  scaling60<-list(y = scale(build60$y),x1 = scale(build60$x1),
                  x2 = scale(build60$x2))
  str(scaling60)
  
  variables60<-as.data.frame(scale(build60[,2:3]))
  str(variables60)
  
  response60<-as.vector(scale(build60[,1]))
  str(response60)
  
  boostingMod2For60 <- xgboost::xgboost(data = data.matrix(variables60),
                                        label = response60,
                                        booster = "gbtree",
                                        objective = "reg:squarederror",
                                        nrounds = optimal_paramsV2$bestTune$nrounds,
                                        max_depth=optimal_paramsV2$bestTune$max_depth,
                                        colsample_bytree=optimal_paramsV2$bestTune$colsample_bytree,
                                        min_child_weight=optimal_paramsV2$bestTune$min_child_weight,
                                        subsample=optimal_paramsV2$bestTune$subsample,
                                        eta=optimal_paramsV2$bestTune$eta,
                                        gamma=optimal_paramsV2$bestTune$gamma,
                                        verbose = 0)
  
  val60<-valCheck[,1:3]
  val60$x1<-with(val60,(x1-attributes(scaling60$x1)[[2]])/attributes(scaling60$x1)[[3]])
  val60$x2<-with(val60,(x2-attributes(scaling60$x2)[[2]])/attributes(scaling60$x2)[[3]])
  
  predictions60<-predict(boostingMod2For60,data.matrix(val60[,2:3]),type="response")
  predictions60Unscaled<-predictions60*attributes(scaling60$y)[[3]]+attributes(scaling60$y)[[2]]
  
  val60$Error<-predictions60Unscaled-val60$y
  RMSE60<-sqrt(mean(val60$Error^2))
  
  sampleSizeComparison$ValidationRMSE[sampleSizeComparison$Round==i&round(sampleSizeComparison$PercentOfData,1)==0.6]<-RMSE60
  
  sampleFor80<-sample(1:nrow(restCheck),0.80*nrow(restCheck),replace=F)
  
  build80<-restCheck[sampleFor80,-4]
  head(build80)
  
  scaling80<-list(y = scale(build80$y),x1 = scale(build80$x1),
                  x2 = scale(build80$x2))
  str(scaling80)
  
  variables80<-as.data.frame(scale(build80[,2:3]))
  str(variables80)
  
  response80<-as.vector(scale(build80[,1]))
  str(response80)
  
  boostingMod2For80 <- xgboost::xgboost(data = data.matrix(variables80),
                                        label = response80,
                                        booster = "gbtree",
                                        objective = "reg:squarederror",
                                        nrounds = optimal_paramsV2$bestTune$nrounds,
                                        max_depth=optimal_paramsV2$bestTune$max_depth,
                                        colsample_bytree=optimal_paramsV2$bestTune$colsample_bytree,
                                        min_child_weight=optimal_paramsV2$bestTune$min_child_weight,
                                        subsample=optimal_paramsV2$bestTune$subsample,
                                        eta=optimal_paramsV2$bestTune$eta,
                                        gamma=optimal_paramsV2$bestTune$gamma,
                                        verbose = 0)
  
  val80<-valCheck[,1:3]
  val80$x1<-with(val80,(x1-attributes(scaling80$x1)[[2]])/attributes(scaling80$x1)[[3]])
  val80$x2<-with(val80,(x2-attributes(scaling80$x2)[[2]])/attributes(scaling80$x2)[[3]])
  
  predictions80<-predict(boostingMod2For80,data.matrix(val80[,2:3]),type="response")
  predictions80Unscaled<-predictions80*attributes(scaling80$y)[[3]]+attributes(scaling80$y)[[2]]
  
  val80$Error<-predictions80Unscaled-val80$y
  RMSE80<-sqrt(mean(val80$Error^2))
  
  sampleSizeComparison$ValidationRMSE[sampleSizeComparison$Round==i&sampleSizeComparison$PercentOfData==0.8]<-RMSE80
  
  sampleFor100<-sample(1:nrow(restCheck),1.00*nrow(restCheck),replace=F)
  
  build100<-restCheck[sampleFor100,-4]
  head(build100)
  
  scaling100<-list(y = scale(build100$y),x1 = scale(build100$x1),
                   x2 = scale(build100$x2))
  str(scaling100)
  
  variables100<-as.data.frame(scale(build100[,2:3]))
  str(variables100)
  
  response100<-as.vector(scale(build100[,1]))
  str(response100)
  
  boostingMod2For100 <- xgboost::xgboost(data = data.matrix(variables100),
                                         label = response100,
                                         booster = "gbtree",
                                         objective = "reg:squarederror",
                                         nrounds = optimal_paramsV2$bestTune$nrounds,
                                         max_depth=optimal_paramsV2$bestTune$max_depth,
                                         colsample_bytree=optimal_paramsV2$bestTune$colsample_bytree,
                                         min_child_weight=optimal_paramsV2$bestTune$min_child_weight,
                                         subsample=optimal_paramsV2$bestTune$subsample,
                                         eta=optimal_paramsV2$bestTune$eta,
                                         gamma=optimal_paramsV2$bestTune$gamma,
                                         verbose = 0)
  
  val100<-valCheck[,1:3]
  val100$x1<-with(val100,(x1-attributes(scaling100$x1)[[2]])/attributes(scaling100$x1)[[3]])
  val100$x2<-with(val100,(x2-attributes(scaling100$x2)[[2]])/attributes(scaling100$x2)[[3]])
  
  predictions100<-predict(boostingMod2For100,data.matrix(val100[,2:3]),type="response")
  predictions100Unscaled<-predictions100*attributes(scaling100$y)[[3]]+attributes(scaling100$y)[[2]]
  
  val100$Error<-predictions100Unscaled-val100$y
  RMSE100<-sqrt(mean(val100$Error^2))
  
  sampleSizeComparison$ValidationRMSE[sampleSizeComparison$Round==i&sampleSizeComparison$PercentOfData==1.0]<-RMSE100
}

#Using 100% was best, so let's just do that.

scalingFull<-list(y = scale(buildDF$y),x1 = scale(buildDF$x1),
                  x2 = scale(buildDF$x2))
str(scalingFull)

variablesFull<-as.data.frame(scale(buildDF[,2:3]))
str(variablesFull)

responseFull<-as.vector(scale(buildDF[,1]))
str(responseFull)

boostingMod2ForPrediction <- xgboost::xgboost(data = data.matrix(variablesFull),
                                              label = responseFull,
                                              booster = "gbtree",
                                              objective = "reg:squarederror",
                                              nrounds = optimal_paramsV2$bestTune$nrounds,
                                              max_depth=optimal_paramsV2$bestTune$max_depth,
                                              colsample_bytree=optimal_paramsV2$bestTune$colsample_bytree,
                                              min_child_weight=optimal_paramsV2$bestTune$min_child_weight,
                                              subsample=optimal_paramsV2$bestTune$subsample,
                                              eta=optimal_paramsV2$bestTune$eta,
                                              gamma=optimal_paramsV2$bestTune$gamma,
                                              verbose = 0)

final<-read.csv("testData.csv",header=T)
summary(final)
summary(buildDF)
#The test data looks to be distributed very similarly to what I was building on
#- I don't have to worry about that

testScaled<-final
testScaled$x1<-with(testScaled,(x1-attributes(scalingFull$x1)[[2]])/attributes(scalingFull$x1)[[3]])
testScaled$x2<-with(testScaled,(x2-attributes(scalingFull$x2)[[2]])/attributes(scalingFull$x2)[[3]])

predictionsToUse<-predict(boostingMod2ForPrediction,data.matrix(testScaled[,2:3]),type="response")
predictionsToUseUnscaled<-predictionsToUse*attributes(scalingFull$y)[[3]]+attributes(scalingFull$y)[[2]]

final$Predictions<-predictionsToUseUnscaled

summary(final)

summary(buildDF)

summary(predictions100Unscaled)

str(final)

#Publishing the predictions as a CSV
write.csv(final,file="TestDataPredictions.csv")