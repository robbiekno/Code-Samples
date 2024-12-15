#1) General Setup

#Bring in the Required R Packages for this analysis
require(xgboost)
require(caret)
require(data.table)
require(ggplot2)
require(Ckmeans.1d.dp)
require(dplyr)

#Ingest the data and get a feel for its size and structure
pitchesDF<-read.csv(file="pitches_2011 MLB",header=T)
head(pitchesDF)
str(pitchesDF)

#Looking at pitch types. A lot going on - most notably, there are some designations
#that aren't really pitch type (e.g. IN, PO) and some that have no information
#(e.g. Blank, UN). I left those out for this analysis
table(pitchesDF$pitch_type)

#Checking pitcher and batter handedness, which look fine. I've never seen Stand
#for batter handedness!
table(pitchesDF$p_throws)
table(pitchesDF$stand)

#Moving onto a new dataframe, making it a data.table to improve functionality
#and excluding the non-pitch types/unknown pitch types.
pitchesToSort<-data.table(pitchesDF)
table(pitchesToSort$pitch_type)

pitchesToSort<-pitchesToSort[pitchesToSort$pitch_type %in% c("CH","CU","EP",
                                                             "FA","FC","FF",
                                                             "FO","FS","FT",
                                                             "KC","KN","SC",
                                                             "SI","SL"),]

#Adding in a date rank column to easily compare the dates of games to each other
#This did the job, but I could definitely make the ranks look a bit nicer
#(ties made things look weird, though everything was fine for each game relative
#to each other game other than maybe some doubleheader or suspended game quirks)
pitchesToSort[, daterank := frank(date),
              by = list(year)]
pitchesToSort$daterank<-as.numeric(pitchesToSort$daterank)

#Looking into pitchers' usage of each pitch, want to get a feel for what's common
#and what to focus on
pitcherCheck<-summarise(group_by(pitchesToSort,pitcher_id),
                        CHRate = mean(pitch_type=="CH"),
                        CURate = mean(pitch_type=="CU"),
                        EPRate = mean(pitch_type=="EP"),
                        FARate = mean(pitch_type=="FA"),
                        FCRate = mean(pitch_type=="FC"),
                        FFRate = mean(pitch_type=="FF"),
                        FORate = mean(pitch_type=="FO"),
                        FSRate = mean(pitch_type=="FS"),
                        FTRate = mean(pitch_type=="FT"),
                        KCRate = mean(pitch_type=="KC"),
                        KNRate = mean(pitch_type=="KN"),
                        SCRate = mean(pitch_type=="SC"),
                        SIRate = mean(pitch_type=="SI"),
                        SLRate = mean(pitch_type=="SL"),
                        PitchCount = n())

#Looking at a table for each pitch type.
#Inspecting this, very few EP, FA, FO, and SC while KN is borderline
plot(table(pitchesToSort$pitch_type),ylab="")

#Looking at things by pitcher for each of those pitches
#Definitely important pitches for a few guys, but don't want
#the variables for everyone else.
#Let's have an Other pitch type, and then we can insert the
#right pitch type into that column based on what the pitcher has thrown
#Reducing the number of pitch types is also going to help us with runtime in a sec
hist(pitcherCheck$EPRate)
hist(pitcherCheck$FARate)
hist(pitcherCheck$FORate)
hist(pitcherCheck$SCRate)
hist(pitcherCheck$KNRate)

#Creating a new pitch type column with those less common pitches grouped together
pitchesToSort$pitchTypeForModel<-with(pitchesToSort,ifelse(pitch_type %in% c("EP","FA","FO","SC","KN"),"OT",pitch_type))
table(pitchesToSort$pitchTypeForModel)
table(pitchesToSort$pitch_type)

#2) Feature Engineering

#Overall, if we're trying to predict the next pitch, the mindset is to understand
#1) what pitches the pitcher generally throws
#2) which pitches he throws against the batter's handedness
#3) which pitches he throws in similar counts to the one he's in
#4) how previous pitches could affect the next one
#5) how usage for the pitcher within the game could affect things

#I'm intentionally not going to dive into stuff-related metrics - the pitcher
#is going to throw the best pitches that his team and him think he has
#If there is a sudden stuff difference, we can try to capture that based on
#in-game usage of the pitch (though I had trouble with that in the end, see below)

#We can try to dive into usage against the batter on a basic level as well
#though obviously a pitcher can only throw the pitches he has
#Looking at results in previous ABs or previous games
#is probably going to cause as many problems as it resolves.
#If I only have a few hours, let's spend most of it transforming the data
#so that we have past usages for each pitcher

#It does seem like we need to reduce the number of pitches per pitcher
#removing the pitches that no one throws.
#The question is whether we should reformat the variable so that it's each
#pitchers 1st, 2nd...5th most common pitches
#I didn't wind up doing that, but I discuss it more below
#It also seems difficult to get usage in the game prior to each pitch
#in this project timeframe.
#I would usually do that in SQL and I did't have time to figure it out in R.

#Getting set up to get each pitcher's usage prior to each game
pitcherPitchesByGame<-summarise(group_by(pitchesToSort,pitcher_id,daterank),
                                CHCount = sum(pitchTypeForModel=="CH"),
                                CUCount = sum(pitchTypeForModel=="CU"),
                                FCCount = sum(pitchTypeForModel=="FC"),
                                FFCount = sum(pitchTypeForModel=="FF"),
                                FSCount = sum(pitchTypeForModel=="FS"),
                                FTCount = sum(pitchTypeForModel=="FT"),
                                KCCount = sum(pitchTypeForModel=="KC"),
                                OTCount = sum(pitchTypeForModel=="OT"),
                                SICount = sum(pitchTypeForModel=="SI"),
                                SLCount = sum(pitchTypeForModel=="SL"),
                                TotalCount = n())

head(pitcherPitchesByGame)
nrow(pitcherPitchesByGame)

#This is an optimized function (much better than a for loop)
#that gets the pitcher's usage of each pitch prior to each game
TotalRates<-sapply(1:nrow(pitcherPitchesByGame), function(i){c(sum(pitcherPitchesByGame$CHCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])])/sum(pitcherPitchesByGame$TotalCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
                                                               sum(pitcherPitchesByGame$CUCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])])/sum(pitcherPitchesByGame$TotalCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
                                                               sum(pitcherPitchesByGame$FCCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])])/sum(pitcherPitchesByGame$TotalCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
                                                               sum(pitcherPitchesByGame$FFCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])])/sum(pitcherPitchesByGame$TotalCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
                                                               sum(pitcherPitchesByGame$FSCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])])/sum(pitcherPitchesByGame$TotalCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
                                                               sum(pitcherPitchesByGame$FTCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])])/sum(pitcherPitchesByGame$TotalCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
                                                               sum(pitcherPitchesByGame$KCCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])])/sum(pitcherPitchesByGame$TotalCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
                                                               sum(pitcherPitchesByGame$OTCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])])/sum(pitcherPitchesByGame$TotalCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
                                                               sum(pitcherPitchesByGame$SICount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])])/sum(pitcherPitchesByGame$TotalCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
                                                               sum(pitcherPitchesByGame$SLCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])])/sum(pitcherPitchesByGame$TotalCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
                                                               sum(pitcherPitchesByGame$TotalCount[with(pitcherPitchesByGame,pitcher_id==pitcher_id[i]&daterank<daterank[i])])
)})

head(TotalRates)

#Checking things for a pitcher with a lot of pitches thrown
which(pitcherPitchesByGame$pitcher_id==434378)
TotalRates[,which(pitcherPitchesByGame$pitcher_id==434378)]

#Adding the new prior usage variables to the dataframe
pitcherPitchesByGame$CHRatePrior<-TotalRates[1,]
pitcherPitchesByGame$CURatePrior<-TotalRates[2,]
pitcherPitchesByGame$FCRatePrior<-TotalRates[3,]
pitcherPitchesByGame$FFRatePrior<-TotalRates[4,]
pitcherPitchesByGame$FSRatePrior<-TotalRates[5,]
pitcherPitchesByGame$FTRatePrior<-TotalRates[6,]
pitcherPitchesByGame$KCRatePrior<-TotalRates[7,]
pitcherPitchesByGame$OTRatePrior<-TotalRates[8,]
pitcherPitchesByGame$SIRatePrior<-TotalRates[9,]
pitcherPitchesByGame$SLRatePrior<-TotalRates[10,]
pitcherPitchesByGame$PitchCountPrior<-TotalRates[11,]



#This section is the same as the previous one, but now for a pitcher's usage
#only against the current batter's handedness prior to the game
pitcherPitchesByBatHand<-summarise(group_by(pitchesToSort,pitcher_id,stand,daterank),
                                   CHCount = sum(pitchTypeForModel=="CH"),
                                   CUCount = sum(pitchTypeForModel=="CU"),
                                   FCCount = sum(pitchTypeForModel=="FC"),
                                   FFCount = sum(pitchTypeForModel=="FF"),
                                   FSCount = sum(pitchTypeForModel=="FS"),
                                   FTCount = sum(pitchTypeForModel=="FT"),
                                   KCCount = sum(pitchTypeForModel=="KC"),
                                   OTCount = sum(pitchTypeForModel=="OT"),
                                   SICount = sum(pitchTypeForModel=="SI"),
                                   SLCount = sum(pitchTypeForModel=="SL"),
                                   TotalCount = n())

head(pitcherPitchesByBatHand)
nrow(pitcherPitchesByBatHand)

TotalRatesBatHand<-sapply(1:nrow(pitcherPitchesByBatHand), function(i){c(sum(pitcherPitchesByBatHand$CHCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])])/sum(pitcherPitchesByBatHand$TotalCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])]),
                                                                         sum(pitcherPitchesByBatHand$CUCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])])/sum(pitcherPitchesByBatHand$TotalCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])]),
                                                                         sum(pitcherPitchesByBatHand$FCCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])])/sum(pitcherPitchesByBatHand$TotalCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])]),
                                                                         sum(pitcherPitchesByBatHand$FFCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])])/sum(pitcherPitchesByBatHand$TotalCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])]),
                                                                         sum(pitcherPitchesByBatHand$FSCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])])/sum(pitcherPitchesByBatHand$TotalCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])]),
                                                                         sum(pitcherPitchesByBatHand$FTCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])])/sum(pitcherPitchesByBatHand$TotalCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])]),
                                                                         sum(pitcherPitchesByBatHand$KCCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])])/sum(pitcherPitchesByBatHand$TotalCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])]),
                                                                         sum(pitcherPitchesByBatHand$OTCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])])/sum(pitcherPitchesByBatHand$TotalCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])]),
                                                                         sum(pitcherPitchesByBatHand$SICount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])])/sum(pitcherPitchesByBatHand$TotalCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])]),
                                                                         sum(pitcherPitchesByBatHand$SLCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])])/sum(pitcherPitchesByBatHand$TotalCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])]),
                                                                         sum(pitcherPitchesByBatHand$TotalCount[with(pitcherPitchesByBatHand,pitcher_id==pitcher_id[i]&stand==stand[i]&daterank<daterank[i])])
)})

head(TotalRatesBatHand)

which(pitcherPitchesByBatHand$pitcher_id==434378)
TotalRatesBatHand[,which(pitcherPitchesByBatHand$pitcher_id==434378)]

pitcherPitchesByBatHand$CHRatePriorBatHand<-TotalRatesBatHand[1,]
pitcherPitchesByBatHand$CURatePriorBatHand<-TotalRatesBatHand[2,]
pitcherPitchesByBatHand$FCRatePriorBatHand<-TotalRatesBatHand[3,]
pitcherPitchesByBatHand$FFRatePriorBatHand<-TotalRatesBatHand[4,]
pitcherPitchesByBatHand$FSRatePriorBatHand<-TotalRatesBatHand[5,]
pitcherPitchesByBatHand$FTRatePriorBatHand<-TotalRatesBatHand[6,]
pitcherPitchesByBatHand$KCRatePriorBatHand<-TotalRatesBatHand[7,]
pitcherPitchesByBatHand$OTRatePriorBatHand<-TotalRatesBatHand[8,]
pitcherPitchesByBatHand$SIRatePriorBatHand<-TotalRatesBatHand[9,]
pitcherPitchesByBatHand$SLRatePriorBatHand<-TotalRatesBatHand[10,]
pitcherPitchesByBatHand$PitchCountPriorBatHand<-TotalRatesBatHand[11,]

#Creating a count factor variable, combining balls and strikes, then checking it
pitchesToSort$Count<-with(pitchesToSort,factor(paste(balls,strikes,sep=",")))
table(pitchesToSort$Count)
sum(is.na(pitchesToSort$Count))

#Removing a few weird pitches with an impossible count
pitchesToSort<-pitchesToSort[pitchesToSort$Count!="4,2",]

# Creating a variable grouping by count. If I was starting from scratch, I could
# find this anew, but given the short timeframe of the project, I used knowledge
# from prior models I've built about which counts should be viewed similarly.
pitchesToSort$CountGroup<-with(pitchesToSort,ifelse(Count %in% c("0,0","1,1"),"Early Even",""))
pitchesToSort$CountGroup<-with(pitchesToSort,ifelse(Count %in% c("0,1","0,2","1,2","2,2"),"Aggressive",CountGroup))
pitchesToSort$CountGroup<-with(pitchesToSort,ifelse(Count %in% c("1,0","2,0","2,1","3,0","3,1","3,2"),"Behind",CountGroup))
table(pitchesToSort$CountGroup)

#Repeating the same process as above, but now getting a pitcher's usage prior to
#the game for each row based on count group.
pitcherPitchesByCountGroup<-summarise(group_by(pitchesToSort,pitcher_id,CountGroup,daterank),
                                      CHCount = sum(pitchTypeForModel=="CH"),
                                      CUCount = sum(pitchTypeForModel=="CU"),
                                      FCCount = sum(pitchTypeForModel=="FC"),
                                      FFCount = sum(pitchTypeForModel=="FF"),
                                      FSCount = sum(pitchTypeForModel=="FS"),
                                      FTCount = sum(pitchTypeForModel=="FT"),
                                      KCCount = sum(pitchTypeForModel=="KC"),
                                      OTCount = sum(pitchTypeForModel=="OT"),
                                      SICount = sum(pitchTypeForModel=="SI"),
                                      SLCount = sum(pitchTypeForModel=="SL"),
                                      TotalCount = n())

head(pitcherPitchesByCountGroup)
nrow(pitcherPitchesByCountGroup)

TotalRatesCountGroup<-sapply(1:nrow(pitcherPitchesByCountGroup), function(i){c(sum(pitcherPitchesByCountGroup$CHCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])])/sum(pitcherPitchesByCountGroup$TotalCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])]),
                                                                               sum(pitcherPitchesByCountGroup$CUCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])])/sum(pitcherPitchesByCountGroup$TotalCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])]),
                                                                               sum(pitcherPitchesByCountGroup$FCCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])])/sum(pitcherPitchesByCountGroup$TotalCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])]),
                                                                               sum(pitcherPitchesByCountGroup$FFCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])])/sum(pitcherPitchesByCountGroup$TotalCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])]),
                                                                               sum(pitcherPitchesByCountGroup$FSCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])])/sum(pitcherPitchesByCountGroup$TotalCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])]),
                                                                               sum(pitcherPitchesByCountGroup$FTCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])])/sum(pitcherPitchesByCountGroup$TotalCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])]),
                                                                               sum(pitcherPitchesByCountGroup$KCCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])])/sum(pitcherPitchesByCountGroup$TotalCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])]),
                                                                               sum(pitcherPitchesByCountGroup$OTCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])])/sum(pitcherPitchesByCountGroup$TotalCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])]),
                                                                               sum(pitcherPitchesByCountGroup$SICount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])])/sum(pitcherPitchesByCountGroup$TotalCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])]),
                                                                               sum(pitcherPitchesByCountGroup$SLCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])])/sum(pitcherPitchesByCountGroup$TotalCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])]),
                                                                               sum(pitcherPitchesByCountGroup$TotalCount[with(pitcherPitchesByCountGroup,pitcher_id==pitcher_id[i]&CountGroup==CountGroup[i]&daterank<daterank[i])])
)})

head(TotalRatesCountGroup)


which(pitcherPitchesByCountGroup$pitcher_id==434378)
TotalRatesCountGroup[,which(pitcherPitchesByCountGroup$pitcher_id==434378)]

pitcherPitchesByCountGroup$CHRatePriorCountGroup<-TotalRatesCountGroup[1,]
pitcherPitchesByCountGroup$CURatePriorCountGroup<-TotalRatesCountGroup[2,]
pitcherPitchesByCountGroup$FCRatePriorCountGroup<-TotalRatesCountGroup[3,]
pitcherPitchesByCountGroup$FFRatePriorCountGroup<-TotalRatesCountGroup[4,]
pitcherPitchesByCountGroup$FSRatePriorCountGroup<-TotalRatesCountGroup[5,]
pitcherPitchesByCountGroup$FTRatePriorCountGroup<-TotalRatesCountGroup[6,]
pitcherPitchesByCountGroup$KCRatePriorCountGroup<-TotalRatesCountGroup[7,]
pitcherPitchesByCountGroup$OTRatePriorCountGroup<-TotalRatesCountGroup[8,]
pitcherPitchesByCountGroup$SIRatePriorCountGroup<-TotalRatesCountGroup[9,]
pitcherPitchesByCountGroup$SLRatePriorCountGroup<-TotalRatesCountGroup[10,]
pitcherPitchesByCountGroup$PitchCountPriorCountGroup<-TotalRatesCountGroup[11,]

#Grouping pitch types to help us get a general idea of how each batter is pitched to
#I intentionally left out knuckleballs, whose usage varies much less based on batter
pitchesToSort$PitchGroup<-with(pitchesToSort,ifelse(pitch_type %in% c("FA","FC","FF","FT","SI"),"Fastball",""))
pitchesToSort$PitchGroup<-with(pitchesToSort,ifelse(pitch_type %in% c("CU","EP","KC","SC","SL"),"Breaking Ball",PitchGroup))
pitchesToSort$PitchGroup<-with(pitchesToSort,ifelse(pitch_type %in% c("CH","FO","FS"),"Offspeed",PitchGroup))
table(pitchesToSort$PitchGroup)

#Now repeating the process above for pitch usage against each batter in each row
#based on the categories of pitches we just created
batterPitchesByThrowsAndPitchGroup<-summarise(group_by(pitchesToSort,batter_id,p_throws,daterank),
                                              FastballCount = sum(PitchGroup=="Fastball"),
                                              BreakingCount = sum(PitchGroup=="Breaking Ball"),
                                              OffspeedCount = sum(PitchGroup=="Offspeed"),
                                              RelevantTotalCount = sum(PitchGroup!=""))

head(batterPitchesByThrowsAndPitchGroup)
nrow(batterPitchesByThrowsAndPitchGroup)

TotalRatesBatterPlatoon<-sapply(1:nrow(batterPitchesByThrowsAndPitchGroup), function(i){c(sum(batterPitchesByThrowsAndPitchGroup$FastballCount[with(batterPitchesByThrowsAndPitchGroup,batter_id==batter_id[i]&p_throws==p_throws[i]&daterank<daterank[i])])/sum(batterPitchesByThrowsAndPitchGroup$RelevantTotalCount[with(batterPitchesByThrowsAndPitchGroup,batter_id==batter_id[i]&p_throws==p_throws[i]&daterank<daterank[i])]),
                                                                                          sum(batterPitchesByThrowsAndPitchGroup$BreakingCount[with(batterPitchesByThrowsAndPitchGroup,batter_id==batter_id[i]&p_throws==p_throws[i]&daterank<daterank[i])])/sum(batterPitchesByThrowsAndPitchGroup$RelevantTotalCount[with(batterPitchesByThrowsAndPitchGroup,batter_id==batter_id[i]&p_throws==p_throws[i]&daterank<daterank[i])]),
                                                                                          sum(batterPitchesByThrowsAndPitchGroup$OffspeedCount[with(batterPitchesByThrowsAndPitchGroup,batter_id==batter_id[i]&p_throws==p_throws[i]&daterank<daterank[i])])/sum(batterPitchesByThrowsAndPitchGroup$RelevantTotalCount[with(batterPitchesByThrowsAndPitchGroup,batter_id==batter_id[i]&p_throws==p_throws[i]&daterank<daterank[i])]),
                                                                                          sum(batterPitchesByThrowsAndPitchGroup$RelevantTotalCount[with(batterPitchesByThrowsAndPitchGroup,batter_id==batter_id[i]&p_throws==p_throws[i]&daterank<daterank[i])])
)})

which(batterPitchesByThrowsAndPitchGroup$batter_id==110029)
TotalRatesBatterPlatoon[,which(batterPitchesByThrowsAndPitchGroup$batter_id==110029)]

head(TotalRatesBatterPlatoon[,2])

batterPitchesByThrowsAndPitchGroup$FastballRatePriorBatterPlatoon<-TotalRatesBatterPlatoon[1,]
batterPitchesByThrowsAndPitchGroup$BreakingBallRatePriorBatterPlatoon<-TotalRatesBatterPlatoon[2,]
batterPitchesByThrowsAndPitchGroup$OffspeedRatePriorBatterPlatoon<-TotalRatesBatterPlatoon[3,]
batterPitchesByThrowsAndPitchGroup$PitchCountPriorBatterPlatoon<-TotalRatesBatterPlatoon[4,]

#Creating a new variable that is the pitch in game that is continuous 
#(unlike the pitch_id variable that skips between ABS)
#and also removes the pitches that we took out above.
pitchesToSort[, PitchInGame := frank(pitch_id),
              by = list(game_pk)]

#Getting whether a given pitch was a swing-and-miss
pitchesToSort$IsMiss<-with(pitchesToSort,ifelse(pitch_des %in% c("Swinging Strike","Swinging Strike (Blocked)"),1,0))
table(pitchesToSort$IsMiss)

#Creating a dataframe to join in to the main one for prior pitches.
#I grabbed a bunch of primary key types of variables, checks to make sure the
#same batter is batting and pitcher is pitching (to make sure the prior pitches
#are relevant), and then pitch type and whether each pitch was a miss
pitchesForJoin<-pitchesToSort[,c("game_pk","PitchInGame","inning","batter_id","pitcher_id","IsMiss","pitchTypeForModel")]

#Getting ready to join in the prior pitch
pitchesForJoinPrior<-pitchesForJoin
pitchesForJoinPrior$PitchInGame<-pitchesForJoinPrior$PitchInGame+1
colnames(pitchesForJoinPrior)[6:7]<-c("IsMissPrior","pitchTypeForModelPrior")

#Joining in 2 pitches before
pitchesForJoin2Prior<-pitchesForJoin
pitchesForJoin2Prior$PitchInGame<-pitchesForJoin2Prior$PitchInGame+2
colnames(pitchesForJoin2Prior)[6:7]<-c("IsMiss2Prior","pitchTypeForModel2Prior")

#Joining in the prior pitch and confirming that it's only there within the same AB
pitchesToSort<-left_join(pitchesToSort,pitchesForJoinPrior,by=c("game_pk","PitchInGame","inning","batter_id","pitcher_id"))
print.data.frame(head(pitchesToSort[,c("game_pk","PitchInGame","inning","batter_id","pitcher_id","pitchTypeForModel","pitchTypeForModelPrior")],100))

#Setting all prior pitches from a previous batter to be blank rather than NA
#for inclusion in the XGBoost model below.
#For IsMiss, having irrelevant pitches be zero is probably something I would change
#if I was running this again
pitchesToSort$pitchTypeForModelPrior[is.na(pitchesToSort$pitchTypeForModelPrior)]<-""
pitchesToSort$IsMissPrior[is.na(pitchesToSort$IsMissPrior)]<-0

#Same thing for 2 pitches before
pitchesToSort<-left_join(pitchesToSort,pitchesForJoin2Prior,by=c("game_pk","PitchInGame","inning","batter_id","pitcher_id"))
print.data.frame(head(pitchesToSort[,c("game_pk","PitchInGame","inning","batter_id","pitcher_id","pitchTypeForModel","pitchTypeForModelPrior","pitchTypeForModel2Prior")],100))

pitchesToSort$pitchTypeForModel2Prior[is.na(pitchesToSort$pitchTypeForModel2Prior)]<-""
pitchesToSort$IsMiss2Prior[is.na(pitchesToSort$IsMiss2Prior)]<-0

#Now joining in all of the prior pitch usages that we calculated
#First, I make sure to select only the relevant columns
head(pitcherPitchesByGame)
colnames(pitcherPitchesByGame)
totalUsageForJoin<-pitcherPitchesByGame[,c(1:2,14:24)]
print.data.frame(head(totalUsageForJoin))

#Then I do the join and check it
pitchesToSort<-left_join(pitchesToSort,totalUsageForJoin,by=c("pitcher_id","daterank"))
print.data.frame(tail(pitchesToSort[pitchesToSort$date=="2011-07-01",]))

#Same as above, but now pitch usage by batter handedness
head(pitcherPitchesByBatHand)
colnames(pitcherPitchesByBatHand)
platoonUsageForJoin<-pitcherPitchesByBatHand[,c(1:3,15:25)]

print.data.frame(head(platoonUsageForJoin))
pitchesToSort<-left_join(pitchesToSort,platoonUsageForJoin,by=c("pitcher_id","daterank","stand"))
print.data.frame(tail(pitchesToSort[pitchesToSort$date=="2011-07-01",]))

#Now usage by count group
head(pitcherPitchesByCountGroup)
colnames(pitcherPitchesByCountGroup)
countUsageForJoin<-pitcherPitchesByCountGroup[,c(1:3,15:25)]
print.data.frame(head(countUsageForJoin))

pitchesToSort<-left_join(pitchesToSort,countUsageForJoin,by=c("pitcher_id","daterank","CountGroup"))
print.data.frame(tail(pitchesToSort[pitchesToSort$date=="2011-07-01",]))

#Now usage against each batter
head(batterPitchesByThrowsAndPitchGroup)
colnames(batterPitchesByThrowsAndPitchGroup)
batterUsageForJoin<-batterPitchesByThrowsAndPitchGroup[,c(1:3,8:11)]
print.data.frame(head(batterUsageForJoin))

pitchesToSort<-left_join(pitchesToSort,batterUsageForJoin,by=c("batter_id","daterank","p_throws"))
print.data.frame(tail(pitchesToSort[pitchesToSort$date=="2011-07-01",]))

#Calculating a score difference variable
pitchesToSort$ScoreDiff<-with(pitchesToSort,ifelse(top==1,home_team_runs-away_team_runs,away_team_runs-home_team_runs))

#I didn't really have time to use this, but in this section, I 
#determine which pitch type should be used when the model predicts Other
#based on the way that we grouped pitches before
#The beginning of the code is basically the same as the usage code,
#though I used raw counts rather than usage here to speed up runtime
pitcherPitchesForOther<-summarise(group_by(pitchesToSort,pitcher_id,daterank),
                                  EPCount = sum(pitch_type=="EP"),
                                  FACount = sum(pitch_type=="FA"),
                                  FOCount = sum(pitch_type=="FO"),
                                  SCCount = sum(pitch_type=="SC"),
                                  KNCount = sum(pitch_type=="KC"))

head(pitcherPitchesForOther)
nrow(pitcherPitchesForOther)

summary(pitcherPitchesForOther)

OtherCounts<-sapply(1:nrow(pitcherPitchesForOther), function(i){c(
  sum(pitcherPitchesForOther$EPCount[with(pitcherPitchesForOther,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
  sum(pitcherPitchesForOther$FACount[with(pitcherPitchesForOther,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
  sum(pitcherPitchesForOther$FOCount[with(pitcherPitchesForOther,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
  sum(pitcherPitchesForOther$SCCount[with(pitcherPitchesForOther,pitcher_id==pitcher_id[i]&daterank<daterank[i])]),
  sum(pitcherPitchesForOther$KNCount[with(pitcherPitchesForOther,pitcher_id==pitcher_id[i]&daterank<daterank[i])])
)})

#Checking what usage for one of these pitches looks like
summary(OtherCounts[1,])

pitcherPitchesForOther$EPCountPrior<-OtherCounts[1,]
pitcherPitchesForOther$FACountPrior<-OtherCounts[2,]
pitcherPitchesForOther$FOCountPrior<-OtherCounts[3,]
pitcherPitchesForOther$SCCountPrior<-OtherCounts[4,]
pitcherPitchesForOther$KNCountPrior<-OtherCounts[5,]

#Seeing a pitcher who throws knuckleballs
print.data.frame(head(pitcherPitchesForOther[pitcherPitchesForOther$KNCount>0,]))

#Determining which "Other" pitch was most used and having that be the selection
#if the pitch type is labeled other. I do that column reordering (8,10:12,9)
#because FA seemed like the safest pitch type for Other if a pitcher has never
#thrown an Other pitch prior to a game.
OtherChoice <- pitcherPitchesForOther[,c(8,10:12,9)] %>%
  mutate(max_prob = max.col(., ties.method = "last"))

#Adding in that pitch choice to the main dataframe
pitcherPitchesForOther$ChoiceForOther<-OtherChoice$max_prob

#Getting the name of the Other pitch
pitcherPitchesForOther$PitchForOther<-sapply(1:nrow(pitcherPitchesForOther),function(i){colnames(pitcherPitchesForOther)[c(8,10:12,9)][[pitcherPitchesForOther$ChoiceForOther[i]]]})
pitcherPitchesForOther$PitchForOther<-substring(pitcherPitchesForOther$PitchForOther,1,2)

#Joining that Other pitch choice in
otherForJoin<-pitcherPitchesForOther[,c("pitcher_id","daterank","PitchForOther")]
pitchesToSort<-left_join(pitchesToSort,otherForJoin,by=c("pitcher_id","daterank"))
print.data.frame(head(pitchesToSort))

#3) Making Sure the New Features Are Making Sense

#Checking overall pitch usage prior to each pitch and putting it in a bar plot
#Overall, this makes sense
priorpitches <- data.frame(
  Pitch=sort(unique(pitchesToSort$pitchTypeForModel)), 
  Usage= colMeans(pitchesToSort[,139:148],na.rm=T))

ggplot(priorpitches, aes(x=Pitch, y=Usage)) + 
  geom_bar(stat = "identity")

#Same thing for pitch usage by batter handedness, for same-side matchups
#Good to see way more breaking balls than changeups
samesidepitches <- data.frame(
  Pitch=sort(unique(pitchesToSort$pitchTypeForModel)), 
  Usage= colMeans(pitchesToSort[pitchesToSort$p_throws==pitchesToSort$stand,150:159],na.rm=T))

ggplot(samesidepitches, aes(x=Pitch, y=Usage)) + 
  geom_bar(stat = "identity")

#Now for opposite-side matchups and we see more changeups
oppsidepitches <- data.frame(
  Pitch=sort(unique(pitchesToSort$pitchTypeForModel)), 
  Usage= colMeans(pitchesToSort[pitchesToSort$p_throws!=pitchesToSort$stand,150:159],na.rm=T))

ggplot(oppsidepitches, aes(x=Pitch, y=Usage)) + 
  geom_bar(stat = "identity")

#Now for 0-0 and 1-1 counts
evenpitches <- data.frame(
  Pitch=sort(unique(pitchesToSort$pitchTypeForModel)), 
  Usage= colMeans(pitchesToSort[pitchesToSort$CountGroup=="Early Even",161:170],na.rm=T))

ggplot(evenpitches, aes(x=Pitch, y=Usage)) + 
  geom_bar(stat = "identity")

#Now for 0-1 and 2-strike pitches. Great to see fewer fastballs, more sliders and changeups
aggressivepitches <- data.frame(
  Pitch=sort(unique(pitchesToSort$pitchTypeForModel)), 
  Usage= colMeans(pitchesToSort[pitchesToSort$CountGroup=="Aggressive",161:170],na.rm=T))

ggplot(aggressivepitches, aes(x=Pitch, y=Usage)) + 
  geom_bar(stat = "identity")

#Now behind in the count, appropriately way more fastballs
behindpitches <- data.frame(
  Pitch=sort(unique(pitchesToSort$pitchTypeForModel)), 
  Usage= colMeans(pitchesToSort[pitchesToSort$CountGroup=="Behind",161:170],na.rm=T))

ggplot(behindpitches, aes(x=Pitch, y=Usage)) + 
  geom_bar(stat = "identity")

#Checking things from the batter perspective for same-side matchups
samesidepitchesbatter <- data.frame(
  Pitch=c("Fastball","Breaking Ball","Offspeed"), 
  Usage= colMeans(pitchesToSort[pitchesToSort$p_throws==pitchesToSort$stand,172:174],na.rm=T))

ggplot(samesidepitchesbatter, aes(x=Pitch, y=Usage)) + 
  geom_bar(stat = "identity")

#And now for opposite-side matchups
oppsidepitchesbatter <- data.frame(
  Pitch=c("Fastball","Breaking Ball","Offspeed"), 
  Usage= colMeans(pitchesToSort[pitchesToSort$p_throws!=pitchesToSort$stand,172:174],na.rm=T))

ggplot(oppsidepitchesbatter, aes(x=Pitch, y=Usage)) + 
  geom_bar(stat = "identity")

#Overall, our usage features look good and let's finalize things for modeling

#4) Final Prep for Modeling

#Selecting the variables we want, the usage variables and some others.
#Definitely other variables that would be worth examining as well in the future
finalDF<-pitchesToSort[is.na(pitchesToSort$pitchTypeForModel)==F,c("pitchTypeForModel","inning","pcount_pitcher","Count","stand","p_throws","ScoreDiff",
                                                                   "pitchTypeForModelPrior","IsMissPrior","pitchTypeForModel2Prior","IsMiss2Prior",
                                                                   "CHRatePrior","CURatePrior","FCRatePrior","FFRatePrior","FSRatePrior","FTRatePrior",
                                                                   "KCRatePrior","OTRatePrior","SIRatePrior","SLRatePrior","PitchCountPrior",
                                                                   "CHRatePriorBatHand","CURatePriorBatHand","FCRatePriorBatHand","FFRatePriorBatHand","FSRatePriorBatHand","FTRatePriorBatHand",
                                                                   "KCRatePriorBatHand","OTRatePriorBatHand","SIRatePriorBatHand","SLRatePriorBatHand","PitchCountPriorBatHand",
                                                                   "CHRatePriorCountGroup","CURatePriorCountGroup","FCRatePriorCountGroup","FFRatePriorCountGroup","FSRatePriorCountGroup","FTRatePriorCountGroup",
                                                                   "KCRatePriorCountGroup","OTRatePriorCountGroup","SIRatePriorCountGroup","SLRatePriorCountGroup","PitchCountPriorCountGroup",
                                                                   "FastballRatePriorBatterPlatoon","BreakingBallRatePriorBatterPlatoon","OffspeedRatePriorBatterPlatoon","PitchCountPriorBatterPlatoon"
)]

tail(finalDF)

#Making some variables factors to meet model requirements and scaling others
finalDF$inning<-finalDF$inning/9
finalDF$pcount_pitcher<-finalDF$inning/100
finalDF$stand<-as.factor(finalDF$stand)
finalDF$p_throws<-as.factor(finalDF$p_throws)
finalDF$ScoreDiff<-as.numeric(scale(finalDF$ScoreDiff))
finalDF$pitchTypeForModelPrior<-as.factor(finalDF$pitchTypeForModelPrior)
finalDF$IsMissPrior<-as.factor(finalDF$IsMissPrior)
finalDF$pitchTypeForModel2Prior<-as.factor(finalDF$pitchTypeForModel2Prior)
finalDF$IsMiss2Prior<-as.factor(finalDF$IsMiss2Prior)
#The pitch usage explanation is already clear, between 0 and 1
finalDF$PitchCountPrior<-as.numeric(scale(log(finalDF$PitchCountPrior+.0001)))
finalDF$PitchCountPriorBatHand<-as.numeric(scale(log(finalDF$PitchCountPriorBatHand+.0001)))
finalDF$PitchCountPriorCountGroup<-as.numeric(scale(log(finalDF$PitchCountPriorCountGroup+.0001)))
finalDF$PitchCountPriorBatterPlatoon<-as.numeric(scale(log(finalDF$PitchCountPriorBatterPlatoon+.0001)))

sort(unique(pitchesToSort$pitchTypeForModel))

#Converting pitch type to this form because that's what the model requires
finalDF$PitchTypeThrown<-with(finalDF,ifelse(pitchTypeForModel=="CH",0,NA))
finalDF$PitchTypeThrown<-with(finalDF,ifelse(pitchTypeForModel=="CU",1,PitchTypeThrown))
finalDF$PitchTypeThrown<-with(finalDF,ifelse(pitchTypeForModel=="FC",2,PitchTypeThrown))
finalDF$PitchTypeThrown<-with(finalDF,ifelse(pitchTypeForModel=="FF",3,PitchTypeThrown))
finalDF$PitchTypeThrown<-with(finalDF,ifelse(pitchTypeForModel=="FS",4,PitchTypeThrown))
finalDF$PitchTypeThrown<-with(finalDF,ifelse(pitchTypeForModel=="FT",5,PitchTypeThrown))
finalDF$PitchTypeThrown<-with(finalDF,ifelse(pitchTypeForModel=="KC",6,PitchTypeThrown))
finalDF$PitchTypeThrown<-with(finalDF,ifelse(pitchTypeForModel=="OT",7,PitchTypeThrown))
finalDF$PitchTypeThrown<-with(finalDF,ifelse(pitchTypeForModel=="SI",8,PitchTypeThrown))
finalDF$PitchTypeThrown<-with(finalDF,ifelse(pitchTypeForModel=="SL",9,PitchTypeThrown))
table(finalDF$PitchTypeThrown)

#Splitting the data, 80% for training and 20% for testing
splitToUse<-createDataPartition(finalDF$PitchTypeThrown, p = 0.8, list = FALSE)
training = finalDF[splitToUse,-48]
testing = finalDF[-splitToUse,-48]

#Seeing the NA's that we need to address when there was no usage information
#prior to the game
summary(training)

#Converting back to a regular dataframe to help with the next part
training<-data.frame(training)


#Null-filling the mean. I'm running out of time - otherwise, I would null-fill more 
#based on platoon and count for the applicable variables
for(i in 1:length(training)){
  AverageToUse<-mean(unlist(training[,i]),na.rm=T)
  training[,i][is.na(training[,i])]<-AverageToUse
}


#Null-filling testing from the means of training
testing<-data.frame(testing)
for(i in 1:length(testing)){
  AverageToUse<-mean(unlist(training[,i]),na.rm=T)
  testing[,i][is.na(testing[,i])]<-AverageToUse
}

#The response variables for training and testing
trainingResponse<-finalDF$PitchTypeThrown[splitToUse]
testingResponse<-finalDF$PitchTypeThrown[-splitToUse]
  
#The number of classes
classNum <- length(unique(finalDF$PitchTypeThrown))

#5) Building the XGBoost Model and Checking It

#Running, with mostly default parameters to finish things off
#I focused more on building the features for this exercise than tuning the
#model, partially because the best methodology can only do so much with subpar
#features and partially because tuning takes forever.
pitchTypePredictionModel <- xgboost::xgboost(data = data.matrix(training),
                                             "label" = trainingResponse,
                                             "objective" = "multi:softprob",
                                             "eval_metric" = "mlogloss",
                                             "num_class" = classNum,
                                             nrounds = 50,
                                             verbose = 1)

#Getting the model's predictions on the training set. This code gets so
#convoluted because we need 10 different predictions for every row
outOfSamplePredTemp <- predict(pitchTypePredictionModel, newdata = data.matrix(testing))
outOfSamplePredictions <- matrix(outOfSamplePredTemp, nrow = classNum,
                                 ncol=length(outOfSamplePredTemp)/classNum) %>%
  t() %>%
  data.frame() 

#Adding in the predicted pitch - the one with the highest probability - and
#then we can compare it to what was actually thrown
outOfSamplePredictions<-outOfSamplePredictions %>%
  mutate(PredictedPitch = max.col(., "last")-1,
         ActualPitch = testingResponse,
  )

#Glancing at what the actual pitch types were compared to the predicted ones
#Way too many fastballs predicted seems to be the main thing that's off
table(outOfSamplePredictions$ActualPitch)
table(outOfSamplePredictions$PredictedPitch)

#Looking at the confusion matrix to see how everything looked and get some 
#summary statistics of the fit. Overall, this model is just getting started
#but is definitely showing some promise.
confusionMatrix(factor(outOfSamplePredictions$PredictedPitch),
                factor(outOfSamplePredictions$ActualPitch),
                mode = "everything")

#Checking a few notable splits within the data
#The model did a pretty good job on 3-0
table(outOfSamplePredictions$PredictedPitch[testing$Count=="3,0"])
table(outOfSamplePredictions$ActualPitch[testing$Count=="3,0"])

#But Not enough secondaries on 1-2, especially changeups
table(outOfSamplePredictions$PredictedPitch[testing$Count=="1,2"])
table(outOfSamplePredictions$ActualPitch[testing$Count=="1,2"])

#Directionally correct in terms of changeup vs. opposite-side and 
#breaking ball vs. same-side, but definitely overdid it
table(outOfSamplePredictions$PredictedPitch[testing$stand==testing$p_throws])
table(outOfSamplePredictions$ActualPitch[testing$stand==testing$p_throws])

table(outOfSamplePredictions$PredictedPitch[testing$stand!=testing$p_throws])
table(outOfSamplePredictions$ActualPitch[testing$stand!=testing$p_throws])

#Getting variable importance for the model
variableNames <-  colnames(finalDF[,-48])
importanceMatrix = xgb.importance(feature_names = variableNames, model = pitchTypePredictionModel)
head(importanceMatrix)

#Now plotting the top of it.
#Interesting what's at the top, particularly that overall usage wasn't too helpful.
importancePlot = xgb.ggplot.importance(importanceMatrix)
print(importancePlot) 

#6) What I Would Do Next:
# Feature Engineering: I would spend more time getting usage within the game
# (which would be particularly easier with a database) and have game usage as a new
# split, for all pitches prior to the current one
# I would make all usages go up to the most recent pitch.
# I would explore more stuff and located-related variables - for instance, maybe
# some hitters are weaker against high velocities and would see high-velo fastballs
# more frequently, and it would be interesting to see how a poorly located pitch
# such a 2-0 fastball way out of the zone, affects usage of the next pitch
# One simple thing that I mentioned above is that I would want more intelligent
# null-filling for the usages, basing it more specifically on the given platoon
# matchup or count group rather than the address of the whole column across groups.

# Outcome: the biggest thing I thought about doing was redefining pitch type to be
# on a pitcher basis, and then we would be looking at the likelihood of a pitcher's
# most common pitch, second-most pitch, etc. It would be interesting to see if that
# model would be better.
# I also wanted to use my code above to select which pitch any Other classifications
# really should be and then we can compare that to the actual classifications

# Tuning: The XGBoost model needs to  be tuned. It may take several rounds, computing
# the optimal parameters, checking the confusion matrix and several different splits
# and then going back to Feature Engineering with different ideas to fix things

#Thank you for your time!