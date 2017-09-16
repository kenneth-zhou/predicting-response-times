
################  LOADING LIBRARIES ################ 
######## ######## ######## ######## ######## ######## 
library(readr)
library(randomForest)
library(lubridate) #hours() and minutes()
library(xgboost)
library(dplyr)
library(caret)
setwd("C:/Users/Kenneth/Desktop/Kaggle")

################  READING DATA ###################### 
######## ######## ######## ######## ######## ######## 

train = read.csv("lafdtraining.csv")
test = read.csv("testing.csv")
testog = read.csv("testing.csv")
stationresponsemetrics = read.csv("All_Stations_Response_Metrics.csv", stringsAsFactors = FALSE)
cityresponsemetrics = read.csv("Citywide_Response_Metrics.csv", stringsAsFactors = FALSE)
firestations = read.csv("fire_stations.csv")

################  FEATURE CORRELATIONS ################ 
######## ######## ######## ######## ######## ######## 

## 0.x correlations:
# dispatch sequence
# UnitType arson

## 0.0x correlations:
# hour (numeric)
# rush hour
# PPE level
# hour_time1
# hour_time2
# Unit type battalion chief
# Unit type bush patrol
# Unit.TypeWT - Water Tender
# Unit.TypeUR - USAR Apparatus
# ... some other unit types
# dispatch status (numeric)
#dispatch status CAV
#station_travel_time
#station_incident
#turnout_difference
#travel_difference

## 0.00x correlations:
#dispatch status
#individual years
#station_turnout_time
#city_turnout_time
#city_travel_time

## Below 0.00x correlations:
#fire station (numeric) - individual fire stations hover between 0.0x and 0.00x and below
#Unit Type (numeric)
#year (numeric)
#city_call_time

######### MERGING STATION RESPONSE METRICS ########## 
######## ######## ######## ######## ######## ######## 

#split train into EMS and non-EMS
train_EMS = train[train$PPE.Level == "EMS",]
train_nonEMS = train[train$PPE.Level != "EMS",]

#omitting year 2017
stationresponsemetrics = stationresponsemetrics[stationresponsemetrics$YEAR != 2017,]
#keeping only overall
stationresponsemetrics = stationresponsemetrics[stationresponsemetrics$MONTH %in% c("Overall", "Overall 2014", "Overall 2015", "Overall 2016"),]
#changing column names for merge
colnames(stationresponsemetrics)[c(1,2)] = c("year", "First.in.District")

#merging station response metrics with EMS
train_EMS = left_join(train_EMS, stationresponsemetrics, by = c("year","First.in.District"))
#removing unwanted variables
train_EMS = train_EMS[,-c(12,13,15,17)]
#changing var names
colnames(train_EMS)[c(12,13,14)] = c("station_turnout_time","station_travel_time","station_incident")

#merging station response metrics with EMS
train_nonEMS = left_join(train_nonEMS, stationresponsemetrics, by = c("year","First.in.District"))
#removing unwanted variables
train_nonEMS = train_nonEMS[,-c(12,14,16,18)]
#changing var names
colnames(train_nonEMS)[c(12,13,14)] = c("station_turnout_time","station_travel_time","station_incident")

#split test into EMS and non-EMS
test_EMS = test[test$PPE.Level == "EMS",]
test_nonEMS = test[test$PPE.Level != "EMS",]

#merging station response metrics with EMS
test_EMS = left_join(test_EMS, stationresponsemetrics, by = c("year","First.in.District"))
#removing unwanted variables
test_EMS = test_EMS[,-c(11,12,14,16)]
#changing var names
colnames(test_EMS)[c(11,12,13)] = c("station_turnout_time","station_travel_time","station_incident")

#merging station response metrics with EMS
test_nonEMS = left_join(test_nonEMS, stationresponsemetrics, by = c("year","First.in.District"))
#removing unwanted variables
test_nonEMS = test_nonEMS[,-c(11,13,15,17)]
#changing var names
colnames(test_nonEMS)[c(11,12,13)] = c("station_turnout_time","station_travel_time","station_incident")

#merge EMS and non-EMS back
train = rbind(train_EMS, train_nonEMS)
test = rbind(test_EMS, test_nonEMS)

rm(train_EMS)
rm(train_nonEMS)
rm(test_EMS)
rm(test_nonEMS)
rm(stationresponsemetrics)

######### MERGING CITYWIDE RESPONSE METRICS ########## 
######## ######## ######## ######## ######## ######## 

#split train into EMS and non-EMS
train_EMS = train[train$PPE.Level == "EMS",]
train_nonEMS = train[train$PPE.Level != "EMS",]

#omitting year 2017
cityresponsemetrics = cityresponsemetrics[cityresponsemetrics$YEAR != 2017,]
#keeping only overall
cityresponsemetrics = cityresponsemetrics[cityresponsemetrics$MONTH %in% c("Overall 2013", "Overall 2014", "Overall 2015", "Overall 2016"),]
#changing column names for merge
colnames(cityresponsemetrics)[c(1)] = c("year")

#merging station response metrics with EMS
train_EMS = left_join(train_EMS, cityresponsemetrics, by = c("year"))
#removing unwanted variables
train_EMS = train_EMS[,-c(15,17,19)]
#changing var names
colnames(train_EMS)[c(15,16,17)] = c("city_call_time","city_turnout_time","city_travel_time")

#merging station response metrics with EMS
train_nonEMS = left_join(train_nonEMS, cityresponsemetrics, by = c("year"))
#removing unwanted variables
train_nonEMS = train_nonEMS[,-c(15,18,20)]
#changing var names
colnames(train_nonEMS)[c(15,16,17)] = c("city_call_time","city_turnout_time","city_travel_time")

#split test into EMS and non-EMS
test_EMS = test[test$PPE.Level == "EMS",]
test_nonEMS = test[test$PPE.Level != "EMS",]

#merging station response metrics with EMS
test_EMS = left_join(test_EMS, cityresponsemetrics, by = c("year"))
#removing unwanted variables
test_EMS = test_EMS[,-c(14,16,18)]
#changing var names
colnames(test_EMS)[c(14,15,16)] = c("city_call_time","city_turnout_time","city_travel_time")

#merging station response metrics with EMS
test_nonEMS = left_join(test_nonEMS, cityresponsemetrics, by = c("year"))
#removing unwanted variables
test_nonEMS = test_nonEMS[,-c(14,17,19)]
#changing var names
colnames(test_nonEMS)[c(14,15,16)] = c("city_call_time","city_turnout_time","city_travel_time")

#merge EMS and non-EMS back
train = rbind(train_EMS, train_nonEMS)
test = rbind(test_EMS, test_nonEMS)
rm(train_EMS)
rm(train_nonEMS)
rm(test_EMS)
rm(test_nonEMS)
rm(cityresponsemetrics)

######## CHANGING RESPONSE METRICS TO SECS ######### 
######## ######## ######## ######## ######## ########

train$station_turnout_time <- strptime(train$station_turnout_time, format="%H:%M:%S")
train$station_turnout_time <- hour(train$station_turnout_time)*360 + minute(train$station_turnout_time)*60 + second(train$station_turnout_time)

train$station_travel_time <- strptime(train$station_travel_time, format="%H:%M:%S")
train$station_travel_time <- hour(train$station_travel_time)*360 + minute(train$station_travel_time)*60 + second(train$station_travel_time)

train$city_call_time <- strptime(train$city_call_time, format="%H:%M:%S")
train$city_call_time <- hour(train$city_call_time)*360 + minute(train$city_call_time)*60 + second(train$city_call_time)

train$city_turnout_time <- strptime(train$city_turnout_time, format="%H:%M:%S")
train$city_turnout_time <- hour(train$city_turnout_time)*360 + minute(train$city_turnout_time)*60 + second(train$city_turnout_time)

train$city_travel_time <- strptime(train$city_travel_time, format="%H:%M:%S")
train$city_travel_time <- hour(train$city_travel_time)*360 + minute(train$city_travel_time)*60 + second(train$city_travel_time)

train$turnout_difference = train$station_turnout_time - train$city_turnout_time
train$travel_difference = train$station_travel_time - train$city_travel_time

test$station_turnout_time <- strptime(test$station_turnout_time, format="%H:%M:%S")
test$station_turnout_time <- hour(test$station_turnout_time)*360 + minute(test$station_turnout_time)*60 + second(test$station_turnout_time)

test$station_travel_time <- strptime(test$station_travel_time, format="%H:%M:%S")
test$station_travel_time <- hour(test$station_travel_time)*360 + minute(test$station_travel_time)*60 + second(test$station_travel_time)

test$city_call_time <- strptime(test$city_call_time, format="%H:%M:%S")
test$city_call_time <- hour(test$city_call_time)*360 + minute(test$city_call_time)*60 + second(test$city_call_time)

test$city_turnout_time <- strptime(test$city_turnout_time, format="%H:%M:%S")
test$city_turnout_time <- hour(test$city_turnout_time)*360 + minute(test$city_turnout_time)*60 + second(test$city_turnout_time)

test$city_travel_time <- strptime(test$city_travel_time, format="%H:%M:%S")
test$city_travel_time <- hour(test$city_travel_time)*360 + minute(test$city_travel_time)*60 + second(test$city_travel_time)

test$turnout_difference = test$station_turnout_time - test$city_turnout_time
test$travel_difference = test$station_travel_time - test$city_travel_time

######### MERGING STATION DATA ###################### 
######## ######## ######## ######## ######## ######## 

colnames(firestations)[1] = c("First.in.District")
train = left_join(train, firestations, by = c("First.in.District"))
test = left_join(test, firestations, by = c("First.in.District"))

################  CREATION TIME ###################### 
######## ######## ######## ######## ######## ######## 

#new variables created: hour_time, minute_time, second_time, rush_hour

#creating hour, minute and second variables
creationtime <- strptime(train$Incident.Creation.Time..GMT., format="%H:%M:%S")
#run the same for test after:
#creationtime <- strptime(test$Incident.Creation.Time..GMT., format="%H:%M:%S")
hour_time <- hour(creationtime)
minute_time <- minute(creationtime)
second_time <- second(creationtime)

#rounding hours up/down
add_minutes <- function(hour, minutes)
{
  if (is.na(minutes))
  {
    return (hour)
  }
  if (is.na(hour))
  {
    return(hour)
  }
  return(hour + minutes/60)
}
add_seconds <- function(hour, seconds)
{
  if (is.na(seconds))
  {
    return (hour)
  }
  if (is.na(hour))
  {
    return(hour)
  }
  return(hour + seconds/360)
}
hour_time = mapply(add_minutes, hour_time, minute_time)
hour_time = mapply(add_seconds, hour_time, second_time)

#changing from GMT to LA time (PST)
hour_time <- hour_time - 8
change_time <- function(x)
{
  if (is.na(x)) #change NA values to 5
  {
   x = 5
  }
  if (x < 0)
  {
    x = 24 + x
  }
  return (x)
}
hour_time = sapply(hour_time, FUN = change_time)

#creating rush hour variable
rush_hour_function <- function(x)
{
  rush = 0
  if (x <= 10 && x >= 7)
  {
    rush = 1
  }
  if (x>=15 && x<= 20)
  {
    rush = 1
  }
  return(rush)
} 
rush_hour = sapply(hour_time, FUN=rush_hour_function)

################  RANDOM FOREST ###################### 
######## ######## ######## ######## ######## ######## 

#train$Station2 = train$`First in District`
#train$`First in District`[train$`First in District` > 58] = 0
#train$Station2[train$Station2 <= 58] = 0
#train$Station2 = factor(train$Station2)

#train$year = factor(train$year)
#train$incident.ID = factor(train$incident.ID)
#train$`First in District` = factor(train$`First in District`)
#train$`Dispatch Sequence` = factor(train$`Dispatch Sequence`)
#train$`Dispatch Status` = factor(train$`Dispatch Status`)
#train$`Unit Type` = factor(train$`Unit Type`)
#train$`PPE Level` = factor(train$`PPE Level`)
#colnames(train)[4] = c("Station")
#colnames(train)[6] = c("Sequence")
#colnames(train)[7] = c("Status")
#colnames(train)[8] = c("Unit")
#colnames(train)[9] = c("PPE")

#train = train[,-c(1,2,5,6,10)]
#train = na.omit(train)

#set.seed(1)
#rf_model = randomForest(elapsed_time~., data = (train_train), mtry = 6)
#rf_model

#ranger_model = ranger(elapsed_time~., data = train_train)

################  LINEAR MODEL  ########################## 
######## ######## ######## ######## ######## ########

train$year = factor(train$year)
test$year = factor(test$year)
train$First.in.District = factor(train$First.in.District)
test$First.in.District = factor(test$First.in.District)
train$Unit.Type = as.numeric(train$Unit.Type)
train$Unit.Type = as.numeric(train$Unit.Type)

linearmodel<-lm(elapsed_time~year+Unit.Type+First.in.District+poly(Dispatch.Sequence,7)+PPE.Level+Dispatch.Status+hour_time+rush_hour, data=train) 
prediction = predict(linearmodel, newdata = test) 
mean((prediction - train_test$elapsed_time)^2) #1318963

predictions = data.frame(testog$row.id, prediction)
colnames(predictions) = c("row.id", "prediction")
write.csv(predictions, "linearmodel.csv", row.names = FALSE)

################  XGBOOST  ########################## 
######## ######## ######## ######## ######## ########

##refining train data
train = train[,-c(1,2,5,10)] #removing redundant variables
train = cbind(train, hour_time)
train = cbind(train, rush_hour)
train$rush_hour = factor(train$rush_hour)
train = na.omit(train)
#train = train[!is.na(train$First.in.District),]#removing NAs for fire district variable
#train$elapsed_time[is.na(train$elapsed_time)] <- median(train$elapsed_time, na.rm = TRUE) #change NAs to median value
#train$Dispatch.Sequence[is.na(train$Dispatch.Sequence)] <- median(train$Dispatch.Sequence, na.rm = TRUE) #change NAs to median value

#doesn't work:
#train$arson = 0
#train$arson[train$Unit.Type %in% c("AR - Arson")] = 1 #create arson variable
#train = train[,-c(1,2,11,12,13)] #removing uncorrelated predictors
#train$Dispatch.Status = as.numeric(train$Dispatch.Status)
#train$Unit.Type = as.numeric(train$Unit.Type)
#train$rush_hour = factor(train$rush_hour) #factorizing rush hour
#train = na.omit(train)
#train.matrix = model.matrix(~ 0 + ., data = train[,-4])

#first.in.district, year, dispatch.sequence, PPE.level, population density, hour_time

xgboost_mod = xgboost(params = list(max.depth = 6), data = data.matrix(train[,-5]), label = data.matrix(train$elapsed_time), nrounds = 50, verbose = 1, early_stopping_rounds = 20, eta = 0.1)

##refining test data
test$Dispatch.Sequence[is.na(test$Dispatch.Sequence)] = 1 #change NAs in dispatch sequence to 2 (same with train)
test = test[,-c(1,2,5,10)] #removing redundant variables
test = cbind(test, hour_time)
test = cbind(test, rush_hour)
test$rush_hour = factor(test$rush_hour)

#doesn't work:
#test$arson = 0
#test$arson[test$Unit.Type %in% c("AR - Arson")] = 1 #create arson variable
#test = test[,-c(1,2,10,11,12)] #removing uncorrelated predictors
#test$Dispatch.Status = as.numeric(train$Dispatch.Status)
#test$Unit.Type = as.numeric(train$Unit.Type)
#test$rush_hour = factor(test$rush_hour) #factorizing rush hour
#test.matrix = model.matrix(~ 0 + ., data = test)

predictions = predict(xgboost_mod, newdata = data.matrix(test2))
predictions = data.frame(testog$row.id, predictions[1])
colnames(predictions) = c("row.id", "prediction")
write.csv(predictions, "xgboost.csv", row.names = FALSE)

##############  CROSS VALIDATION  ################### 
######## ######## ######## ######## ######## ########

set.seed(1)

indices = sample(1:nrow(train), nrow(train)*0.8)

train_train = train[indices,]
train_test = train[-indices,]

#Model Matrix ALL
xgboost_modelmatrix_300 = xgboost(data = model.matrix(~ 0 + ., data = train_train[,-7]), label = data.matrix(train_train$elapsed_time), nrounds = 300, verbose = 1, early_stopping_rounds = 50, eta = 0.1)
xgboost_modelmatrix_50 = xgboost(data = model.matrix(~ 0 + ., data = train_train[,-7]), label = data.matrix(train_train$elapsed_time), nrounds = 50, verbose = 1, early_stopping_rounds = 20, eta = 0.1)
predictions_modelmatrix_300 = predict(xgboost_modelmatrix_300, newdata = model.matrix(~ 0 + ., data = train_test[,-7])) 
predictions_modelmatrix_50 = predict(xgboost_modelmatrix_50, newdata = model.matrix(~ 0 + ., data = train_test[,-7]))
mean((predictions_modelmatrix_300 - train_test$elapsed_time)^2) #1335226
mean((predictions_modelmatrix_50 - train_test$elapsed_time)^2) #1339087

#Data Matrix ALL
xgboost_datamatrix_300 = xgboost(data = data.matrix(train_train[,-7]), label = data.matrix(train_train$elapsed_time), nrounds = 300, verbose = 1, early_stopping_rounds = 50, eta = 0.1)
xgboost_datamatrix_50 = xgboost(data = data.matrix(train_train[,-7]), label = data.matrix(train_train$elapsed_time), nrounds = 50, verbose = 1, early_stopping_rounds = 20, eta = 0.1)
predictions_datamatrix_300 = predict(xgboost_datamatrix_300, newdata = data.matrix(train_test[,-7])) 
predictions_datamatrix_50 = predict(xgboost_datamatrix_50, newdata = data.matrix(train_test[,-7]))
mean((predictions_datamatrix_300 - train_test$elapsed_time)^2) #1331573
mean((predictions_datamatrix_50 - train_test$elapsed_time)^2) #1338759

#Data Matrix without fire station
xgboost_datamatrix_300 = xgboost(data = data.matrix(train_train[,-c(2,7)]), label = data.matrix(train_train$elapsed_time), nrounds = 300, verbose = 1, early_stopping_rounds = 50, eta = 0.1)
predictions_datamatrix_300 = predict(xgboost_datamatrix_300, newdata = data.matrix(train_test[,-c(2,7)])) 
mean((predictions_datamatrix_300 - train_test$elapsed_time)^2) #1338277

#old with just minute, second, hour, rush hour and data.matrix
#Data matrix: 1173319

#with time var, all response metrics and data.matrix
#Data matrix: 1135913

#with time var, all response metrics, station data, and data.matrix
#Data.matrix: 1141177



