################################ PACKAGES #####################################
library(smotefamily)
library(tictoc)
library(caret)
library(dplyr)
library(stargazer)
library(randomForest)
library(pROC)

################################ GET DATA - GENERAL #####################################
setwd('/Users/paula/Desktop/Speciale/R _Studio')
reservations_data <- data.frame
reservations_data = read.csv('reservations_data.csv')
final_data <- data.frame(reservations_data)
final_data = subset(final_data, select = -c(CancellationDate, Created, ReservationNo, CancellationTime, Arrival, Departure, GroupID, AgentID, CompanyID, Quarter))
#Convert to numeric values in order to perform SMOTE
final_data$ArrivalDay[final_data$ArrivalDay == 'monday'] <- '1'
final_data$ArrivalDay[final_data$ArrivalDay == 'tuesday'] <- '2'
final_data$ArrivalDay[final_data$ArrivalDay == 'wednesday'] <- '3'
final_data$ArrivalDay[final_data$ArrivalDay == 'thursday'] <- '4'
final_data$ArrivalDay[final_data$ArrivalDay == 'friday'] <- '5'
final_data$ArrivalDay[final_data$ArrivalDay == 'saturday'] <- '6'
final_data$ArrivalDay[final_data$ArrivalDay == 'sunday'] <- '7'
final_data$ArrivalMonth[final_data$ArrivalMonth == "jan"] <- '1'
final_data$ArrivalMonth[final_data$ArrivalMonth == "feb"] <- '2'
final_data$ArrivalMonth[final_data$ArrivalMonth == "mar"] <- '3'
final_data$ArrivalMonth[final_data$ArrivalMonth == "apr"] <- '4'
final_data$ArrivalMonth[final_data$ArrivalMonth == "maj"] <- '5'
final_data$ArrivalMonth[final_data$ArrivalMonth == "jun"] <- '6'
final_data$ArrivalMonth[final_data$ArrivalMonth == "jul"] <- '7'
final_data$ArrivalMonth[final_data$ArrivalMonth == "aug"] <- '8'
final_data$ArrivalMonth[final_data$ArrivalMonth == "sep"] <- '9'
final_data$ArrivalMonth[final_data$ArrivalMonth == "okt"] <- '10'
final_data$ArrivalMonth[final_data$ArrivalMonth == "nov"] <- '11'
final_data$ArrivalMonth[final_data$ArrivalMonth == "dec"] <- '12'
table(final_data$RoomType)
final_data$RoomType[final_data$RoomType == "Brid"] <- 1
final_data$RoomType[final_data$RoomType == "D/T"] <- 2
final_data$RoomType[final_data$RoomType == "DB2"] <- 3
final_data$RoomType[final_data$RoomType == "FAM"] <- 4
final_data$RoomType[final_data$RoomType == "Fami"] <- 5
final_data$RoomType[final_data$RoomType == "FAML"] <- 6
final_data$RoomType[final_data$RoomType == "JUSU"] <- 7
final_data$RoomType[final_data$RoomType == "L"] <- 8
final_data$RoomType[final_data$RoomType == "M"] <- 9
final_data$RoomType[final_data$RoomType == "Msea"] <- 10
final_data$RoomType[final_data$RoomType == "R272"] <- 11
final_data$RoomType[final_data$RoomType == "RUN"] <- 12
final_data$RoomType[final_data$RoomType == "RUNF"] <- 13
final_data$RoomType[final_data$RoomType == "S 1"] <- 14
final_data$RoomType[final_data$RoomType == "XS"] <- 15
final_data$Country <- as.numeric(factor(final_data$Country))
table(final_data$Department)
final_data$Department[final_data$Department == "HF"] <- 1
final_data$Department[final_data$Department == "HT"] <- 2
final_data$Department[final_data$Department == "FV"] <- 3

final_data_smote <- data.frame(final_data)
sapply(final_data_smote, class)
final_data_smote <- transform(
  final_data_smote,
  RoomType=as.integer(RoomType),
  BookingType=as.integer(BookingType),
  Group=as.integer(Group),
  Agent=as.integer(Agent),
  Company=as.integer(Company),
  Cancellation=as.numeric(Cancellation),
  ArrivalDay=as.integer(ArrivalDay),
  ArrivalMonth=as.integer(ArrivalMonth),
  Department=as.integer(Department),
  Country=as.integer(Country)
)

sapply(final_data, class)
final_data <- transform(
  final_data,
  RoomType=as.factor(RoomType),
  BookingType=as.factor(BookingType),
  Group=as.factor(Group),
  Agent=as.factor(Agent),
  Company=as.factor(Company),
  Cancellation=as.factor(Cancellation),
  ArrivalDay=as.factor(ArrivalDay),
  ArrivalMonth=as.factor(ArrivalMonth),
  Department=as.factor(Department)
)

final_data <- final_data %>% relocate(Cancellation, .after = last_col())
final_data_smote <- final_data_smote %>% relocate(Cancellation, .after = last_col())


################################ SPLITTING DATA - IMBALANCED #####################################
training_df <- final_data[1:66407, ]
testing_df <- final_data[66408:83009, ]

training_df_rf = subset(training_df,select = -c(Country))
testing_df_rf = subset(testing_df,select = -c(Country))
sapply(training_df_rf, class)
sapply(testing_df_rf, class)

################################ SMOTE ###########################
set.seed(100)

#General SMOTE
training_df_smote <- final_data_smote[1:66407, ]
testing_df_smote <- final_data_smote[66408:83009, ]

sapply(training_df_smote, class)

#Perform SMOTE oversamplong
smote_training = SMOTE(training_df_smote[-15], training_df_smote[15], K = 10, dup_size = 0)
#Save new training data
training_df_smote = smote_training$data
#Change the column name
colnames(training_df_smote)[15] <- c("Cancellation")
#Set Cancellation response as numeric
training_df_smote <- transform(training_df_smote,Cancellation=as.numeric(Cancellation))
#Round all values except pric
training_df_smote[,-4] <-round(training_df_smote[,-4],0)



#RF training SMOTE data
training_df_rf_smote = subset(training_df_smote,select = -c(Country))
testing_df_rf_smote = subset(testing_df_smote,select = -c(Country))

#Inspect numbers
table(training_df$Cancellation)
table(training_df_smote$Cancellation)
table(training_df_rf_smote$Cancellation)
#Both SMOTE training sets are identical

table(testing_df_rf$Cancellation)
table(testing_df_smote$Cancellation)


################################ IMBALANCED DATA #####################################
##ntree tuning (100,200,300,400,500,1000):
tic("Random Forest ntree=100")
rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=100, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall", positive = "1")
toc() 

tic("Random Forest ntree=200")
rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=200, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall", positive = "1")
toc() 

tic("Random Forest ntree=300")
rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=300, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall", positive = "1")
toc() 

tic("Random Forest ntree=400")
rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=400, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall", positive = "1")
toc() 

tic("Random Forest ntree=500")
rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=500, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall", positive = "1")
toc() 

tic("Random Forest ntree=500")
rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=600, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall", positive = "1")
toc() 

tic("Random Forest ntree=500")
rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=700, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall", positive = "1")
toc() 

tic("Random Forest ntree=500")
rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=800, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall", positive = "1")
toc() 

tic("Random Forest ntree=500")
rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=900, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall", positive = "1")
toc() 

tic("Random Forest ntree=1000")
rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=1000, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall", positive = "1")
toc() 


##mtry = 1,2,3,4,5,6,7 with optimal ntree
rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=300, mtry=1, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall",positive = "1")

rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=300, mtry=2, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall",positive = "1")

rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=300, mtry=3, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall",positive = "1")

rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=300, mtry=4, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall",positive = "1")
toc()

rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=300, mtry=5, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall",positive = "1")

rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=300, mtry=6, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall",positive = "1")

rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=300, mtry=7, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall",positive = "1")

###BEST MODEL - IMBALANCED:
rf_classifier = randomForest(Cancellation ~ ., data=training_df_rf, ntree=300, mtry=4, importance=TRUE)
rf_classifier
pred_rf = predict(rf_classifier, newdata = testing_df_rf)
cm_rf <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf)
confusionMatrix(cm_rf,mode = "prec_recall",positive = "1")

table(testing_df_rf$Cancellation)

pred_rf <- as.numeric(pred_rf)
roc_obj <- roc(response=testing_df_rf$Cancellation, predictor=pred_rf)
plot(roc_obj)
auc(roc_obj)
?roc

#Variable importance:
varImp(rf_classifier)
varImpPlot(rf_classifier, type=1,main="")

################################ BALANCED DATA #####################################
#combine the training ans testing before defining factors
full_data_rf_smote <- rbind(training_df_rf_smote,testing_df_rf_smote)

full_data_rf_smote <- transform(
  full_data_rf_smote,
  RoomType=as.factor(RoomType),
  NoChildren=as.integer(NoChildren),
  NoAdults=as.integer(NoAdults),
  BookingType=as.factor(BookingType),
  NoDays=as.integer(NoDays),
  LeadTime=as.integer(LeadTime),
  Group=as.factor(Group),
  Agent=as.factor(Agent),
  Company=as.factor(Company),
  Cancellation=as.factor(Cancellation),
  ArrivalDay=as.factor(ArrivalDay),
  ArrivalMonth=as.factor(ArrivalMonth),
  Department=as.factor(Department)
)
sapply(full_data_rf_smote, class)

#Split again:
training_df_rf_smote <- full_data_rf_smote[1:96203,]
testing_df_rf_smote <- full_data_rf_smote[96204:112805,]

#Fit the models
##ntree tuning (100,200,300,400,500,1000):
rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=100, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf)
cm_rf_balance <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall", positive = "1") 

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=200, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf)
cm_rf_balance <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall", positive = "1") 

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=300, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf)
cm_rf_balance <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall", positive = "1") 

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=400, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf)
cm_rf_balance <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall", positive = "1") 

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=500, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf)
cm_rf_balance <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall", positive = "1") 

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=600, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf)
cm_rf_balance <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall", positive = "1") 

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=700, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf)
cm_rf_balance <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall", positive = "1") 

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=800, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf)
cm_rf_balance <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall", positive = "1") 

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=900, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf)
cm_rf_balance <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall", positive = "1") 

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=1000, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf)
cm_rf_balance <- table(observed=testing_df_rf$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall", positive = "1") 

#mtry = 1, 2, 3,4,5,6,7 with optimal ntree
rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=800, mtry=1, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf_smote)
cm_rf_balance <- table(observed=testing_df_rf_smote$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall",positive = "1")

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=800, mtry=2, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf_smote)
cm_rf_balance <- table(observed=testing_df_rf_smote$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall",positive = "1")

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=800, mtry=3, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf_smote)
cm_rf_balance <- table(observed=testing_df_rf_smote$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall",positive = "1")

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=800, mtry=4, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf_smote)
cm_rf_balance <- table(observed=testing_df_rf_smote$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall",positive = "1")

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=800, mtry=5, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf_smote)
cm_rf_balance <- table(observed=testing_df_rf_smote$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall",positive = "1")

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=800, mtry=6, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf_smote)
cm_rf_balance <- table(observed=testing_df_rf_smote$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall",positive = "1")

rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=800, mtry=7, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf_smote)
cm_rf_balance <- table(observed=testing_df_rf_smote$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall",positive = "1")


###BEST MODEL:
rf_classifier_balance = randomForest(Cancellation ~ ., data=training_df_rf_smote, ntree=500, mtry=4, importance=TRUE)
rf_classifier_balance
pred_rf_balance = predict(rf_classifier_balance, newdata = testing_df_rf_smote)
cm_rf_balance <- table(observed=testing_df_rf_smote$Cancellation, predicted=pred_rf_balance)
confusionMatrix(cm_rf_balance,mode = "prec_recall",positive = "1")


pred_rf_balance <- as.numeric(pred_rf_balance)
roc_obj <- roc(response=testing_df_rf_smote$Cancellation, predictor=pred_rf_balance)
plot(roc_obj)
auc(roc_obj)

#Variable importance:
varImp(rf_classifier_balance,scale=FALSE)
plot(varImp(rf_classifier_balance,scale=FALSE))
varImpPlot(rf_classifier_balance,main="",type=1)

importance(rf_classifier_balance, type=1)

?varImp
