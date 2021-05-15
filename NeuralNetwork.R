################################ PACKAGES #####################################
library(smotefamily)
library(tictoc)
library(caret)
library(dplyr)
library(stargazer)
library(pROC)
library(neuralnet)
library(nnet)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')


################################ GET DATA - GENERAL #####################################
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

#Normalize data - Min-Max normalization
#final_data_nn <- data.frame(final_data)
final_data_nn <- subset(final_data, select = -c(Country))
final_data_nn$NoChildren <- (final_data_nn$NoChildren - min(final_data_nn$NoChildren)) / (max(final_data_nn$NoChildren) - min(final_data_nn$NoChildren))
final_data_nn$NoAdults <- (final_data_nn$NoAdults - min(final_data_nn$NoAdults)) / (max(final_data_nn$NoAdults) - min(final_data_nn$NoAdults))
final_data_nn$Price <- (final_data_nn$Price - min(final_data_nn$Price)) / (max(final_data_nn$Price) - min(final_data_nn$Price))
final_data_nn$NoDays <- (final_data_nn$NoDays - min(final_data_nn$NoDays)) / (max(final_data_nn$NoDays) - min(final_data_nn$NoDays))
final_data_nn$LeadTime <- (final_data_nn$LeadTime - min(final_data_nn$LeadTime)) / (max(final_data_nn$LeadTime) - min(final_data_nn$LeadTime))

#Create Dummy Variables in final_data_nn
#final_data_nn_matrix <- model.matrix(~RoomType+NoChildren+NoAdults+Price+BookingType+NoDays+Department+LeadTime+ArrivalDay+ArrivalMonth+Group+Agent+Company+Cancellation, data = final_data_nn)
#colnames(final_data_nn_matrix)

#Change to dataframe
#final_data_nn <- as.data.frame(final_data_nn_matrix)

#Remove the intercept column
#final_data_nn <- final_data_nn[,-1]

#Change smote final df
final_data_smote <- data.frame(final_data_nn)
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
  Department=as.integer(Department)
)

#Change variable types:
#sapply(final_data_nn, class)

final_data_nn <- final_data_nn %>% relocate(Cancellation, .after = last_col())
final_data_smote <- final_data_smote %>% relocate(Cancellation, .after = last_col())


################################ SPLITTING DATA - IMBALANCED #####################################
#training_df <- final_data[1:66407, ]
#testing_df <- final_data[66408:83009, ]

final_data_nn <- transform(
  final_data_nn,
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

#Split data
training_df_nn <- final_data_nn[1:66407, ]
testing_df_nn <- final_data_nn[66408:83009, ]
sapply(training_df_nn, class)

################################ SMOTE ###########################
set.seed(100)

#General SMOTE
training_df_smote <- final_data_smote[1:66407, ]
testing_df_smote <- final_data_smote[66408:83009, ]

sapply(training_df_smote, class)

#Perform SMOTE oversamplong
smote_training = SMOTE(training_df_smote[-14], training_df_smote[14], K = 10, dup_size = 0)
#Save new training data
training_df_smote = smote_training$data
#Change the column name
colnames(training_df_smote)[14] <- c("Cancellation")
#Set Cancellation response as numeric
training_df_smote <- transform(training_df_smote,Cancellation=as.numeric(Cancellation))
#Round all values except pric
#training_df_smote[,1:14] <-round(training_df_smote[,1:14],0)
# training_df_smote[,18:19] <-round(training_df_smote[,18:19],0)
# training_df_smote[,21:22] <-round(training_df_smote[,21:22],0)
# training_df_smote[,24:44] <-round(training_df_smote[,24:44],0)
training_df_smote[,-4] <-round(training_df_smote[,-4],0)

#Inspect numbers
table(training_df_nn$Cancellation)
table(training_df_smote$Cancellation)
table(testing_df_nn$Cancellation)

training_df_smote <- transform(
  training_df_smote,
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

testing_df_smote <- transform(
  testing_df_smote,
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

################################ IMBALANCED DATA #####################################
sapply(training_df_nn, class)
sapply(testing_df_nn, class)
#training_df_nn <- lapply(training_df_nn, as.integer)
#training_df_nn <- transform(training_df_nn,Cancellation=as.factor(Cancellation))
#training_df_nn$Cancellation <- ifelse(training_df_nn$Cancellation == 1, TRUE, FALSE)

#Train model
n <- names(training_df_nn)
f <- as.formula(paste("Cancellation ~", paste(n[!n %in% "Cancellation"], collapse = " + ")))

##NUMBER OF NEURONS
nn <- nnet(f,training_df_nn,size=1)
plot.nnet(nn)
pred_nn <- predict(nn,testing_df_nn,type="class")
cm_nn <- table(testing_df_nn$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_nn$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

nn <- nnet(f,training_df_nn,size=3)
pred_nn <- predict(nn,testing_df_nn,type="class")
cm_nn <- table(testing_df_nn$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_nn$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

nn <- nnet(f,training_df_nn,size=5)
pred_nn <- predict(nn,testing_df_nn,type="class")
cm_nn <- table(testing_df_nn$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_nn$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

nn <- nnet(f,training_df_nn,size=10)
pred_nn <- predict(nn,testing_df_nn,type="class")
cm_nn <- table(testing_df_nn$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_nn$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)


##MAX ITERATION
nn <- nnet(f,training_df_nn,size=5,maxit=200)
pred_nn <- predict(nn,testing_df_nn,type="class")
cm_nn <- table(testing_df_nn$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_nn$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

nn <- nnet(f,training_df_nn,size=5,maxit=300)
pred_nn <- predict(nn,testing_df_nn,type="class")
cm_nn <- table(testing_df_nn$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_nn$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

nn <- nnet(f,training_df_nn,size=5,maxit=400)
pred_nn <- predict(nn,testing_df_nn,type="class")
cm_nn <- table(testing_df_nn$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_nn$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

nn <- nnet(f,training_df_nn,size=5,maxit=500)
pred_nn <- predict(nn,testing_df_nn,type="class")
cm_nn <- table(testing_df_nn$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_nn$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

######BEST MODEL - IMBALANCED DATA #######
nn <- nnet(f,training_df_nn,size=5,maxit=200)
pred_nn <- predict(nn,testing_df_nn,type="class")
cm_nn <- table(testing_df_nn$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_nn$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)


################################ BALANCED DATA #####################################
sapply(training_df_smote, class)

#Train model
n <- names(training_df_smote)
f <- as.formula(paste("Cancellation ~", paste(n[!n %in% "Cancellation"], collapse = " + ")))

##NUmber of Neurons
nn <- nnet(f,training_df_smote,size=1)
pred_nn <- predict(nn,testing_df_smote,type="class")
cm_nn <- table(testing_df_smote$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_smote$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

nn <- nnet(f,training_df_smote,size=3)
pred_nn <- predict(nn,testing_df_smote,type="class")
cm_nn <- table(testing_df_smote$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_smote$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

nn <- nnet(f,training_df_smote,size=5, maxit=100)
pred_nn <- predict(nn,testing_df_smote,type="class")
cm_nn <- table(testing_df_smote$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_smote$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

nn <- nnet(f,training_df_smote,size=10)
pred_nn <- predict(nn,testing_df_smote,type="class")
cm_nn <- table(testing_df_smote$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_smote$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

##MAX ITERATIONS
nn <- nnet(f,training_df_smote,size=5,maxit=200)
pred_nn <- predict(nn,testing_df_smote,type="class")
cm_nn <- table(testing_df_smote$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_smote$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

nn <- nnet(f,training_df_smote,size=5,maxit=300)
pred_nn <- predict(nn,testing_df_smote,type="class")
cm_nn <- table(testing_df_smote$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_smote$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

nn <- nnet(f,training_df_smote,size=5,maxit=400)
pred_nn <- predict(nn,testing_df_smote,type="class")
cm_nn <- table(testing_df_smote$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_smote$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

nn <- nnet(f,training_df_smote,size=5,maxit=500)
pred_nn <- predict(nn,testing_df_smote,type="class")
cm_nn <- table(testing_df_smote$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_smote$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)

######BEST MODEL - BALANCED DATA########
nn <- nnet(f,training_df_smote,size=5, maxit=100)
pred_nn <- predict(nn,testing_df_smote,type="class")
cm_nn <- table(testing_df_smote$Cancellation, pred_nn)
cm_nn
confusionMatrix(cm_nn,mode = "prec_recall",positive = "1")
pred_nn <- as.numeric(pred_nn)
roc_obj <- roc(response=testing_df_smote$Cancellation, predictor=pred_nn)
plot(roc_obj)
auc(roc_obj)


varImp(rf_classifier_balance)
varImpPlot(rf_classifier_balance, type=2,main="")
