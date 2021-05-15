################################ PACKAGES #####################################
library(smotefamily)
library(tictoc)
library(caret)
library(dplyr)
library(stargazer)
library(pROC)

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

sapply(final_data, class)

################################ SPLITTING DATA - IMBALANCED #####################################
training_df <- final_data[1:66407, ]
testing_df <- final_data[66408:83009, ]
#Relocate the Cancellation column in the original training and testing set
testing_df <- testing_df %>% relocate(Cancellation, .after = last_col())
training_df <- training_df %>% relocate(Cancellation, .after = last_col())
table(testing_df$Cancellation)

training_df_glm = subset(training_df, select = -c(Country,Department))
testing_df_glm = subset(testing_df, select = -c(Country,Department))
table(testing_df_glm$Cancellation)

#Classes needed for modeling
training_df_glm <- transform(
  training_df_glm,
  RoomType=as.factor(RoomType),
  BookingType=as.factor(BookingType),
  Group=as.factor(Group),
  Agent=as.factor(Agent),
  Company=as.factor(Company),
  Cancellation=as.factor(Cancellation),
  ArrivalDay=as.factor(ArrivalDay),
  ArrivalMonth=as.factor(ArrivalMonth)
)
testing_df_glm <- transform(
  testing_df_glm,
  RoomType=as.factor(RoomType),
  BookingType=as.factor(BookingType),
  Group=as.factor(Group),
  Agent=as.factor(Agent),
  Company=as.factor(Company),
  Cancellation=as.factor(Cancellation),
  ArrivalDay=as.factor(ArrivalDay),
  ArrivalMonth=as.factor(ArrivalMonth)
)

################################ SMOTE ###########################
set.seed(100)

#General SMOTE
training_df_smote <- data.frame(training_df)

training_df_smote <- transform(
  training_df_smote,
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


#GLM training SMOTE data
training_df_glm_smote = subset(training_df_smote, select = -c(Country,Department))

#Inspect numbers
table(training_df$Cancellation)
table(training_df_smote$Cancellation)
table(training_df_glm_smote$Cancellation)
#Both SMOTE training sets are identical

################################ IMBALANCED DATA #####################################
#Fit the model
glm_model <- glm(Cancellation ~ ., family = binomial, data = training_df_glm)

#predict on testing set
prob_glm = predict(glm_model, newdata = testing_df_glm, type="response")
pred_glm = rep("0", dim(testing_df_glm)[1])
pred_glm[prob_glm > .5] = 1

#Evaluation
cm_glm <- table(pred_glm,testing_df_glm$Cancellation)
confusionMatrix(cm_glm,mode = "prec_recall", positive = "1")

table(testing_df_glm$Cancellation)

#Summary statistics
summary(glm_model)
stargazer(glm_model)

pred_glm <- as.numeric(pred_glm)
roc_obj <- roc(response=testing_df_glm$Cancellation, predictor=pred_glm)
plot(roc_obj)
auc(roc_obj)

################################ BALANCED DATA - SMOTE #####################################
#Change the datatypes back to factors
training_df_glm_smote <- transform(
  training_df_glm_smote,
  RoomType=as.factor(RoomType),
  BookingType=as.factor(BookingType),
  Group=as.factor(Group),
  Agent=as.factor(Agent),
  Company=as.factor(Company),
  Cancellation=as.factor(Cancellation),
  ArrivalDay=as.factor(ArrivalDay),
  ArrivalMonth=as.factor(ArrivalMonth)
)
sapply(training_df_glm_smote,class)

#Fit the model
glm_model_balance <- glm(Cancellation ~ ., family = binomial, data = training_df_glm_smote)

#predict on testing set
prob_glm_balance = predict(glm_model_balance, newdata = testing_df_glm, type="response")
pred_glm_balance = rep("0", dim(testing_df_glm)[1])
pred_glm_balance[prob_glm_balance > .5] = 1

#Evaluation
cm_glm_balance <- table(pred_glm_balance,testing_df_glm$Cancellation)
confusionMatrix(cm_glm_balance, mode = "prec_recall", positive = "1")

summary(glm_model_balance)
stargazer(glm_model_balance)
stargazer(glm_model,glm_model_balance)


pred_glm <- as.numeric(pred_glm_balance)
roc_obj <- roc(response=testing_df_glm$Cancellation, predictor=pred_glm)
plot(roc_obj)
auc(roc_obj)
