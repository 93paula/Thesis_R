################################ FEATURES #####################################
reservations_data$Arrival = as.Date(reservations_data$Arrival, format = "%Y-%m-%d")

reservations_data$Departure = as.Date(reservations_data$Departure, format = "%Y-%m-%d")

reservations_data$Created = as.Date(reservations_data$Created, format = "%Y-%m-%d")

reservations_data$CancellationDate = as.Date(reservations_data$CancellationDate, format = "%Y-%m-%d")

reservations_data <- reservations_data[order(reservations_data$Arrival),]

reservations_data$CancellationTime = reservations_data$Arrival - reservations_data$CancellationDate
reservations_data = transform(reservations_data, CancellationTime = as.numeric(CancellationTime))

reservations_data$LeadTime = reservations_data$Arrival - reservations_data$Created
reservations_data = transform(reservations_data, LeadTime = as.numeric(LeadTime))

reservations_data$Cancellation = ifelse(is.na(reservations_data$CancellationDate)==TRUE,0,1)
reservations_data$Cancellation <- as.character(reservations_data$Cancellation)

reservations_data$ArrivalDay = weekdays(as.Date(reservations_data$Arrival))
reservations_data$ArrivalDay[reservations_data$ArrivalDay == 'mandag'] <- 'monday'
reservations_data$ArrivalDay[reservations_data$ArrivalDay == 'tirsdag'] <- 'tuesday'
reservations_data$ArrivalDay[reservations_data$ArrivalDay == 'onsdag'] <- 'wednesday'
reservations_data$ArrivalDay[reservations_data$ArrivalDay == 'torsdag'] <- 'thursday'
reservations_data$ArrivalDay[reservations_data$ArrivalDay == 'fredag'] <- 'friday'
reservations_data$ArrivalDay[reservations_data$ArrivalDay == 'lørdag'] <- 'saturday'
reservations_data$ArrivalDay[reservations_data$ArrivalDay == 'søndag'] <- 'sunday'

reservations_data$ArrivalMonth = months(as.Date(reservations_data$Arrival), TRUE)

reservations_data$Quarter = quarters(as.Date(reservations_data$Arrival))

reservations_data$Group = ifelse(reservations_data$GroupID == 0,0,1)
reservations_data$Agent = ifelse(reservations_data$AgentID == 0,0,1)
reservations_data$Company = ifelse(reservations_data$CompanyID == 0,0,1)


################################ DATA CLEANING #####################################
##Pris - Remove negative price
#negative_price <- subset(reservations_data, reservations_data$Price < 0) #219 obs
reservations_data <- subset(reservations_data, reservations_data$Price >= 0)

##Afdeling 
#Remove rows with NA afdeling (2 obs)
reservations_data <- subset(reservations_data, is.na(reservations_data$Department) == FALSE)
#Remove observations after 03-10-2017 for TH department due to renovations
reservations_data <- subset(reservations_data, reservations_data$Department != "TH")
#reservations_data <- reservations_data[!(reservations_data$Department == "TH" & reservations_data$Arrival >= '2017-10-03'),]

##Værelsestype
room_types <- c('D/T', 'JUSU', 'FAM', 'RUN', 'RUNF', 'R272', 'STUD', 'EXSU', 'XS', 'S 1', 'M', 'Msea', 'L', 'Fami', 'Brid', 'DB2', 'FAML', '4R', '6R', '4B', 'DB', 'FA')
reservations_data <- subset(reservations_data, (reservations_data$RoomType %in% room_types) == TRUE)

##Bestillingsgruppe
reservations_data$BookingType[is.na(reservations_data$BookingType)] <- 0
#table(reservations_data$BookingType)
booking_types <- c('01', '02', '03', '04', '05')
reservations_data <- subset(reservations_data, (reservations_data$BookingType %in% booking_types) == TRUE)

#Number of days no observations
reservations_data <- subset(reservations_data, reservations_data$NoDays > 0)

#Time booking
#negative_TimeBooking <- subset(reservations_data, reservations_data$LeadTime < 0)
reservations_data <- subset(reservations_data, reservations_data$LeadTime > 0)

#CancellationTime
reservations_data <- reservations_data[(reservations_data$CancellationTime > 0 | is.na(reservations_data$CancellationTime) == TRUE), ]

##Land
reservations_data$Country = toupper(reservations_data$Country)
reservations_data <- subset(reservations_data, is.na(reservations_data$Country)==FALSE)
reservations_data$Country[reservations_data$Country == 'D'] <- 'DE'
reservations_data$Country[reservations_data$Country == 'I'] <- 'IT'
reservations_data$Country[reservations_data$Country == 'F'] <- 'FR'
reservations_data$Country[reservations_data$Country == 'E'] <- 'ES'
reservations_data$Country[reservations_data$Country == 'FIN'] <- 'FI'
reservations_data$Country[reservations_data$Country == 'CAN'] <- 'CA'
reservations_data$Country[reservations_data$Country == 'B'] <- 'BE'

##Ankomst og Afrejse
reservations_data <- subset(reservations_data, reservations_data$Arrival != reservations_data$Departure)

##Antal gæster
reservations_data <- subset(reservations_data, reservations_data$NoAdults + reservations_data$NoChildren > 0)
reservations_data <- subset(reservations_data, reservations_data$NoAdults > 0)

##Company
reservations_data <- subset(reservations_data, is.na(reservations_data$Company) == FALSE)


#setwd()
write.csv(reservations_data, file = "reservations_data.csv", row.names =FALSE)
reservations_data <- data.frame
reservations_data = read.csv('reservations_data.csv')

################################ FINAL DATASET FOR ANALYSIS #####################################
#final_data <- data.frame
final_data <- data.frame(reservations_data)
final_data = subset(final_data, select = -c(CancellationDate, Created, ReservationNo, CancellationTime, Arrival, Departure, GroupID, AgentID, CompanyID))

#Check for NA values
subset(final_data, is.na(final_data$RoomType) == TRUE)
subset(final_data, is.na(final_data$NoChildren) == TRUE)
subset(final_data, is.na(final_data$NoAdults) == TRUE)
subset(final_data, is.na(final_data$Price) == TRUE)
subset(final_data, is.na(final_data$BookingType) == TRUE)
subset(final_data, is.na(final_data$Country) == TRUE)
subset(final_data, is.na(final_data$NoDays) == TRUE)
subset(final_data, is.na(final_data$Department) == TRUE)
subset(final_data, is.na(final_data$Group) == TRUE)
subset(final_data, is.na(final_data$Agent) == TRUE)
subset(final_data, is.na(final_data$Company) == TRUE)
subset(final_data, is.na(final_data$LeadTime) == TRUE)
subset(final_data, is.na(final_data$Cancellation) == TRUE)
subset(final_data, is.na(final_data$ArrivalDay) == TRUE)
subset(final_data, is.na(final_data$ArrivalMonth) == TRUE)


################################ DESCRIPTIVE STATISTICS #####################################
#Summary
sapply(final_data, class)

final_data <- transform(
  final_data,
  RoomType=as.factor(RoomType),
  BookingType=as.factor(BookingType),
  Department=as.factor(Department),
  Cancellation=as.factor(Cancellation),
  ArrivalDay=as.factor(ArrivalDay),
  ArrivalMonth=as.factor(ArrivalMonth),
  Country=as.factor(Country),
  Quarter=as.factor(Quarter),
  Group=as.factor(Group),
  Agent=as.factor(Agent),
  Company=as.factor(Company)
)

str(final_data)
summary(final_data)
stargazer(final_data)

summary(cancellation_data)
table(final_data$RoomType)
table(final_data$BookingType)
table(final_data$Country)
table(final_data$Department)
table(final_data$Group)
table(final_data$Agent)
table(final_data$Company)
table(final_data$ArrivalDay)
table(final_data$ArrivalMonth)
table(final_data$Cancellation)
table(final_data$Quarter)







