routeAdata <- read.table("C:\\Users\\Pratish\\Documents\\Assignments\\Data Mining\\Final Group Project\\Output\\DelaysA1.txt", header=FALSE, sep=",")
names(routeAdata) <- c("Unix_Time_Stamp","Bus_ID", "Route_ID","Initial_Delay","Wells_Delay","Jordan_Delay","IMU_Delay","Back_to_Stadium_Delay","Weather","Passenger_Count","Time_Of_Day")
routeAdata
names(routeAextendedData) <- c("Unix_Time_Stamp","Bus_ID","Route_ID", "Leave_Stadium_Delay","Wells_Delay","Jordan_Delay","IMU_Delay","Back_to_Stadium_Delay","Snow_Rain","Temperature", "Visibility","Wind","Passenger_Count", "Week" , "Month" , "Time_of_Day")
routeAextendedData <- read.table("C:\\Users\\Pratish\\Documents\\Assignments\\Data Mining\\Final Group Project\\Output\\DelaysA1.txt", header=FALSE, sep=",")

testpredictdata <- read.table("C:\\Users\\Pratish\\Documents\\Assignments\\Data Mining\\Final Group Project\\Output\\UseTest.txt", header=FALSE, sep=",") 
onlyAtest <- testpredictdata[testpredictdata$V2 == "A",]
names(testpredictdata) <- c("IMU_Delay","Bus_ID","Initial_Delay","Weather","Passenger_Count", "Time_Of_Day")
names(testpredictdata) <- c("Back_To_Stadium_Delay","Bus_ID","Initial_Delay","Weather","Passenger_Count", "Time_Of_Day")
testpredictdata$IMU_Delay <- 0



table(routeAextendedData$Passenger_Count)

library(rpart) 
library(randomForest)   
library('e1071')
install.packages('party')
library(party)


sub <- sample(nrow(routeAdata), floor(nrow(routeAdata) * 0.9))
extendedsub <- sample(nrow(routeAextendedData), floor(nrow(routeAextendedData) * 0.9))


training <- routeAdata[sub, ]
names(training) <- c("Unix_Time_Stamp","Bus_ID","Route_ID", "Initial_Delay","Wells_Delay","Jordan_Delay","IMU_Delay","Back_to_Stadium_Delay","Weather","Passenger_Count","Time_Of_Day")
testing <- routeAdata[-sub, ]
names(testing) <- c("Unix_Time_Stamp","Bus_ID","Route_ID", "Initial_Delay","Wells_Delay","Jordan_Delay","IMU_Delay","Back_to_Stadium_Delay","Weather","Passenger_Count","Time_Of_Day")

#Sample code
#my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=train,method="class")
table(routeAdata)
names(routeAdata)
names(routeAextendedData)
#,control = rpart.control(minsplit = 50, cp=0.01)
#decision tree
psngr_cnt_tree <- rpart(`Passenger_Count` ~ `Bus_ID` + `Leave_Stadium_Delay` + `Wells_Delay` + `Jordan_Delay` + `IMU_Delay` + `Back_to_Stadium_Delay` + ` Visibility` +`Snow_Rain` + `Temperature` +   + `Wind`,data=training,method="anova")


##Random Forest
psngr_cnt_tree_forest <- randomForest(`Wells_Delay` ~ `Bus_ID` + `Leave_Stadium_Delay` + `Passenger_Count` + `Jordan_Delay` + `IMU_Delay` + `Back_to_Stadium_Delay` +`Snow_Rain` + `Temperature` + `Wind`,data = training, importance = TRUE, ntree = 1000)

##`Leave_Stadium_Delay`  + `Jordan_Delay` + `IMU_Delay` + `Back_to_Stadium_Delay`

leave_stadium_delay_tree_forest <- randomForest(`Leave_Stadium_Delay` ~ `Bus_ID` + `Wells_Delay`  + `Jordan_Delay` + `IMU_Delay` + `Back_to_Stadium_Delay` +`Snow_Rain` + `Temperature` + `Visibility` + `Wind`+ `Passenger_Count`,data = routeAextendedData, importance = TRUE, ntree = 1000)
#Old Conditions
wells_delay_tree_forest <- randomForest(`Wells_Delay` ~ `Bus_ID` + `Snow_Rain` + `Temperature` + `Visibility` + `Wind`+ `Passenger_Count`,data = training, importance = TRUE, ntree = 1000)




# New Conditions ##----------------------------------------------------------------------------------------------------------------
wells_delay_tree_forest <- randomForest(`Wells_Delay` ~ `Bus_ID` + `Initial_Delay` + `Weather`+`Time_Of_Day` + `Passenger_Count`,data = routeAdata, importance = TRUE, mtry = 3 ,ntree = 1000)
jordon_delay_tree_forest<- randomForest(`Jordan_Delay` ~ `Bus_ID` + `Initial_Delay` + `Weather`+`Time_Of_Day` + `Passenger_Count`,data = routeAdata, importance = TRUE, mtry = 3 ,ntree = 1000)
imu_delay_tree_forest <- randomForest(`IMU_Delay` ~ `Bus_ID` + `Initial_Delay` + `Weather`+`Time_Of_Day` + `Passenger_Count`,data = routeAdata, importance = TRUE, mtry = 3 ,ntree = 1000)
back_stadium_delay_tree_forest <- randomForest(`Back_to_Stadium_Delay` ~ `Bus_ID` + `Initial_Delay` + `Weather`+`Time_Of_Day` + `Passenger_Count`,data = routeAdata, importance = TRUE, mtry = 3 ,ntree = 1000)
########------------------------------------------------------------------------------------------------###################

imu_delay_tree_forest <- randomForest(`IMU_Delay` ~ `Bus_ID` + `Leave_Stadium_Delay`  + `Jordan_Delay` + `Wells_Delay` + `Back_to_Stadium_Delay` +`Snow_Rain` + `Temperature` + `Visibility` + `Wind`+ `Passenger_Count`,data = training, importance = TRUE, ntree = 1000)
back_stadium_delay_tree_forest <- randomForest(`Back_to_Stadium_Delay` ~ `Bus_ID` + `Leave_Stadium_Delay`  + `Jordan_Delay` + `Wells_Delay` + `IMU_Delay` +`Snow_Rain` + `Temperature` + `Visibility` + `Wind`+ `Passenger_Count`,data = training, importance = TRUE, ntree = 1000)




#random forest prediction##################################################################
wells_delay_pred_forest <- predict(wells_delay_tree_forest, testpredictdata )
jordon_delay_pred_forest<- predict(jordon_delay_tree_forest, testpredictdata )
imu_delay_pred_forest <- predict (imu_delay_tree_forest , testpredictdata)
back_to_stadium_delay_pred_forest <- predict (back_stadium_delay_tree_forest , testpredictdata)




#random forest Results  -----------------------------------------------------------------###############
results_wells_delay <- data.frame( wells_delay_pred_forest)
results_jordon_delay <- data.frame( jordon_delay_pred_forest)
results_imu_delay <- data.frame( imu_delay_pred_forest)
results_back_stadium_delay <- data.frame(back_to_stadium_delay_pred_forest )


results_compare <- data.frame(Actual_Count = testing$Wells_Delay, Predicted_Count = psngr_cnt_pred_forest)
results_leave_stadium_delay <- data.frame(Actual_Count = testing$Leave_Stadium_Delay, Predicted_Count = leave_stadium_delay_pred_forest)

results_jordon_delay <- data.frame(Actual_Count = testpredictdata$Jordan_Delay, Predicted_Count = jordon_delay_pred_forest)
results_imu_delay<- data.frame(Actual_Count = testing$IMU_Delay, Predicted_Count = imu_delay_pred_forest)
results_back_stadium_delay <- data.frame(Actual_Count = testing$Back_to_Stadium_Delay, Predicted_Count = back_stadium_delay_pred_forest)


testpredictdata$Wells_Delay <- results_wells_delay$wells_delay_pred_forest
testpredictdata$Jordon_Delay <- results_jordon_delay$jordon_delay_pred_forest
testpredictdata$IMU_Delay <- results_imu_delay$imu_delay_pred_forest
testpredictdata$Back_To_Stadium_Delay <- results_back_stadium_delay$back_to_stadium_delay_pred_forest




#####Writing output to file 
write.table(testpredictdata, "C:\\Users\\Pratish\\Documents\\Assignments\\Data Mining\\Final Group Project\\Output\\Wells_Pred_Delay.txt", sep="," , row.names = FALSE)
write.table(testpredictdata, "C:\\Users\\Pratish\\Documents\\Assignments\\Data Mining\\Final Group Project\\Output\\Jordan_Pred_Delay.txt", sep="," , row.names = FALSE)
write.table(testpredictdata, "C:\\Users\\Pratish\\Documents\\Assignments\\Data Mining\\Final Group Project\\Output\\IMU_Pred_Delay.txt", sep="," , row.names = FALSE)
write.table(testpredictdata, "C:\\Users\\Pratish\\Documents\\Assignments\\Data Mining\\Final Group Project\\Output\\Back_To_Stadium_Pred_Delay.txt", sep="," , row.names = FALSE)
str(testpredictdata)







######### Extra code 

#party tree predictions
set.seed(415)
wells_delay_tree_forest <- cforest(`Wells_Delay` ~ `Bus_ID` + `Snow_Rain` + `Temperature` + `Visibility` + `Wind`+ `Passenger_Count`,data = training, controls=cforest_unbiased(ntree=2000, mtry=6))

##Getting Passenger count in range
routeAextendedData["Passenger_Count_Range"] <- NULL
routeAextendedData$Passenger_Count_Range[routeAextendedData$Passenger_Count < 50] <- "Less"
routeAextendedData$Passenger_Count_Range[routeAextendedData$Passenger_Count >= 50 & routeAextendedData$Passenger_Count <100] <- "Medium"
routeAextendedData$Passenger_Count_Range[routeAextendedData$Passenger_Count >= 100 & routeAextendedData$Passenger_Count <150] <- "Average"
routeAextendedData$Passenger_Count_Range[routeAextendedData$Passenger_Count >= 150 & routeAextendedData$Passenger_Count <250] <- "Heavy"
routeAextendedData$Passenger_Count_Range[routeAextendedData$Passenger_Count >= 250 ] <- "Full"



is.na(routeAextendedData)
mean(routeAextendedData, na.rm = TRUE)

jordon_delay_tree_forest <- randomForest(`Jordan_Delay` ~ `Bus_ID` + `Leave_Stadium_Delay`  + `Wells_Delay` + `IMU_Delay` + `Back_to_Stadium_Delay` +`Snow_Rain` + `Temperature` + `Visibility` + `Wind`+ `Passenger_Count`,data = training, importance = TRUE, ntree = 1000)


## SVM 

psngr_cnt_tree_svm <- svm(`Passenger_Count` ~ `Bus_ID` + `Leave_Stadium_Delay` + `Wells_Delay` + `Jordan_Delay` + `IMU_Delay` + `Back_to_Stadium_Delay` + ` Visibility` +`Snow_Rain` + `Temperature` +   + `Wind`, data=training,type= 'C',kernel='sigmoid',probability = TRUE)



# Predicting values-----------------------------------
psngr_cnt_pred <- predict(psngr_cnt_tree , testing , type = "vector")
psngr_cnt_pred_forest <- predict(psngr_cnt_tree , testing )



extended_training <- routeAextendedData[extendedsub, ]
names(extended_training) <- c("Unix_Time_Stamp","Bus_ID","Route_ID", "Initial_Delay","Wells_Delay","Jordan_Delay","IMU_Delay","Back_to_Stadium_Delay","Snow_Rain","Temperature", "Visibility","Wind","Passenger_Count", "Week" , "Month" , "Time_of_Day","Passenger_Count_Range")
extended_testing <- routeAextendedData[-extendedsub, ]
names(extended_testing) <- c("Unix_Time_Stamp","Bus_ID","Route_ID", "Initial_Delay","Wells_Delay","Jordan_Delay","IMU_Delay","Back_to_Stadium_Delay","Snow_Rain","Temperature", "Visibility","Wind","Passenger_Count", "Week" , "Month" , "Time_of_Day","Passenger_Count_Range")

leave_stadium_delay_pred_forest <- predict(leave_stadium_delay_tree_forest, testing )

wells_delay_pred_forest <- predict(wells_delay_tree_forest, onlyAtest )
#party tree prediction
wells_delay_pred_forest <- predict(wells_delay_tree_forest, extended_testing, OOB=TRUE, type = "response")

jordon_delay_pred_forest <- predict(jordon_delay_tree_forest, testing )
imu_delay_pred_forest <- predict(imu_delay_tree_forest, testing )
back_stadium_delay_pred_forest <- predict(back_stadium_delay_tree_forest, testing )

psngr_cnt_pred_svm <- predict(psngr_cnt_tree_svm, testing, probability = TRUE)


table(wells_delay_pred_forest)
#party tree prediction
results_wells_delay <- data.frame(Actual_Count = extended_testing$Wells_Delay, Predicted_Count = wells_delay_pred_forest)