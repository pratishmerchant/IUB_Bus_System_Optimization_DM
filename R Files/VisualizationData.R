routeAdata <- read.table("C:\\Users\\Pratish\\Documents\\Assignments\\Data Mining\\Final Group Project\\Output\\DelaysA1.txt", header=FALSE, sep=",")
names(routeAdata) <- c("Unix_Time_Stamp","Bus_ID", "Route_ID","Initial_Delay","Wells_Delay","Jordan_Delay","IMU_Delay","Back_to_Stadium_Delay","Weather","Passenger_Count","Time_Of_Day")

filter_routeAdata <- routeAdata[which (routeAdata$Wells_Delay < 21) ,]


var(routeAdata$Wells_Delay)

routeBdata <- read.table("C:\\Users\\Pratish\\Documents\\Assignments\\Data Mining\\Final Group Project\\Output\\DelaysBbk.txt", header=FALSE, sep=",")
names(routeBdata) <- c("Unix_Time_Stamp","Bus_ID", "Route_ID","Initial_Delay","10_Jordon_Delay","Jordan_3_Delay","10_Jordon_Return_Delay","Back_to_Ficher_Delay","Weather","Passenger_Count","Time_Of_Day")
names(routeBupdated) <- c("Unix_Time_Stamp","Bus_ID", "Route_ID","Load_Delay","10_Jordon_Delay","Jordan_3_Delay","10_Jordon_Return_Delay","Back_to_Ficher_Delay","Weather","Passenger_Count","Time_Of_Day","Day_Of_Week")


routeBupdated <- read.table("C:\\Users\\Pratish\\Documents\\Assignments\\Data Mining\\Final Group Project\\Output\\DelaysB.txt", header=FALSE, sep=",")
names(routeBupdated) <- c("Unix_Time_Stamp","Bus_ID", "Route_ID","Load_Delay","10Jordon_Delay","Jordan3_Delay","10Jordon2_Delay","BckFicher_Delay","Weather","Passenger_Count","Time_Of_Day","Day_Of_Week")
plot(routeBdata$Back_to_Ficher_Delay,type = 'h')

jordon10_mean <- mean(routeBupdated$`10_Jordon_Delay`)
jordon3_mean <- mean(routeBupdated$Jordan_3_Delay)
jordon10R_mean <- mean(routeBupdated$`10_Jordon_Return_Delay`)
intial_delay_mean <- mean(routeBupdated$Initial_Delay)
backToFischer_mean <- mean(routeBupdated$Back_to_Ficher_Delay)




jordon10_var <- var(routeBupdated$`10_Jordon_Delay`)
jordon3_var <-var(routeBupdated$Jordan_3_Delay)
jordon10R_var<-var(routeBupdated$`10_Jordon_Return_Delay`)
intial_delay_var<-var(routeBupdated$Initial_Delay)
backToFischer_var<-var(routeBupdated$Back_to_Ficher_Delay)



install.packages("mvoutlier")
library(mvoutlier)
uni.plot(routeBupdated[,c(4,5,6,7,8)],quan = 1)
uni.plot(clean_data[,c(4,5,6,7,8)],quan = 1)
uni.plot(routeBupdated[,c(4,5)], quan = 1)



mean(routeBupdated[,c(4,5,6,7)]
str (routeBupdated)
clean_data = routeBupdated[(abs(routeBupdated$Initial_Delay)<15 & abs(routeBupdated$`10_Jordon_Delay`)<15&abs(routeBupdated$Jordan_3_Delay)<15&abs(routeBupdated$`10_Jordon_Return_Delay`)<15&abs(routeBupdated$Back_to_Ficher_Delay)<15),]



table(routeBupdated$Initial_Delay,routeBupdated$Passenger_Count)






weather_table <- routeBupdated[,c(4,5,6,7,8,9)]


jordon_delay_wea <- table(routeBupdated$`10_Jordon_Delay`,routeBupdated$Weather)


counts <- table(routeBdata$Weather ,routeBdata$`10_Jordon_Delay`, routeBdata$Jordan_3_Delay )
barplot(jordon_delay_wea, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("red","green","black","darkgreen"),
        legend = rownames(jordon_delay_wea))






Initial_Delay <- mean(clean_data$Initial_Delay)
Jordon10_Delay <- mean(clean_data$10_Jordon_Delay)
Jordan3_Delay <- mean(clean_data$Jordan_3_Delay)
Jordon10_Return_Delay <- mean(clean_data$10_Jordon_Return_Delay)
Back_to_Fischer_Delay <- mean(clean_data$Back_to_Ficher_Delay)


Initial_Var <- var(clean_data$Initial_Delay)
Jordon10_Var <- var(clean_data$`10_Jordon_Delay`)
Jordan3_Var <- var(clean_data$Jordan_3_Delay)
Jordon10_Return_Var <- var(clean_data$`10_Jordon_Return_Delay`)
Back_to_Fischer_Var <- var(clean_data$Back_to_Ficher_Delay)



mean_stops = cbind(Initial_Delay,Jordon10_Delay,Jordan3_Delay,Jordon10_Return_Delay,Back_to_Fischer_Delay)
var = cbind(Initial_Delay,Jordon10_Delay,Jordan3_Delay,Jordon10_Return_Delay,Back_to_Fischer_Delay)

stargazer::stargazer(var,title("Variance of delay at each stop"))





days123 = clean_data[!duplicated(clean_data$Day_of_week),12],

tab = data.frame(),  
  for(i in days123) {
    
    Initial_Delay <- var(clean_data[clean_data$Day_of_week==i,4])
    Wells_Delay <- var(clean_data[clean_data$Day_of_week==i,5])
    Jordan_Delay <- var(clean_data[clean_data$Day_of_week==i,6])
    IMU_Delay <- var(clean_data[clean_data$Day_of_week==i,7])
    Back_to_Stadium_Delay <- var(clean_data[clean_data$Day_of_week==i,8])
    mean_stops = cbind(c(i),Initial_Delay,Wells_Delay,Jordan_Delay,IMU_Delay,Back_to_Stadium_Delay)
    #print(mean_stops)
    tab = rbind(tab,mean_stops)
  }
  
},


C:\Users\Pratish\AppData\Roaming\Skype\My Skype Received Files\

stargazer::stargazer(tab,title("Mean of delay at each stop"),summary=FALSE)



clean_data[clean_data$Day_Of_Week=='Friday',4]


variance <- read.table("C:\\Users\\Pratish\\AppData\\Roaming\\Skype\\My Skype Received Files\\tab", header=TRUE, sep=" ")

library(lattice)