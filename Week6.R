world <- read.csv("C:/Users/tdunmire/Desktop/Foundations of Data Analysis/WorldBankData.csv")

gbr <- world[world$Country.Code == "GBR",]

gbr2000 <- gbr[gbr$year >= 2000 & gbr$year < 2010,]

time <- gbr2000$year - 2000 #vector of years since start
mv <- gbr2000$motor.vehicles
plot(time, mv)

expFit(time, mv)
logisticFit(time, mv)

tripleFit(time, mv) #fits all three models along with R-squared

expFitPred(time, mv, 12) #years since start (so this is 2012)
logisticFitPred(time, mv, 12)

#Prelab - use world bank dataset called "World"


    #First Low income country
lowInc <- world[world$IncomeGroup == "Low income",]
lowInc[1,]

    #What was the rural population in Aruba in 1970
subset(world, Country == "Aruba" & year == 1970, select = rural.population)

    #First year Australia had more than 0 mobile users
aus <- subset(world, Country == "Australia" & mobile.users > 0, select = year)
aus[1,]

    #Subset data for US and select from years since 1990
us <- world[world$Country.Code == "USA",]
us_select <- us[us$year >= 1990,]

us_select$internet.mil <- us_select$internet.users/1000000 #make more workable
us_select$time <- us_select$year - 1990

us_select_10 <- us_select[us_select$time < 10,]

    #Fit expontential and logistic model from SDSFoundations
library(SDSFoundations)

expFit(us_select_10$time, us_select_10$internet.mil)
logisticFit(us_select_10$time, us_select_10$internet.mil)

    #predict number of users in 2006 and find actual amount
e <- expFitPred(us_select_10$time, us_select_10$internet.mil, 16)
l <- logisticFitPred(us_select_10$time, us_select_10$internet.mil, 16)

us_select[us_select$time == 16, c("Country", "year", "internet.mil")]

    #Find residuals
us_select$internet.mil[us_select$time == 16] - e
us_select$internet.mil[us_select$time == 16] - l

expFit(us_select$time, us_select$internet.mil)
logisticFit(us_select$time, us_select$internet.mil)

tripleFit(us_select$time, us_select$internet.mil) #find best fitting model

expFitPred(us_select_10$time, us_select_10$internet.mil, 22)

#Lab - using world bank data as 'world': Find best fitting model for internet usage for Denmark
#and Belarus and then decide if income level has an impact on the speed with which a country
#adopts use of the internet

    #variable that represents proportion of population using internet
world$prop.Internet <- (world$internet.users/world$population) 

    #subset only data from 1990 onward
world_select <- world[world$year >= 1990,]
world_select$time <- world_select$year - 1990

    #subset based on countries of interest
denmark <- world_select[world_select$Country == "Denmark",]
belarus <- world_select[world_select$Country == "Belarus",]

    #exponential and logistic models for denmark
expFit(denmark$time, denmark$prop.Internet)
logisticFit(denmark$time, denmark$prop.Internet)

    #exponential and logistic models for belarus
expFit(belarus$time, belarus$prop.Internet)
logisticFit(belarus$time, belarus$prop.Internet)

    #To solve for time to reach a certain threhold use 
-log(((C/threshold)-1)/a)/log(b)

#Problem Set - Has mobile phone usage in brazil changed since 1995

    #make subset showing years since 1995 in brazil
braz <- world[world$Country == "Brazil",]
braz_select <- braz[braz$year >= 1995,]
braz_select$yearsSince95 <- braz_select$year - 1995
braz_select$mobile.mil <- braz_select$mobile.users/1000000

tripleFit(braz_select$yearsSince95, braz_select$mobile.mil)
logisticFitPred(braz_select$yearsSince95, braz_select$mobile.mil, 30) #predict number in 2025

    #Yellowstone wolf project
years <- c(1,3)
numWolves <- c(25, 45)

wolf <- data.frame(years, numWolves)
linFit(wolf$years, wolf$numWolves)
expFit(wolf$years, wolf$numWolves)

linFitPred(wolf$years, wolf$numWolves, 7)
expFitPred(wolf$years, wolf$numWolves, 7)