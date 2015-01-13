#Notes from Tutorial Videos

    #use bull riders dataset, make scatterplots and then calculate correlation

plot(bull$YearsPro, bull$BuckOuts, xlab = "Years Pro", 
     ylab = "Buckouts", main = "Plot of Years Pro vs Buckouts")
abline(lm(bull$BuckOuts ~ bull$YearsPro)) #not a very strong linear relationship

plot(bull$Events, bull$BuckOuts, xlab = "No of Events", ylab = "Buckouts", 
     main = "Plot of Events vs Buckouts")
abline(lm(bull$Buckouts ~ bull$Events))

cor(bull$YearsPro, bull$Buckouts) #actually calculates correlation
cor(bull$Events, bull$Buckouts)
myvars <- c("YearsPro", "Events", "BuckOuts")
cor(bull[,myvars]) #makes a correlation matrix

#Pre-Lab - Is there a linear relationship between how often a rider places in the 
#Top 10 and how often he stays on his bull

bull[which.min(bull$BuckOuts),] #get row that has minimum buckouts

    #Visualize Ride Percentage
hist(bull$RidePer)
fivenum(bull$RidePer)
mean(bull$RidePer)
sd(bull$RidePer)

    #Visualize Top 10
hist(bull$Top10)
fivenum(bull$Top10)
mean(bull$Top10)
sd(bull$Top10)

    #Create scatter plot
plot(bull$RidePer, bull$Top10)
abline(lm(bull$Top10 ~ bull$RidePer))

    #Calculate correlation coefficient and matrix
cor(bull$RidePer, bull$Top10)

vars <- c("Top10", "RidePer")
cor(bull[,vars])

    #Get specific rider
bull[which(bull$Top10 == 5 & bull$RidePer == 0.53),]

#Lab - Which variable has the strongest linear relationship with Earnings: 
#Ride Percentage or Cup Points

hist(bull$Earnings)
mean(bull$Earnings)
median(bull$Earnings)
max(bull$Earnings)

plot(bull$RidePer, bull$Earnings)
cor(bull$RidePer, bull$Earnings)

plot(bull$CupPoints, bull$Earnings)
abline(lm(bull$Earnings ~ bull$CupPoints))
cor(bull$CupPoints, bull$Earnings)

    #Identify extreme case
which(bull$Earnings == max(bull$Earnings))

    #Subset without outlier
nooutlier <- bull[-1,]
cor(nooutlier$RidePer, nooutlier$Earnings)
cor(nooutlier$CupPoints, nooutlier$Earnings)

#Problem Set
    #Can ride a bull multiple times at the same event
bull$RidesPerEvent <- bull$Rides/bull$Events

hist(bull$RidesPerEvent)
fivenum(bull$RidesPerEvent)

plot(bull$RidesPerEvent, bull$Place)
abline(lm(bull$Place ~ bull$RidesPerEvent))

cor(bull$RidesPerEvent, bull$Place)