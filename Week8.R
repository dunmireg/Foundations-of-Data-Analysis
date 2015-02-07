#Week 8 - Hypothesis Testing One Group Means

#using bull rider data 

bull <- read.csv("C:/Users/tdunmire/Desktop/Foundations of Data Analysis/BullRiders.csv")

#Pre-Lab: Average American adult male weighs 190 lbs, do bull riders weight the same

mean(bull$Weight)
sd(bull$Weight)

hist(bull$Weight, main = "Histogram of Bull Rider Weights", xlab = "Weight (lbs)")

t.test(bull$Weight, mu = 190)

#Lab - Do professional bull riders stay on their bulls at least 50% of the time?
    #Hint: use a two-tailed t test

mean(bull$RidePer)
sd(bull$RidePer)

t.test(bull$RidePer, mu = 0.50) #looks like less than 50%

#Problem Set

    #Make earnings per event variable
bull$earningPerEvent <- bull$Earnings/bull$Events
hist(bull$earningPerEvent) #not normal

logEarnings <- log(bull$earningPerEvent)
hist(log(bull$earningPerEvent)) #approximately normal
mean(logEarnings)

t.test(logEarnings, mu = mean(logEarnings))

    #Turn the confidence intervals back into exponents
exp(8.53)
exp(9.00)

#Students collected 8 random bags of chips, getting the vector below of their 
#weights. They want to test the hypothesis the mean wieght is 28.5 grams

weights <- c(29.4, 29.0, 28.4, 28.8, 28.9, 29.3, 28.5, 28.2)
mean(weights)
sd(weights)

t.test(weights, mu = 28.5)

#Frogs can tolerate calcium concentrations up to 91 mg/L. We measure 25 random
#samples with a mean of 93.6 mg/L and sd of 7.8mg/L

xbar <- 93.6
s <- 7.8

t <- (xbar - 91)/(s/sqrt(25))

