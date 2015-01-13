#TUTORIAL

library(SDSFoundations)

#Using AnimalData.csv

animaldata <- AnimalData
head(animaldata) #displays summary with variables and first few values

#Very basic summary by Sex

table(animaldata$Sex)
plot(animaldata$Sex, main = "Bar Chart of Animal Genders", xlab = "Animal Gender", 
     ylab = "Frequency") #add features separated by comma

#To get info for a categorical variable let's use a histogram
hist(animaldata$Age.Intake, main = "Histogram of Intake Ages", 
     xlab = "Age at Intake")

#split data set into male and female sets then make a histogram for each

femaleage <- animaldata$Age.Intake[animaldata$Sex == "Female"]
maleage <- animaldata$Age.Intake[animaldata$Sex == "Male"]

hist(femaleage, main = "Histogram of Female Ages", xlab = "Age at Intake of 
     Female Animals")
hist(maleage, main = "Histogram of Male Ages", xlab = "Age at Intake of 
     Male Animals")

#can adjust the number of bins that the histogram gives you with the 'breaks' 
#argument

hist(maleage, main = "Histogram of Male Ages", xlab = "Age at Intake of 
     Male Animals", breaks = 5)

#'Which' allows you to pull out a specific case

max(maleage)
which(animaldata$Age.Intake == 17) #row 415
animaldata[415,]

#mean(), median(), sd(), and fivenum() all useful functions

#PRE-LAB
library(SDSFoundations)
animaldata <- AnimalData

table(animaldata$Outcome.Type) #Find number of animals adopted
adopted <- animaldata[animaldata$Outcome.Type == "Adoption",] #Pull out adopted animals

daystoadopt <- adopted$Days.Shelter #Pull days in shelter for adopted animals

#Visualize

hist(daystoadopt)
fivenum(daystoadopt)
mean(daystoadopt)
sd(daystoadopt)
which(animaldata$Days.Shelter == max(daystoadopt))
longest <- animaldata[425,]

#LAB

library(SDSFoundations)
animaldata <- AnimalData

#1). Create table showing how many adult cats and dogs are in data set
adult <- animaldata$Age.Intake >= 1 #adult is defined as at least one year old
adults <- animaldata[adult,]
table(adults$Animal.Type)

#2). Make a histogram of weight for both adult dogs and cats
cats <- adults$Animal.Type == "Cat"
dogs <- adults$Animal.Type == "Dog"
dogWeight <- adults[dogs,]$Weight
hist(dogWeight)
catWeight <- adults[cats,]$Weight
hist(catWeight)

#3). Calculate the appropriate measures of center and spread for each distribution
mean(catWeight)
sd(catWeight)

#4). Z score for a 13-pound cat
catMean <- mean(catWeight)
sdCat <- sd(catWeight)
z <- (13-catMean)/sdCat
1-pnorm(z) #proportion of adult cats weighing more than 13 lbs

#5). Z score for a 13 pound dog
dogMean <- mean(dogWeight)
sdDog <- sd(dogWeight)
z2 <- (13 - dogMean)/sdDog
fivenum(dogWeight)
1-pnorm(z2)

##Problem Set

#Question 1

#1a. What was the most common way that dogs arrived in the shelter?
dogs <- animaldata[animaldata$Animal.Type == "Dog",]
table(dogs$Intake.Type)

#1b. What proportion of dogs were brought to the shelter as an owner surrender?
81/(1+81+20+189)

#1c Of the dogs that were brought to the shelter as an owner surrender, how 
#many were returned to their owner
ownerSur <- dogs[dogs$Intake.Type == "Owner Surrender",]
returned <- ownerSur[ownerSur$Outcome.Type == "Return to Owner",] #2

#1d. What was the mean number of days that these dogs spent at the shelter
#before being returned to their owner
mean(returned$Days.Shelter)

#Question 2

#2a Total of 6 students reported they took the exam for 5 hours or longer. 
#What is the total number of students who took the exam
6/0.15

#2b. How many students spent fewer than 3 hours completing the take home exam
100-(20+20+15+15+10+5)
20+20+15 #number taking it less than 3 hours is 55%
0.55*40

#2c. If the professor wanted to report the center of the distribution, which measure
#should she use?

Median

#2d. Which bin includes the value of Q3?
At least 4 hours, but less than 5 hours


