#Week 7 - Sampling
#use student survey dataset called 'survey'

survey <- read.csv("C:/Users/tdunmire/Desktop/Foundations of Data Analysis/StudentSurvey.csv")
sample(survey$age, size = 30) #pulls a sample from the data set

#make a sampling distribution
myxbar <- rep(NA, 1000)
for (i in 1:1000) {
  mysamp <- sample(survey$age, size = 30)
  myxbar[i] <- mean(mysamp)
}

hist(myxbar)

mean(myxbar)
mean(survey$age)

sd(myxbar)
sd(survey$age)/sqrt(30)

#Pre-Lab - How many letters long is a typical UT students name? How does the estimate
#change as we increase the size of our sample


    #Draw 1,000 samples of size n = 5 and get the means and graph them to see shape
hist(survey$name_letters) #population stats
fivenum(survey$name_letters)
mean(survey$name_letters)
sd(survey$name_letters)

xbar5 <- rep(NA, 1000)
for (i in 1:1000) {
  x <- sample(survey$name_letters, size = 5)
  xbar5[i] <- mean(x)
}

hist(xbar5, xlim = c(2,10))

mean(xbar5)
sd(xbar5)

    #compare to the std dev predicted by CTL
sd(survey$name_letters)/sqrt(5)

    #repeat for samples of size n = 15
xbar15 <- rep(NA, 1000)
for (i in 1:1000){
  x <- sample(survey$name_letters, size = 15)
  xbar15[i] <- mean(x)
}
hist(xbar15, xlim = c(2,10))
mean(xbar15)
sd(xbar15)
sd(survey$name_letters)/sqrt(15)

#repeat for samples of size n = 25
xbar25 <- rep(NA, 1000)
for (i in 1:1000){
  x <- sample(survey$name_letters, size = 25)
  xbar25[i] <- mean(x)
}
hist(xbar25, xlim = c(2,10))
mean(xbar25)
sd(xbar25)
sd(survey$name_letters)/sqrt(25)


#Lab - What percentage of the time are college students happy? How does our 
#estimate of the true mean change as our sample size increases?

    #population data
hist(survey$happy)
mean(survey$happy)
sd(survey$happy)

    #samples of size 5
xbar5 <- rep(NA, 1000)
for(i in 1:1000) {
  x <- sample(survey$happy, size = 5)
  xbar5[i] <- mean(x)
}
hist(xbar5)
mean(xbar5)
sd(xbar5)
sd(survey$happy)/sqrt(5)

    #samples of size 15
xbar15 <- rep(NA, 1000)
for(i in 1:1000) {
  x <- sample(survey$happy, size = 15)
  xbar15[i] <- mean(x)
}
hist(xbar15)
mean(xbar15)
sd(xbar15)
sd(survey$happy)/sqrt(15)

    #samples of size 25
xbar25 <- rep(NA, 1000)
for(i in 1:1000) {
  x <- sample(survey$happy, size = 25)
  xbar25[i] <- mean(x)
}
hist(xbar25)
mean(xbar25)
sd(xbar25)
sd(survey$happy)/sqrt(25)

#Problem Set 

    #How much do students like Austin
hist(survey$austin)
mean(survey$austin)
sd(survey$austin)
sd(survey$austin)/sqrt(10) #for sampling distribution with samples n = 10

    #draw 1,000 samples with n = 10 and compare
xbar10 <- rep(NA, 1000)
for(i in 1:1000){
  x <- sample(survey$austin, size = 10)
  xbar10[i] <- mean(x)
}
hist(xbar10)
mean(xbar10)
sd(xbar10)