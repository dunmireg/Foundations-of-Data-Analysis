#Week 9 - Two Sample T Tests

#Pre-Lab - using Post Survey data
#Who is happier at the beginning of the semester, lower or upper classmen
#does student happiness change from the beginning of the semester

post <- read.csv("C:/Users/tdunmire/Desktop/Foundations of Data Analysis/PostSurvey.csv")

    #make a vector of happiness scores for each sample, check normality and run 
    #an independent t test

underclass_happy <- post$happy[post$classification == "Freshman" | post$classification == "Sophomore"]
upperclass_happy <- post$happy[post$classification == "Junior" | post$classification == "Senior"]

hist(underclass_happy, xlab = "Underclassman Happiness", main = "Percent of Time Happy")
hist(upperclass_happy, xlab = "Upperclassmen Happiness", main = "Percent of Time Happy")

t.test(underclass_happy, upperclass_happy)

    #make a vector of difference scores, check normality, and run dependent t test
post$diff_happy <- post$happy - post$post_happy
hist(post$diff, xlab = "Difference in Happiness over the Semester", main = "Happy-Post Happy")

t.test(post$happy, post$post_happy, paired = T)

#Lab - using post data 

    #Question 1 - Do students at UT spend more time on homework per week in college than they did in high school
post$hw_diff <- post$hw_hours_college - post$hw_hours_HS

hist(post$hw_diff, xlab = "Hours in college - hours in HS", main = "Difference in Homework Hours")

t.test(post$hw_hours_college, post$hw_hours_HS, paired = T, alternative = "greater")

    #Question 2 - Do students in fraternities or sororities get less sleep on weekends than other college students
isGreek <- post$sleep_Sat[post$greek == "yes"]
notGreek <- post$sleep_Sat[post$greek == "no"]

hist(isGreek, xlab = "Sleep on Saturday", main = "Sleep on Saturday if Greek")
hist(notGreek, xlab = "Sleep on Saturday", main = "Sleep on Saturday if not Greek")

t.test(isGreek, notGreek, alternative = "less")

#Problem Set

    #Is the increase in time spent studying the same for nursing and biology majors
isNurse <- post$hw_diff[post$major == "Nursing"]
isBio <- post$hw_diff[post$major == "Biology"]

hist(isNurse, xlab = "Difference in hours studying college - hs", main = "Hours for Nurses")
hist(isBio, xlab = "Difference in hours studying college - hs", main = "Hours for Biology")

t.test(isNurse, isBio)

#Remember the alternative field is based on the first then second means in a two-sample t test. 
#Eg "greater" is t.test(mean1 > mean2)