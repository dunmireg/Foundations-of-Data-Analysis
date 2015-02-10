#Week 11 - Hypothesis Testing with More than 2 Means

#Pre-Lab - using film data set

film <- read.csv("C:/Users/tdunmire/Desktop/Foundations of Data Analysis/FilmData.csv")

    #Does a film's rating (PG, PG-13, or R) impact its cost to produce
table(film$Rating)

aggregate(Budget ~ Rating, film, mean) #average film budget by group
aggregate(Budget ~ Rating, film, sd) #standard deviation

boxplot(film$Budget ~ film$Rating, main = "Film Budgets by Rating", 
        ylab = "Budget", xlab = "MPAA Rating")

modelbud <- aov(film$Budget ~ film$Rating) #run anova
summary(modelbud)

TukeyHSD(modelbud) #run post-hoc test

    #Does a film's rating influence its IMDB score
aggregate(IMDB ~ Rating, film, mean)
aggregate(IMDB ~ Rating, film, sd)

boxplot(film$IMDB ~ film$Rating, main = "ImDB Scores by Rating", 
        ylab = "IMDB Score", xlab = "MPAA Rating")

modelscore <- aov(film$IMDB ~ film$Rating)
summary(modelscore)

TukeyHSD(modelscore)

#Lab 