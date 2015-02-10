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

#Lab - still using film data set

table(film$Studio)
    #Are some studios more successful in keeping their films in the theaters longer
aggregate(Days ~ Studio, film, mean)
aggregate(Days ~ Studio, film, sd)

boxplot(film$Days ~ film$Studio, main = "Days in Theater by Studio", 
        ylab = "Days in Theater", xlab = "Studio")

daysStudio <- aov(film$Days ~ film$Studio)
summary(daysStudio)

TukeyHSD(daysStudio)

    #Do some studios earn a greater percentage of their earnings domestically than others
aggregate(Pct.Dom ~ Studio, film, mean)
aggregate(Pct.Dom ~ Studio, film, sd)

boxplot(film$Pct.Dom ~ film$Studio, main = "Percentage of Domestic sales by Studio",
        ylab = "Percent Grosss from Domestic", xlab = "Studio")

perDomest <- aov(film$Pct.Dom ~ film$Studio)
summary(perDomest)

TukeyHSD(perDomest)

#Problem Set

    #Define films as low, medium, or high budget and answer questions
film$Budget.Class[film$Budget < 100] <- "Low Budget"
film$Budget.Class[100 <= film$Budget & film$Budget < 150] <- "Medium Budget"
film$Budget.Class[film$Budget >= 150] <- "High Budget"

table(film$Budget.Class)

aggregate(Pct.Dom ~ Budget.Class, film, mean)

boxplot(film$Pct.Dom ~ film$Budget.Class, main = "Percentage of Domestic Sales by Budget Class", 
        ylab = "Percent Gross Sales from Domestic Market", xlab = "Budget Class")

perDomBudget <- aov(film$Pct.Dom ~ film$Budget.Class)
summary(perDomBudget)

TukeyHSD(perDomBudget)

    #Local police department anova
tickets <- c(8, 4, 6, 8, 6, 4, 3, 7, 0, 2, 7, 5, 1, 2, 7, 6, 5, 0)
section <- c("one", "one", "one", "one", "one", "one", "two", "two","two","two","two","two", "three", "three","three","three","three","three")
data <- data.frame(tickets, section)

aggregate(tickets ~ section, data, mean)
ticketANOVA <- aov(data$tickets ~ data$section)
summary(ticketANOVA)