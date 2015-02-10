#Week 10 - Hypothesis Testing, Categorical Data

#Pre-Lab - using Austin City Limits dataset
acl <- read.csv("C:/Users/tdunmire/Desktop/Foundations of Data Analysis/AustinCityLimits.csv")

    #Are there an equal number of male and female performers on Austin City Limits
    #Goodness of Fit test
gender_tab <- table(acl$Gender)

ExpGender <- c(0.50, 0.50)
chisq.test(gender_tab, p = ExpGender)$expected #expected if null is true

chisq.test(gender_tab, p = ExpGender)

    #Are male performers just as likely to have had a Top 10 hit as female
    #Test of independence
gender_top10 <- table(acl$Gender, acl$BB.wk.top10)

chisq.test(gender_top10, correct = FALSE)$expected

chisq.test(gender_top10, correct = FALSE)

#Lab - acl data set

    #Are each of the four musical genres equally represented on Austin City Limits
    #Goodnes of Fit
genre_table <- table(acl$Genre)

expGenre <- c(0.25, 0.25, 0.25, 0.25)

chisq.test(genre_table, p = expGenre)$expected

chisq.test(genre_table, p = expGenre)

    #Are some genres more likely to draw a large (100K+) Twitter following than others
    #Test of independence
tweet_genre <- table(acl$Genre, acl$Twitter.100k)

chisq.test(tweet_genre)$expected

chisq.test(tweet_genre)

#Problem Set



