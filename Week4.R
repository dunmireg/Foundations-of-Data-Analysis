#Tutorials - Austin City Limits dataset

gtab <- table(acl$Grammy)
prop.table(gtab) #calculates proportion, requires table object

gtab2 <- table(acl$Grammy, acl$Gender) #makes a contingency table
prop.table(gtab2) #gives proportion of total observations in each cell
prop.table(gtab2, 1) #conditions on rows, proportion on columns
prop.table(gtab2, 2) #conditions on columns, row proportions

barplot(gtab, main = "ACL Grammy Winners", xlab = "Grammy Winner", ylab = "Counts")
barplot(gtab2, legend = T, main = "Gender by Grammy Winner", xlab = "Gender", 
        ylab = "Counts", beside = T)
barplot(prop.table(gtab2, 2))

#PreLab - ACL data set

    #How many of the first 10 artists were Grammy winners
subset(acl[1:10,], Grammy == "Y")

    #What genre was played by the first female artist in the dataset who was over 60
subset(acl, Gender == "F" & Age > 60)

    #Create a table to show the marginal distribution for each variable.

genre <- table(acl$Genre)
genre
gender <- table(acl$Gender)
gener

    #Create a contingency table to show the conditional distribution for gender and genre.  

twoway <- table(acl$Gender, acl$Genre)
twoway

    #Make a bar chart to better visualize how many male and female artists played in each genre.

barplot(twoway, legend = T, beside = T)

      #Calculate P(A):  the probability of each type of music (genre) being played.

prop.table(genre)

      #Calculate P(A|B):  the probability of each genre, given the artist's gender.

prop.table(twoway,1) #1 refers to first variable

      #probability a randomly selected artist from dataset performed rock

prop.table(genre)

      #Probability that a randomly selected female performed rock
prop.table(twoway, 1)

# Lab Is there an association between winning a Grammy and the genre of music played

    #Create a table to show the marginal distributions for Genre and Grammy.

genre <- table(acl$Genre)
grammy <- table(acl$Grammy)

    #Create a contingency table to show the conditional distribution for Genre and Grammy.

twoway2 <- table(acl$Genre, acl$Grammy)

    #Make a bar chart to better visualize how many artists in each Genre received a Grammy.

barplot(twoway2, legend = T, beside = T)

    #Calculate P(A):  the probability of winning a Grammy.

prop.table(grammy)

    #Calculate P(A|B): the probability of winning Grammy, given the artist's Genre.

prop.table(twoway2,1)

    #Interpret what these probabilities tell us about the relationship between Genre and winning a Grammy.
#Genre and winning a grammy are not independent, there is a relationship

#Problem Set

    #Generate a table to show the number of artists that are "popular" (100K+ FB)
table(acl$Facebook.100K)
table(acl$Facebook.100k, acl$Age.Group)
prop.table(table(acl$Facebook.100k, acl$Age.Group),2)
