3 + 4
5*6*7/9
2^10

##Can highlight lines and click 'Run' to only run those lines

#Assign the value 6+2 to x
x <- 6+2

library(SDSFoundations)
BikeData <- BikeData

#Number students in data set
table(BikeData$student)

#Pull out student data into new frame
student <- BikeData[BikeData$student ==1,]

#Find how often the students ride
table(student$cyc_freq)

#Create vector for variable distance
distance <- student$distance
distance

#Find average distance ridden
mean(distance)