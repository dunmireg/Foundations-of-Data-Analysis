#Week 5 - using World Records csv as WR

##Note: linFit is a function from the SDSFoundations package

#Pre Lab 

    #Create a subset of dataset that contains only WR cases for men's shotput & women's
menshot <- WR[WR$Event == "Mens Shotput",]
womenshot <- WR[WR$Event == "Womens Shotput",]

    #Create a scatterplot of year and record shotput 
plot(menshot$Year, menshot$Record, main = "Mens Shotput World Records", xlab = 
       "Year", ylab = "World Record Distance (m)", pch = 16)
plot(womenshot$Year, womenshot$Record, main = "womens Shotput World Records", 
     xlab = "Year", ylab = "Womens Record Distance (m)", pch = 16)

      #They look rather linear

      #Run a linear model and interpret results, use SDSFoundations
linFit(menshot$Year, menshot$Record)
linFit(womenshot$Year, womenshot$Record)

#Lab

    #Create a subset of data for men and women's Mile Event
menMile <- WR[WR$Event == "Mens Mile",]
womenMile <- WR[WR$Event == "Womens Mile",]

    #Make a scatterplot for both
plot(menMile$Year, menMile$Record, main = "Mens Mile Record", xlab = "Year", 
     ylab = "Mens Mile Distance (m)", pch = 16)
plot(womenMile$Year, womenMile$Record, main = "Womens Mile Record", xlab = "Year", 
     ylab = "Womens Mile Distance (m)", pch = 16)

    #Run linear model on each
linFit(menMile$Year, menMile$Record)
linFit(womenMile$Year, womenMile$Record)

#Problem Set

    #Make subset for men's pole vault in years 1970 and later
menPole <- subset(WR, Event == "Mens Polevault" & Year >= 1970)

plot(menPole$Year, menPole$Record, main = "Mens Polevault Record", xlab = "Year",
     ylab = "Mens PoleVault Height(m)", pch = 16)

linFit(menPole$Year, menPole$Record)
