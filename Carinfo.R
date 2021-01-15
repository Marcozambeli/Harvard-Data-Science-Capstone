#Installing necessary packages----
#Package and dataset download and 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org")

#Loading critical packages
library(tidyverse)
library(caret)
library(data.table)

#Download Dataset----

url<- getURL("https://raw.githubusercontent.com/Marcozambeli/Harvard-Data-Science-Capstone/master/AutoCosts")
carinfo<- read.csv(text = url)
carinfo<-carinfo %>% drop_na() #drop all NAs

head(carinfo)
names(carinfo)
summary(carinfo)
view(carinfo)
min(carinfo$Year)
max(carinfo$Year)

#separating by manufacturers
#Bmw
BMWdata <- filter(carinfo, Make == "BMW")
BMWdata
names(BMWdata)
nrow(BMWdata)

#Chrisler
Chrislerdata <- filter(carinfo, Make == "Chrisler")


#potencia pelo ano
ggplot(data = carinfo, aes(x = Year, y = Engine.HP)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)



# histogram of HP
hist(carinfo$Engine.HP, freq=TRUE, col="black", border="white", 
     main="Horse Power", xlab="hp", ylab="Units")

# analysing downsizing of bmw cars
carinfo2 <- filter(carinfo, Make == "BMW")
carinfo2
nrow(carinfo2) #number of observations
ncol(carinfo2) #number of variables
names(carinfo2)
ggplot(data = carinfo2, aes(x = Year, y = Engine.Cylinders)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)
ggplot(data = carinfo2, aes(x = Year, y = Engine.HP)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

#Analyzing the SUV trend
SUVdata <- filter(carinfo, Vehicle.Style == "4dr SUV")
nrow(SUVdata)
ggplot(data = SUVdata, aes(x = Year, y = Popularity)) + 
  #geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

#Analysing Downsizing through the years
ggplot(data = carinfo, aes(x = Year, y = Engine.Cylinders)) + 
  #geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)
ggplot(data = carinfo, aes(x = Year, y = Engine.HP)) + 
  #geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

#Analysing the fuel economy through the years
ggplot(data = carinfo, aes(x = Year, y = (city.mpg + highway.MPG)/2)) + 
  #geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

#popularity and access to cars
hist(carinfo$Popularity, freq=TRUE, col="black", border="white", 
     main="Popularity", xlab="hp", ylab="Units")

ggplot(data = carinfo, aes(x = Year, y = Popularity)) + 
  #geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)
median(carinfo$Popularity)
mad(carinfo$Popularity)
mean(carinfo$Popularity)

#Retail price 
ggplot(data = carinfo, aes(x = Year, y = MSRP)) + 
  #geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)
ggplot(data = carinfo, aes(x = MSRP, y = Driven_Wheels)) + 
  #geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) #PRECO PELA TRACAO
ggplot(data = carinfo, aes(x = Number.of.Doors, y = MSRP)) + 
  #geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) #Price by the no doors

#correlation between transmission and fuel economy
ggplot(data = carinfo, aes(x = (city.mpg + highway.MPG)/2, y = Transmission.Type)) + 
  #geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)
ggplot(data = carinfo, aes(x = Year, y = Transmission.Type)) + 
  #geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)






