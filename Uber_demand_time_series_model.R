#This file explores time series patterns of Uber demand in New York City in a week.
#The raw data can be found at NYC Taxi & Limousine Commission.
#https://github.com/fivethirtyeight/uber-tlc-foil-response.

#extract pickup demand by date
df <- read.csv("uber-raw-data-janjune-15.csv", stringsAsFactors = F)

df$Pickup_date <- as.POSIXct(df$Pickup_date, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
head(df)

df$date <- as.Date(df$Pickup_date)

library(plyr)
uber <- count(df$date)
colnames(uber) <- c('date', 'pickup')

write.csv(uber, file='uber_pickup_by_day_Jan_to_June_2015.csv', row.names=F)



#plot realization
uber <- uber[-nrow(uber),]
plot(uber$pickup)
lines(uber$pickup)

library(RTseries)
uber.ts <- ts(uber$pickup/1000, frequency = 7, start = 1)
uber.tsd <- tsd(uber.ts, 
                data.title='Uber Demand in New York City Jan to Jun 2015',
                response.units='Thousand Pickups', 
                time.units='Week')
plot(uber.tsd)



#tentative identification
iden(uber.tsd)
iden(uber.tsd, d=1)
iden(uber.tsd, D=1)
iden(uber.tsd, d=1, D=1)



#estimate parameters
#model 1
model1 <- esti(uber.tsd, model=model.pdq(p=1, P=1, D=1, Q=1, period=7))
#model 2
model2 <- esti(uber.tsd, model=model.pdq(p=1, d=1, q=1, D=1, Q=1, period=7))
#model 3
model3 <- esti(uber.tsd, model=model.pdq(q=1, d=1, D=1, Q=1, period=7))



