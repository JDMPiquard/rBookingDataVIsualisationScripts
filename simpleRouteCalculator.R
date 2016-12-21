#
#Calculates a simple delivery route 


#original simple script by JD Nov 2014
#updated by JD Nov 2016


#Inspirations
#https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/
#

#consider adding a restriction to central london destinations, by using a distance measured from postcode WC2N 5DN (trafalgar square)

#load required libraries
library('ggplot2')
library('ggmap')
require(plyr)

#set variables
#file <- 'FridayDeliveries.csv' #not in use
Time1 <- 12 #time slot beginning time
Time2 <- 14 #time slot finish time
day <- 'Monday' #choose from Mon Tue Wed Thu Fri Sat Sun
Center <- 'WC2N 5DN' #sets centre point for radius, restricting the deliveries to a certain area
Radius <- 7 #sets the radius from centre point (set to 0 to not use this option)
wh <- 'TW6 2GA' #filter by warehouse: TW6 2GA for LHR, RH6 0NP for LGW
drivers <- 4 #number of drivers on shift

#initializing other variables
store <- NULL


##########################################################################################################
#Load Data

datapath <- '~/Google Drive/PortrHacks/dataCrunching/ABC Bookings Analysis 2016 November 16/source data/Report.csv'
datapath2 <- '~/Google Drive/PortrHacks/dataCrunching/ABC Bookings Analysis 2016 November 16/source data/landing_pages.csv'
datapathSDS <- '~/Google Drive/PortrHacks/dataCrunching/ABC Bookings Analysis 2016 November 16/source data/bookings.csv'
# Time Span for Booking and Sales Analysis
startBookingDate <- as.Date('10/10/2016', format = "%d/%m/%Y")
endBookingDate <- as.Date('01/01/2018', format = "%d/%m/%Y")
# Time Span for Booking and Operations Analysis
startCollectionDate <- as.Date('10/10/2016', format = "%d/%m/%Y")
endCollectionDate <- as.Date('01/01/2017', format = "%d/%m/%Y")

#Load Data: ABC bookings
bookings <- read.csv(datapath)

###Dealing with the bookings###

#Remove duplicates
deduped.bookings <- bookings[!duplicated(bookings[,1]), ]

#Clean up dates
deduped.bookings$Booking_date  <- as.Date(deduped.bookings$Booking_date, format = "%d/%m/%Y")
deduped.bookings$Outward_Journey_Luggage_Collection_date  <- as.Date(deduped.bookings$Outward_Journey_Luggage_Collection_date, format = "%d/%m/%Y")
#Booking_datetime
#deduped.bookings$Booking_datetime  <- as.Date(deduped.bookings$Booking_datetime, format = "%d/%m/%Y %h:%M:s")
deduped.bookings$clean_flt_time <- substr(as.character(deduped.bookings$Out.bound_flt_time), 1, 2)
deduped.bookings$clean_coll_time <- substr(as.character(deduped.bookings$Outward_Journey_Luggage_Collection_time), 1, 5)
deduped.bookings$clean_Complete_time <- substr(as.character(deduped.bookings$Booking_time), 1, 2)
deduped.bookings$coll_slot <- substr(as.character(deduped.bookings$Outward_Journey_Luggage_Collection_time), 1, 2)
deduped.bookings$coll_Day <- weekdays(deduped.bookings$Outward_Journey_Luggage_Collection_date)
#Outward_Journey_Luggage_Collection_date

#Make a few interesting calculations
#deduped.bookings$bookingLead_time <- deduped.bookings$

#Filter to wanted booking dates
allDeduped.bookings <- deduped.bookings
deduped.bookings <- deduped.bookings[(deduped.bookings$Booking_date>=startBookingDate&deduped.bookings$Booking_date<=endBookingDate),]

#Remove cancelled bookings
notCancelled.bookings <- deduped.bookings[deduped.bookings$Cancelled==F,]

#Ops Dataset
opsDatasetABC <- allDeduped.bookings[(allDeduped.bookings$Outward_Journey_Luggage_Collection_date>=startCollectionDate&allDeduped.bookings$Booking_date<=endCollectionDate),]
opsDatasetABC <- opsDatasetABC[opsDatasetABC$Cancelled==F,]

##########################################################################################################################

#reading data table
data <- opsDatasetABC

#extracting the values we're interested in from the table (insert code below)
#data.lite <- data[1:dim(data)[1],] #use this for all data
data.lite <- subset(data, coll_slot>=Time1 & coll_slot<Time2) # use this for focusing on a specific time slot
#data.lite <- subset(data.lite, coll_Day == day)

#convert postcodes into character strings
data.lite$Collect.postcode <- as.character(data.lite$Outward_Journey_Luggage_collection_location_addresss_Postcode)
data.lite$Deliver.postcode <- as.character(data.lite$Outward_Journey_Luggage_drop_off_location_addresss_Postcode)
data.lite <- subset(data.lite, Deliver.postcode==wh) # use this for focusing on a specific time slot
row.names(data.lite) <- NULL

#simple clustering of results based on geocoordinates using kmeans clustering
geoCoords <- geocode(data.lite$Collect.postcode)
km <- kmeans(geoCoords, centers = drivers)
plot(geoCoords$lon, geoCoords$lat, col = km$cluster, pch = 20)
data.lite$driverRef <- km[[1]]
#store approx time to get to warehouse
timeToWh <- mapdist(data.lite$Collect.postcode,data.lite$Deliver.postcode,mode="driving")
data.liteWithTime <- cbind(data.lite,timeToWh)

# Save this general data, since it can be useful on its own right
write.csv(data.liteWithTime,file = "~/Desktop/SampleDriverAllocation.csv")

#########################################################################################################
###Calculate routes
store <- timeToWh
store$from <- timeToWh$to
store$to <- timeToWh$from


#order by shortest time
store <- store[ order(-store[,7], decreasing = TRUE),]
row.names(store) <- NULL  #restarts the numbering of the data frame

## Onto nested loop to find a potential route
#keep closest in separate data frame, then delete it from store
rank <- store[1,] #rank stores the final solution, ranked by order of the route
row.names(rank) <- NULL  #restarts the numbering of the data frame
store <- store[-1,] #removes the first line of the data frame

#create temp data frame
temp <- rank
t <- 2 #initiate variable for while loop
finish <- dim(store)[1]

while(dim(rank)[1]<=finish){
  
  for(i in 1:dim(store)[1]){
    temp[i,] <- mapdist(rank[(t-1),2],store[i,2],mode="driving")
  }
  
  temp <- temp[ order(-temp[,7], decreasing = TRUE),]
  
  rank[t,] <- temp[1,]
  row.names(rank) <- NULL
  temp <- temp[-1,]
  row.names(temp) <- NULL
  store <- temp
  
  t <- t+1
  
}


write.csv(rank,file = "~/Desktop/Monday12DeliveryRoute1.csv")
