#
#Calculates a simple delivery route, now based on the number of drivers available


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
Time1 <- 07 #time slot beginning time
Time2 <- 09 #time slot finish time

#set the following flag to true too mock performance data over the date range
mockData <- 0
day <- 'Saturday' #choose from Mon Tue Wed Thu Fri Sat Sun

Center <- 'WC2N 5DN' #sets centre point for radius, restricting the deliveries to a certain area
Radius <- 7 #sets the radius from centre point (set to 0 to not use this option)
#filter by end location
wh <- 'TW6 2GA' #filter by warehouse: TW6 2GA for LHR, RH6 0NP for LGW
#clustering options
drivers <- 4 #number of drivers on shift

#initializing other variables
store <- NULL
savePath <- '~/Desktop/'

##########################################################################################################
#Load Data

datapath <- '~/Google Drive/PortrHacks/dataCrunching/ABC Bookings Analysis 2016 November 16/source data/Report.csv'
datapath2 <- '~/Google Drive/PortrHacks/dataCrunching/ABC Bookings Analysis 2016 November 16/source data/landing_pages.csv'
datapathSDS <- '~/Google Drive/PortrHacks/dataCrunching/ABC Bookings Analysis 2016 November 16/source data/bookings.csv'
# Time Span for Booking and Sales Analysis
startBookingDate <- as.Date('10/10/2016', format = "%d/%m/%Y")
endBookingDate <- as.Date('01/01/2018', format = "%d/%m/%Y")
# Time Span for Booking and Operations Analysis
startCollectionDate <- as.Date('29/11/2016', format = "%d/%m/%Y")
endCollectionDate <- as.Date('29/11/2017', format = "%d/%m/%Y")

#Load Data: ABC bookings
bookings <- read.csv(datapath)

################################################################################################

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
deduped.bookings$coll_slot <- as.numeric(substr(as.character(deduped.bookings$Outward_Journey_Luggage_Collection_time), 1, 2))
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
if(mockData){
  data.lite <- subset(data.lite, coll_Day == day)
}

#convert postcodes into character strings
data.lite$Collect.postcode <- as.character(data.lite$Outward_Journey_Luggage_collection_location_addresss_Postcode)
data.lite$Deliver.postcode <- as.character(data.lite$Outward_Journey_Luggage_drop_off_location_addresss_Postcode)

# only look into a particular destination
data.lite <- subset(data.lite, Deliver.postcode==wh)
row.names(data.lite) <- NULL

#simple clustering of results based on geocoordinates using kmeans clustering
geoCoords <- geocode(data.lite$Collect.postcode)

#simple clustering of results based on geocoordinates using kmeans clustering
km <- kmeans(geoCoords, centers = drivers, nstart = 3)
plot(geoCoords$lon, geoCoords$lat, col = km$cluster, pch = 20)
data.lite$driverRef <- km[[1]]
data.lite$coll_lon <- geoCoords$lon
data.lite$coll_lat <- geoCoords$lat

#store approx time to get to warehouse
timeToWh <- mapdist(data.lite$Collect.postcode,data.lite$Deliver.postcode,mode="driving")
data.liteWithTime <- cbind(data.lite,timeToWh)
data.liteWithTime <- data.liteWithTime[ order(-data.liteWithTime$minutes, decreasing = TRUE),]

# Save this general data, since it can be useful on its own right
write.csv(data.liteWithTime,file = "~/Desktop/SampleDriverAllocation.csv")

#Simple map plotting
df$lon <- data.liteWithTime$coll_lon
df$lat <- data.liteWithTime$coll_lat
df <- data.frame(df)

map <- get_googlemap('london', markers = df, path = df, scale = 2)
ggmap(map, extent = 'device')


#########################################################################################################

# good stuff
safeStore <- timeToWh
safeStore$from <- timeToWh$to
safeStore$to <- timeToWh$from
safeStore$driverRef <- km[[1]]

###Calculate routes for each driver in turn, based on k clusters

for(driver in 1:max(data.lite$driverRef)){
  
  store <- safeStore[safeStore$driverRef==driver,]
  
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
  
  fileName <- paste0(savePath,'driver',driver,'route','.csv')
  write.csv(rank,fileName)
  
  # going to plot the routes now
  test  <- route(rank$from[1], rank$to[1], mode = "driving", alternatives = FALSE, output="simple")
  
  for(i in 2:dim(rank)[1]){ 
    temp2  <- route(rank$from[i], rank$to[i], mode = "driving", alternatives = FALSE); 
    test <- rbind(test,temp2); 
    temp2  <-  0; 
  }
  
  df <- geocode(rank$to)
  df2 <- test[,c("endLon","endLat")]

  map <- get_googlemap('london', markers = df, path = df2, scale = 2)
  ggmap(map, extent = 'device')
  
}

##V2.1 -> for the route generator
test  <- route(rank$from[1], rank$to[1], mode = "driving", alternatives = FALSE)

for(i in 2:dim(rank)[1]){ 
  temp  <- route(rank$from[i], rank$to[i], mode = "driving", alternatives = FALSE); 
  test <- rbind(test,temp); 
  temp  <-  0; 
}

qmap(location = c(lon = mean(test$startLon), lat = mean(test$startLat)), zoom = 12, maprange = TRUE, maptype = 'hybrid') + geom_path(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),alpha = 3/4, size = 2, lineend ="round", colour = "blue", data = test)


set.seed(500)
df <- round(data.frame(
  x = jitter(rep(-95.36, 50), amount = .3),
  y = jitter(rep( 29.76, 50), amount = .3)
), digits = 2)


#################################################################################################
#Simple map plotting
df$lon <- data.liteWithTime$coll_lon
df$lat <- data.liteWithTime$coll_lat
df <- data.frame(df)

map <- get_googlemap('london', markers = df, path = df, scale = 2)
ggmap(map, extent = 'device')




write.csv(rank,file = "~/Desktop/Monday12DeliveryRoute1.csv")


