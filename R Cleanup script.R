# Simple script to summarise booking information from the CSV export

require(plyr)
require(googleVis)

#landing_pages_2_13_december_2016_
datapath <- '~/Google Drive/PortrHacks/dataCrunching/ABC Bookings Analysis 2016 November 16/source data/Report.csv'
datapathLandin12Dec <- '~/Google Drive/PortrHacks/dataCrunching/ABC Bookings Analysis 2016 November 16/source data/Do not touch - fixed settings/landing_pages.csv'
datapathLandingNew <- '~/Google Drive/PortrHacks/dataCrunching/ABC Bookings Analysis 2016 November 16/source data/landing_pages_2_13_december_2016_.csv'
datapathSDS <- '~/Google Drive/PortrHacks/dataCrunching/ABC Bookings Analysis 2016 November 16/source data/bookings.csv'
# Time Span for Booking and Sales Analysis
startBookingDate <- as.Date('14/12/2016', format = "%d/%m/%Y") #originally used '10/10/2016'
endBookingDate <- as.Date('01/01/2018', format = "%d/%m/%Y")
# Time Span for Booking and Operations Analysis
startCollectionDate <- as.Date('10/10/2016', format = "%d/%m/%Y")
endCollectionDate <- as.Date('01/01/2017', format = "%d/%m/%Y")

###FUNCTIONS###
#Defining a summary function for ABC bookings data
summariseBookingData <- function(df, arg1, arg2){
  if(missing(arg2)){
    summaryBy <- arg1
  }else{
    summaryBy <- c(arg1,arg2)
  }
  summarised.df <- ddply(df, summaryBy, summarise,
                        salesNumber       = length(Booking_reference),
                        meanBags          = mean(Total_luggage_No),
                        totalHandBags     = sum(X.Hand_luggage_No),
                        totalHoldBags     = sum(Hold_luggage_No),
                        totalPaxABC       = sum(Total_passengers, na.rm=TRUE),
                        meanValue         = mean(transaction_payment_total, na.rm=TRUE)/100,
                        totalRevenue      = sum(Card_Payment_Amount, na.rm=TRUE)/100,
                        totalBookingValue = sum(transaction_payment_total, na.rm=TRUE)/100,
                        totalPromoValue   = totalBookingValue - totalRevenue          
  )

  return(summarised.df)
}

# similar summary function but focused on operations
summariseABCOpsData <- function(df, arg1, arg2){
  if(missing(arg2)){
    summaryBy <- arg1
  }else{
    summaryBy <- c(arg1,arg2)
  }
  summarised.df <- ddply(df, summaryBy, summarise,
                         collections       = length(Booking_reference),
                         meanBags          = mean(Total_luggage_No),
                         totalHandBags     = sum(X.Hand_luggage_No),
                         totalHoldBags     = sum(Hold_luggage_No),
                         nbrReassignedBkgs = sum(ifelse(Number_of_bags_downgraded>0,1,0)),
                         nbrReassignedBags = sum(Number_of_bags_downgraded),
                         pctReassigBkgs    = nbrReassignedBkgs/collections*100,
                         totalPaxABC       = sum(Total_passengers),
                         meanValue         = mean(transaction_payment_total, na.rm=TRUE)/100,
                         totalRevenue      = sum(Card_Payment_Amount, na.rm=TRUE)/100,
                         totalBookingValue = sum(transaction_payment_total, na.rm=TRUE)/100,
                         totalPromoValue   = totalBookingValue - totalRevenue          
  )
  
  return(summarised.df)
}

#Defining a summary function for SDS bookings data
summariseSDSBookingData <- function(df, arg1, arg2){
  if(missing(arg2)){
    summaryBy <- arg1
  }else{
    summaryBy <- c(arg1,arg2)
  }
  summarised.df <- ddply(df, summaryBy, summarise,
                         salesNumberSDS       = length(Booking_reference),
                         meanBagsSDS          = mean(Total_luggage_No),
                         totalHandBagsSDS     = sum(Hand_luggage_No),
                         totalHoldBagsSDS     = sum(Hold_luggage_No),
                         meanValueSDS         = mean(transaction_payment_total, na.rm=TRUE),
                         totalRevenueSDS      = sum(transaction_payment_total, na.rm=TRUE),
                         totalBookingValueSDS = sum(Booking_value_gross_total, na.rm=TRUE),
                         totalPromoValueSDS   = totalBookingValueSDS - totalRevenueSDS          
  )
  
  return(summarised.df)
}

#Defining a summary function for Website Visit data
summariseWebVisitData <- function(df, arg1, arg2){
  if(missing(arg2)){
    summaryBy <- arg1
  }else{
    summaryBy <- c(arg1,arg2)
  }
  summarised.df <- ddply(df, summaryBy, summarise,
                         uniqueVisitors        = sum(People.Who.Visited.Site, na.rm=TRUE),
                         quoteFromLondon       = sum(People.Who.got.quote.FROM.london, na.rm=TRUE),
                         quoteFromLondonAndBA  = sum(People.who.selected.British.airways.on.FROM.London.Quote, na.rm=TRUE),
                         quoteToLondon         = sum(People.Who.got.quote.TO.London, na.rm=TRUE),
                         quoteInteraction      = quoteFromLondon + quoteToLondon,
                         selectABC             = sum(People.Who.selected.ABC, na.rm=TRUE),
                         selectSDSFromLondon   = sum(People.who.did.SDS.Selection.From.London, na.rm=TRUE),
                         selectSDSFromLondonBA = sum(People.who.selected.SDS.FROM.London.and.BA, na.rm=TRUE),
                         selectSDSToLondon     = sum(People.who.did.SDS.Selection.TO.London, na.rm=TRUE),
                         selectSDS             = selectSDSFromLondon + selectSDSToLondon
                         
  )
  return(summarised.df)
}

##########################################################################################################

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

###Date Based Summaries###
#Summarise data by booking date
byBooking.data <- summariseBookingData(notCancelled.bookings, "Booking_date")

#Summarise data by number of bags
byBagsBooking.data <- summariseBookingData(notCancelled.bookings, "Hold_luggage_No")

#Summarise data by number of bags
byPaxBooking.data <- summariseBookingData(notCancelled.bookings, "Total_passengers")

### Operational ABC Summaries
byCollectionDate.opsData <- summariseABCOpsData(opsDatasetABC,"Outward_Journey_Luggage_Collection_date")

##########################################################################################################
#Load Data: SDS bookings
bookingsSDS <- read.csv(datapathSDS)
rowNamesSDS <- as.character(unlist(read.csv(datapathSDS, nrows=1, header=F)))
colnames(bookingsSDS)[1] <- "Booking_reference"  # Seems to solve corrupted header name

#Clean up dates
bookingsSDS$Booking_date  <- as.Date(bookingsSDS$Booking_date, format = "%d/%m/%Y")

#Filter by booking dates
bookingsSDS.filtered <- bookingsSDS[(bookingsSDS$Booking_date>=startBookingDate&bookingsSDS$Booking_date<=endBookingDate),]
#note that technically, below, we should convert to boolean then filter. This method is not ideal
bookingsSDS.filtered <- bookingsSDS.filtered[bookingsSDS.filtered$Cancelled=="False",]

###Date Based Summaries###
#Summarise data by booking date
byBookingDateSDS.data <- summariseSDSBookingData(bookingsSDS.filtered, "Booking_date")
#Summarise data by booking date & Journey Direction
byBookingDateAndDirection.data <- summariseSDSBookingData(bookingsSDS.filtered, "Booking_date","Journey_direction")

##########################################################################################################

###Dealing with Website Visits###
#Load Data: website visits
landing12Dec <- read.csv(datapathLandin12Dec)
landingNew <- read.csv(datapathLandingNew)
landingVisits <- rbind(landing12Dec,landingNew)

#Clean up dates
landingVisits$Over.Time  <- as.Date(landingVisits$Over.Time, format = "%Y-%m-%d")

#Filter out unwanted dates
landingVisits.filtered <- landingVisits[(landingVisits$Over.Time>=startBookingDate&landingVisits$Over.Time<=endBookingDate),]

#Filter out unwanted pages
landingVisits.filtered <- landingVisits.filtered[grep("*airportr.com*", landingVisits.filtered$KM.Landing.Page), ]

#Process for some interesting data
landingVisits.filtered$KM.Landing.Page <- gsub('.*data=.*','dataLink',landingVisits.filtered$KM.Landing.Page,ignore.case = T)
landingVisits.filtered$KM.Landing.Page <- gsub('.*partner=ba.*','baDirect',landingVisits.filtered$KM.Landing.Page,ignore.case = T)
landingVisits.filtered$KM.Landing.Page <- gsub('.*campaign=','',landingVisits.filtered$KM.Landing.Page,ignore.case = T)
landingVisits.filtered$KM.Landing.Page <- gsub('.*gclid=.*','gclid',landingVisits.filtered$KM.Landing.Page,ignore.case = T)
landingVisits.filtered$KM.Landing.Page <- gsub('https://www.airportr.com','direct',landingVisits.filtered$KM.Landing.Page,ignore.case = T)
landingVisits.filtered$KM.Landing.Page <- gsub('https://airportr.com/#..','direct',landingVisits.filtered$KM.Landing.Page,ignore.case = T)
landingVisits.filtered$KM.Landing.Page <- gsub('https://airportr.com/..','direct',landingVisits.filtered$KM.Landing.Page,ignore.case = T)

#
landingVisits.filtered$dataLink <- grepl('.*data=.*',landingVisits.filtered$KM.Landing.Page,ignore.case = T)
landingVisits.filtered$directBA <- grepl('.*partner=ba.*',landingVisits.filtered$KM.Landing.Page,ignore.case = T)

#https://airportr.com
#Summarise by date
uniqueVisitsByDate.data <- summariseWebVisitData(landingVisits.filtered,'Over.Time')
uniqueVisitsByDate.data <- uniqueVisitsByDate.data[with(uniqueVisitsByDate.data, order(Over.Time)), ]

#Summarise by date with source
uniqueVisitsByDateAndSource.data <- summariseWebVisitData(landingVisits.filtered,'Over.Time','KM.Landing.Page')
uniqueVisitsByDateAndSource.data <- uniqueVisitsByDateAndSource.data[with(uniqueVisitsByDateAndSource.data, order(Over.Time)), ]

#People.Who.selected.ABC
##########################################################################################################
# 
# ###CSV Merge
# bookingsABC <- bookings
# bookingsSDS2 <- bookingsSDS
# 
# #clean up some names
# colnames(bookingsABC)[1] <- 'Booking_reference'
# colnames(bookingsSDS2)[1] <- 'Booking_reference'
# colnames(bookingsABC)[51] <- 'Hand_luggage_No'
# 
# #clean up some ABC values for consistency
# #paymentValues
# bookingsABC$Booking_value_gross_total <- bookingsABC$transaction_payment_total/100
# bookingsABC$transaction_payment_total <- bookingsABC$Card_Payment_Amount/100
# bookingsABC$Card_Payment_Amount <- NULL
# #zones
# bookingsABC$Zone <- substr(bookingsABC$Zone, 10, 11)
# #dates
# bookingsABC$Booking_date  <- as.Date(bookingsABC$Booking_date, format = "%d/%m/%Y")
# 
# #identify columns which are not present in each spreadsheet
# columnsToDeleteSDS <- names(bookingsSDS2)[!(names(bookingsSDS2) %in% names(bookingsABC))]
# columnsToDeleteABC <- names(bookingsABC)[!(names(bookingsABC) %in% names(bookingsSDS2))]
# 
# bookingsSDS2[columnsToDeleteABC] <- NA
# bookingsABC[columnsToDeleteSDS] <- NA
# 
# bookings.merged <- rbind(bookingsABC,bookingsSDS2)
# 
# write.csv(bookings.merged, '/Users/JD/Desktop/mergedBookings.csv')

##########################################################################################################

###Merge Data Sets###
mergedBooking.data <- merge(byBooking.data,uniqueVisitsByDate.data,by.x="Booking_date",by.y="Over.Time")
mergedBooking.data <- merge(mergedBooking.data, byBookingDateSDS.data, by="Booking_date")

###KPI###
totalSales <- length(notCancelled.bookings$Booking_reference)
totalValue <- sum(notCancelled.bookings$Card_Payment_Amount)/100

###Sales###
mergedBooking.data$totalSales <- mergedBooking.data$salesNumber + mergedBooking.data$salesNumberSDS
mergedBooking.data$pctABCOfTotalSales <- mergedBooking.data$salesNumber/mergedBooking.data$totalSales*100
mergedBooking.data$combinedRevenue <- mergedBooking.data$totalRevenueSDS + mergedBooking.data$totalRevenue
mergedBooking.data$pctABCOfCombinedRevenue <- mergedBooking.data$totalRevenue/mergedBooking.data$combinedRevenue*100
###Conversions###
mergedBooking.data$conversionABC <- mergedBooking.data$salesNumber/mergedBooking.data$uniqueVisitors*100
mergedBooking.data$conversionFromBAQuoteABC <- mergedBooking.data$salesNumber/mergedBooking.data$quoteFromLondonAndBA*100
###Quote Interactions###
mergedBooking.data$pctQuoteInteraction <- mergedBooking.data$quoteInteraction/mergedBooking.data$uniqueVisitors*100
mergedBooking.data$pctFromQuoteBA <- mergedBooking.data$quoteFromLondonAndBA/mergedBooking.data$quoteInteraction*100
mergedBooking.data$pctFromQuote <- mergedBooking.data$quoteFromLondon/mergedBooking.data$quoteInteraction*100
mergedBooking.data$pctToQuote <- mergedBooking.data$quoteToLondon/mergedBooking.data$quoteInteraction*100
mergedBooking.data$clickThroughFromQuote <- (mergedBooking.data$selectABC + mergedBooking.data$selectSDS)/(mergedBooking.data$quoteInteraction)*100
mergedBooking.data$clickThroughABCFromLondon <- mergedBooking.data$selectABC/mergedBooking.data$quoteFromLondon*100


##########################################################################################################

###Merged DataSet Plots###

#Sales By Day

#Stacked Area chart of ABC + SDS Sales
StackedArea_salesPerDay.plot <- gvisAreaChart(mergedBooking.data, xvar="Booking_date", 
                                    yvar=c("salesNumberSDS", "salesNumber"),
                                    options=list(isStacked=TRUE,
                                                 height= 800,
                                                 title= "Stacked ABC + SDS Bookings"))
plot(StackedArea_salesPerDay.plot)

#Stacked Area by Journey Direction
locToAptSDS <- byBookingDateAndDirection.data[byBookingDateAndDirection.data$Journey_direction=='GeneralLocationToAirport',]
aptToLocSDS <- byBookingDateAndDirection.data[byBookingDateAndDirection.data$Journey_direction=='AirportToGeneralLocation',]
comboSDS <- merge(locToAptSDS,aptToLocSDS,by="Booking_date")

comboMerge.data <- merge(byBooking.data,comboSDS,by="Booking_date")

StackedArea_salesPerDayDirection.plot <- gvisAreaChart(comboMerge.data, xvar="Booking_date", 
                                              yvar=c("salesNumberSDS.x", "salesNumberSDS.y", "salesNumber"),
                                              options=list(isStacked=TRUE,
                                                           height= 800,
                                                           title= "Stacked ABC + SDS Bookings"))
plot(StackedArea_salesPerDayDirection.plot)

# % ABC Share by day
pctABCOfTotal.plot <- mergedBooking.data[,c("Booking_date","pctABCOfTotalSales","pctABCOfCombinedRevenue")]
Line_pctABCOfTotal.plot <- gvisLineChart(pctABCOfTotal.plot,
                                         options=list(height= 800,
                                                      title= "% ABC of Total Sales"))
plot(Line_pctABCOfTotal.plot)

###Website Metrics

#Plot Daily Unique Visits
uniqueVisitsByDate.plot <- uniqueVisitsByDate.data[,c("Over.Time","uniqueVisitors")]
Line_uniqueVisitsByDate.plot <- gvisLineChart(uniqueVisitsByDate.plot,
                                              options=list(height= 800,
                                                           title= "Unique Visits by day"))
plot(Line_uniqueVisitsByDate.plot)

#ABC Sales By Day VS Total Visits
Line2_bookingNumber.plot2 <- gvisLineChart(mergedBooking.data, "Booking_date", c("uniqueVisitors","salesNumber"),
                                           options=list(
                                             series="[{targetAxisIndex: 0},
                                                    {targetAxisIndex:1}]",
                                             vAxes="[{title:'unique visitors'}, {title:'ABC sales'}]",
                                             height= 800,
                                             title= "ABC Sales VS Visitors"
                                           ))
plot(Line2_bookingNumber.plot2)

#ABC Conversion Rates
conversionsABC.plot <- mergedBooking.data[,c("Booking_date","conversionABC","conversionFromBAQuoteABC")]
selectSDS_ABCEligible.plot <- gvisLineChart(conversionsABC.plot,
                                          options=list(height= 800,
                                                       title= "% Conversions"))
plot(selectSDS_ABCEligible.plot)

# Total Quote Interactions
conversionsABC.plot <- mergedBooking.data[,c("Booking_date","quoteInteraction","quoteFromLondonAndBA", "quoteFromLondon", "quoteToLondon")]
selectSDS_ABCEligible.plot <- gvisLineChart(conversionsABC.plot,
                                          options=list(height= 800,
                                                       title= "Total Quote Interaction Splits"))
plot(selectSDS_ABCEligible.plot)

# Pct Quote Interactions
conversionsABC.plot <- mergedBooking.data[,c("Booking_date","pctQuoteInteraction","pctFromQuoteBA", "pctFromQuote", "pctToQuote")]
selectSDS_ABCEligible.plot <- gvisLineChart(conversionsABC.plot,
                                          options=list(height= 800,
                                                       title= "% Quote Interaction Splits"))
plot(selectSDS_ABCEligible.plot)


#People who selected SDS on ABC Eligible Quote
#selectSDSFromLondonBA
selectSDS_ABCEligible.data <- mergedBooking.data[,c("Booking_date","selectSDSFromLondonBA","quoteFromLondonAndBA", "selectABC")]
selectSDS_ABCEligible.plot <- gvisLineChart(selectSDS_ABCEligible.data,
                                          options=list(height= 800,
                                                       title= "SDS Selections on ABC Eligible Quotes "))
plot(selectSDS_ABCEligible.plot)


##############################################################################################################
###Product Statistics

#Summarise booking data by time of flight departure
byTimeFlightDep.data <- summariseBookingData(notCancelled.bookings, "clean_flt_time")
byTimeFlightDep.data <- byTimeFlightDep.data[with(byTimeFlightDep.data, order(clean_flt_time)), ]
#plot
byTimeFlightDep.plot <- byTimeFlightDep.data[,c("clean_flt_time","salesNumber")]
Column_byTimeFlightDep.plot <- gvisColumnChart(byTimeFlightDep.plot,
                                               options=list(height= 800,
                                                            title= "Popular Flight Departure times"))
plot(Column_byTimeFlightDep.plot)

#Summarise booking data by time of collection
byTimeColl.data <- summariseBookingData(notCancelled.bookings, "clean_coll_time")
byTimeColl.data <- byTimeColl.data[with(byTimeColl.data, order(clean_coll_time)), ]
#plot
byTimeColl.plot <- byTimeColl.data[,c("clean_coll_time","salesNumber")]
Column_byTimeColl.plot <- gvisColumnChart(byTimeColl.plot,
                                          options=list(height= 800,
                                                       title= "Popular Collection Times"))
plot(Column_byTimeColl.plot)

#Summarise booking data by booking completion time
byTimeComplete.data <- summariseBookingData(notCancelled.bookings, "clean_Complete_time")
byTimeComplete.data <- byTimeComplete.data[with(byTimeComplete.data, order(clean_Complete_time)), ]
#plot
byTimeComplete.plot <- byTimeComplete.data[,c("clean_Complete_time","salesNumber")]
Column_byTimeComplete.plot <- gvisColumnChart(byTimeComplete.plot,
                                          options=list(height= 800,
                                                       title= "Popular Booking Completion Times"))
plot(Column_byTimeComplete.plot)

#Summarise booking data by collection Zone
notCancelled.bookings$clean_Zone <- substr(notCancelled.bookings$Zone, 6, 11)
byCollZone.data <- summariseBookingData(notCancelled.bookings, "clean_Zone")
byCollZone.data <- byCollZone.data[with(byCollZone.data, order(clean_Zone)), ]
#plot
byCollZone.plot <- byCollZone.data[,c("clean_Zone","salesNumber")]
Column_byCollZone.plot <- gvisColumnChart(byCollZone.plot,
                                          options=list(height= 800,
                                                       title= "Popular Collection Zones"))
plot(Column_byCollZone.plot)

#Summarise ABC booking data by Number of Bags
#plot
byBagsBooking.plot <- byBagsBooking.data[,c("Hold_luggage_No","salesNumber")]
Column_byBagsBooking.plot <- gvisColumnChart(byBagsBooking.plot,
                                          options=list(height= 800,
                                                       title= "Number of HOLD Bags per ABC booking"))
plot(Column_byBagsBooking.plot)

#Summarise ABC booking data by Number of Pax
#plot
byPaxBooking.plot <- byPaxBooking.data[,c("Total_passengers","salesNumber")]
Column_byPaxBooking.plot <- gvisColumnChart(byPaxBooking.plot,
                                             options=list(height= 800,
                                                          title= "Number of Pax per ABC booking"))
plot(Column_byPaxBooking.plot)

### Data Summary Tables ###

#Summarise booking data by promo code
byPromo.data <- summariseBookingData(notCancelled.bookings, "Promocodes")
byPromo.data <- byPromo.data[with(byPromo.data, order(-salesNumber)), ]
Table_byPromo.data <- gvisTable(byPromo.data, 
                   formats=list(meanBags="#.#", meanValue="£ #.##", totalRevenue="£ #,###", totalBookingValue="£ #,###", totalPromoValue="£ #,###"))
plot(Table_byPromo.data)

#Summarise booking data by flight number
byFlight.data <- summariseBookingData(notCancelled.bookings, "Out.bound_flt_code")
byFlight.data <- byFlight.data[with(byFlight.data, order(-salesNumber)), ]
Table_byFlight.data <- gvisTable(byFlight.data, 
                                formats=list(meanBags="#.#", meanValue="£ #.##", totalRevenue="£ #,###", totalBookingValue="£ #,###", totalPromoValue="£ #,###"))
plot(Table_byFlight.data)

#Summarise booking data by Destination City
byCity.data <- summariseBookingData(notCancelled.bookings, "Out.bound_dest_city_name")
byCity.data <- byCity.data[with(byCity.data, order(-salesNumber)), ]
Table_byCity.data <- gvisTable(byCity.data, 
                                 formats=list(meanBags="#.#", meanValue="£ #.##", totalRevenue="£ #,###", totalBookingValue="£ #,###", totalPromoValue="£ #,###"))
plot(Table_byCity.data)

#Summarise booking data by Destination Country
byCountry.data <- summariseBookingData(notCancelled.bookings, "Out.bound_dest_country_name")
byCountry.data <- byCountry.data[with(byCountry.data, order(-salesNumber)), ]
Table_byCountry.data <- gvisTable(byCountry.data, 
                               formats=list(meanBags="#.#", meanValue="£ #.##", totalRevenue="£ #,###", totalBookingValue="£ #,###", totalPromoValue="£ #,###"))
plot(Table_byCountry.data)

#Summarise booking data by user
byEmail.data <- summariseBookingData(notCancelled.bookings, "customer_email")
byEmail.data <- byEmail.data[with(byEmail.data, order(-salesNumber)), ]
Table_byEmail.data <- gvisTable(byEmail.data, 
                                  formats=list(meanBags="#.#", meanValue="£ #.##", totalRevenue="£ #,###", totalBookingValue="£ #,###", totalPromoValue="£ #,###"))
plot(Table_byEmail.data)

### Operational Plots
# byCollectionDate.opsData

# Planned and Past Collections Calendar Heatmap
Cal <- gvisCalendar(byCollectionDate.opsData, 
                    datevar="Outward_Journey_Luggage_Collection_date", 
                    numvar="collections",
                    options=list(
                      title="Daily Collection Density",
                      height=800,
                      calendar="{
                               cellSize: 10,
                               cellColor: { stroke: 'black', strokeOpacity: 0.2 },
                               focusedCellColor: {stroke:'red'}}")
)
plot(Cal)

# Pct Reassign
reasignRates.data <- byCollectionDate.opsData[,c("Outward_Journey_Luggage_Collection_date","collections","nbrReassignedBkgs")]
reasignRates.data <- reasignRates.data[(reasignRates.data$Outward_Journey_Luggage_Collection_date<=Sys.Date()),]
reasignRates.plot <- gvisLineChart(reasignRates.data,
                                          options=list(height= 800,
                                                       title= "Reassign rates by collection date"))
plot(reasignRates.plot)

pctReasignRates.data <- byCollectionDate.opsData[,c("Outward_Journey_Luggage_Collection_date","pctReassigBkgs")]
pctReasignRates.data <- pctReasignRates.data[(pctReasignRates.data$Outward_Journey_Luggage_Collection_date<=Sys.Date()),]
pctReasignRates.plot <- gvisLineChart(pctReasignRates.data,
                                   options=list(height= 800,
                                                title= "Reassign rates by collection date"))
plot(pctReasignRates.plot)

##########################################################################################################
###Maps

ABC.locations <- data.frame(notCancelled.bookings$Outward_Journey_Luggage_collection_location_addresss_Postcode,notCancelled.bookings$Outward_Journey_Luggage_Collection_datetime)
names(ABC.locations) <- c('postcode','tips')

short_ABC.locations <- head(ABC.locations,n=30)

AndrewMap <- gvisMap(short_ABC.locations, 'postcode',"tips",
                     options=list(showTip=TRUE,
                                  showLine=TRUE,
                                  enableScrollWheel=TRUE,
                                  mapType='terrain',
                                  useMapTypeControl=TRUE))
plot(AndrewMap)


df <- data.frame(Postcode=c("EC3M 7HA", "EC2P 2EJ"),
                 Tip=c("<a href='http://www.lloyds.com'>Lloyd's</a>",
                       "<a href='http://www.guildhall.cityoflondon.gov.uk/'>Guildhall</a>"))

M2 <- gvisMap(df, "Postcode", "Tip",
              options=list(showTip=TRUE, mapType='normal',
                           enableScrollWheel=TRUE))

plot(M2)

M2 <- gvisMap(ABC.locations, "postcode", "tips",
              options=list(showTip=TRUE, mapType='normal',
                           enableScrollWheel=TRUE))

plot(M2)

# ## Change mapping icons
# M3 <- gvisMap(df, "Postcode", "Tip",
#               options=list(showTip=TRUE, mapType='normal',
#                            enableScrollWheel=TRUE,
#                            icons=paste0("{",
#                                         "'default': {'normal': 'http://icons.iconarchive.com/",
#                                         "icons/icons-land/vista-map-markers/48/",
#                                         "Map-Marker-Ball-Azure-icon.png',\n",
#                                         "'selected': 'http://icons.iconarchive.com/",
#                                         "icons/icons-land/vista-map-markers/48/",
#                                         "Map-Marker-Ball-Right-Azure-icon.png'",
#                                         "}}")))
# 
# plot(M3)

###########################################################################################################

# Create A KPI Table
#Calculate Average Reassignment Rate
ReassignAvg <- sum(reasignRates.data$nbrReassignedBkgs)/sum(reasignRates.data$collections)*100


