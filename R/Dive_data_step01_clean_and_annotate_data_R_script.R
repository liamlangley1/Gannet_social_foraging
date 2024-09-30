####Script to clean and annotate GPS and dive data
####Liam Langley

#----------------------#
##1. load libraries ####
#----------------------#

library(tidyverse)
library(lubridate)
library(sf)
library(sfheaders)
library(here)


#-----------------------------------#
##2. Read in and manipulate data ####
#-----------------------------------#

## read in GPS data
## all GPS locations interpolated to one minute

df_points <- read_csv(here("Data", "Dive_data", "GannetGPS_DiveEvent_OneMinute_Oceanography_AllYears.csv"))

head(df_points)
str(df_points)

## focus on 2012 and 2013 data
## remove other years

df_points <- subset(df_points, df_points$Year %in% c("2012", "2013"))

## DateTime column in an odd format 
## create a replacement column by pasting Date and Time columns together
## use function from the lubridate package per Sam Cox

df_points$DateTime2 <- dmy_hms(paste0(df_points$Date, ' ', df_points$Time))

## sex column currently binary 1/0
## make character columns for plotting

df_points$sexchr <- ifelse(df_points$Sex == 0, "Male", "Female")

str(df_points)

## load in processed dive data
## contains spatial location of each dive, dive shape (U/V) and oceanography

df_dives <- read_csv(here("Data", "Dive_data","GannetDiveSummary_Oceanography_AllYears.csv"))

head(df_dives)
str(df_dives)

## focus on 2012 and 2013 data
## remove other years

df_dives<- subset(df_dives, df_dives$Year %in% c("2012", "2013"))

## DateTime column in an odd format 
## create a replacement column by pasting Date and Time columns together
## use function from the lubridate package per Sam Cox

df_dives$DateTime2 <- dmy_hms(paste0(df_dives$Date, ' ', df_dives$Time))

## dive shape and sex columns currently binary 1/0
## make character columns for plotting

df_dives$diveshpchr <- ifelse(df_dives$DiveShape == 0, "V", "U")

df_dives$sexchr <- ifelse(df_dives$Sex == 0, "Male", "Female")


## create bird_trip_year columns to loop through for both data frames

df_points$bird_trip_year <- paste0(df_points$BirdID, "_", df_points$Trip, "_", 
                                   df_points$Year)

df_dives$bird_trip_year <- paste0(df_dives$BirdID, "_", df_dives$Trip, "_", 
                                  df_dives$Year)


#-----------------------------------------------------------------------#
##3. Calculate proportion of trip time elapsed for each GPS location ####
#-----------------------------------------------------------------------#

## loop through each bird_trip_year and calculate sttime, entime, trip_dur
## time elapsed for each point and proportion of total trip time elapsed for each point
## create a data frame to loop in to

results <- as.data.frame(matrix(ncol = 36, nrow = 0))

## set up loop

for(j in unique(df_points$bird_trip_year)){
  
  trip <- df_points[df_points$bird_trip_year == j, ]
  
  ## first calculate trip start and end times
  
  trip$Sttime <- trip$DateTime2[1]
  
  trip$Entime <- trip$DateTime2[length(trip$DateTime2)]  
  
  ## calculate amount of time elapsed for each GPS location
  
  trip$difftime <- difftime(trip$DateTime2,trip$Sttime,units="mins")
  
  ## calculate total trip duration
  
  trip$trip_dur <- difftime(trip$Entime,trip$Sttime,units="mins")
  
  ## calculate proportion of trip time elapsed for each location
  
  trip$ppn_trip <- as.numeric(trip$difftime)/as.numeric(trip$trip_dur)
  
  ## create a column for total number of dive events
  
  trip$total_diveevents <- sum(trip$DiveEvent)
  
  ## create a column for cumulative dive events
  
  trip$cumulative_diveevents <- cumsum(trip$DiveEvent)
  
  ## create a column for proportion of total dive events already performed
  
  trip$ppn_diveevents <- trip$cumulative_diveevents/trip$total_diveevents
  
  ## create a column for number of dives previously performed
  
  trip$nprevdiveevents <- 0
  
  for (i in 2:nrow(trip)){
    
    trip$nprevdiveevents[i] <- trip$cumulative_diveevents[i - 1]
  }
  
  ## create a column for proportion of total dives already performed
   
  trip$ppn_prevdiveevents <- trip$nprevdiveevents/trip$total_diveevents
  
  ## append dataframe to results
  
  results <- rbind(results, trip)
  
}

## rename results df

df_points2 <- results


#------------------------------------------------------------#
##4. Annotate GPS locations with distance from the colony ####
#------------------------------------------------------------#

## calculate the distance from the colony for each GPS location point
## use the st_distance function from sf
## first convert df_points2 to an sf object

points.sf <- st_as_sf(df_points2, coords = c("Lon", "Lat"))

## set co-ordinate projection to lat/long with WGS84
## Use EPSG code - 4326

st_crs(points.sf) <- 4326

## check co-ordinate projection has worked

st_crs(points.sf)

## create an sf object for the colony location

colony <- data.frame(x = -5.467, y = 51.716)

colony.sf <- st_as_sf(colony, coords = c("x", "y"))

st_crs(colony.sf) <- 4326

## calaculate distance from colony for every GPS location
## great circle distance as default

points.sf$distance <- st_distance(points.sf$geometry, colony.sf)

## convert points back to a dataframe for annotation

df_points3 <- sf_to_df(points.sf, fill = TRUE)

## convert distance column to numeric

df_points3$distance <- as.numeric(df_points3$distance)

## loop through for each trip
## calculate max distance from colony, time stamp at distal location
## create a variable for inbound/outbound leg
## first create data frame to loop into

results <- as.data.frame(matrix(ncol = 48, nrow = 0))

## create loop

for(j in unique(df_points3$bird_trip_year)){
  
  trip <- df_points3[df_points3$bird_trip_year == j, ]
  
  ## get max value for distance
  
  distmax <- max(trip$distance)
  
  ## create a column for distmax
  
  trip$distmax <- distmax
  
  ## calculate timestamp of distal point
  ## subset data frame for distal location
  
  distal <- trip[trip$distance == distmax, ]
  
  ## create column for time at distal location
  
  trip$disttime <- distal$DateTime2
  
  ## create columns for latitude and longitude at the distal point
  
  trip$distlon <- distal$x
  
  trip$distlat <- distal$y
  
  ## calculate distance between all locations and distal point
  ## first converttrip to an sf object
  
  trip.sf <- st_as_sf(trip, coords = c("x", "y"))
  
  ## set co-ordinate projection to lat/long with WGS84
  ## Use EPSG code - 4326
  
  st_crs(trip.sf) <- 4326
  
  ## create an sf object for the distal point
  
  distal.sf <- st_as_sf(distal, coords = c("x", "y"))
  
  st_crs(distal.sf) <- 4326
  
  ## calaculate distance from distal point for every GPS location
  ## great circle distance as default
  
  trip.sf$distaldistance <- st_distance(trip.sf$geometry, distal.sf)
  
  ## convert trip back to a dataframe
  
  trip2 <- sf_to_df(trip.sf, fill = TRUE)
  
  ## calculate difftime object for distal point location
  ## how many minutes elapsed at distal point
  
  trip2$distdifftime <- difftime(trip2$disttime, trip2$Sttime, units = "mins")
  
  ## create variable for leg - inbound/outbound
  ## before or after distal time stamp
  ## use ifelse function
  
  trip2$leg <- ifelse(trip2$DateTime2 < trip2$disttime, "Outbound",
                      ifelse(trip2$DateTime2 == trip2$disttime, "Distal", "Inbound"))
 
  ## append data frame to results
  
  results <- rbind(results, trip2)
  
}

## rename results data frame

df_points4 <- results

## remove columns related to sf objects

df_points5 <- subset(df_points4, select = -c(sfg_id, point_id, sfg_id..1, point_id..1))

## create a second column for distance from distal point
## negative values on outbound leg and positive values on inbound leg
## for plotting

df_points5$distaldistance2 <- ifelse(df_points5$leg == "Outbound", paste(0 - as.numeric(df_points5$distaldistance)),
                                     paste(as.numeric(df_points5$distaldistance)))


#---------------------------------------#
##5. Annotate points with trip stats ####
#---------------------------------------#

## read in tripstats data from Sam Cox
## tripstats for complete trips in 2012 and 2013 only

df_stats <-read_csv(here("Data", "Dive_data", "GannetSummaryStatistics_CompleteTrips_2012_2013.csv"))

head(df_stats)
str(df_stats)

## join data frames

df_points6 <- full_join(df_points5, df_stats, by = c("BirdID", "Trip", "Sex"))

## remove GPS points from incomplete trips
## those with NAs in trip stats columns from Sam Cox data

df_points7 <- subset(df_points6, !is.na(df_points6$TripMaxDisplacement))

## some trips with no dive events
## remove GPS points from these trips

df_points8 <- subset(df_points7, df_points7$NoDivesPerformed != 0)

## save as a csv file
## define filepath for the saved file

filepath_dfout <- here("Data_outputs","Working_data_frames")

## save dataframe as .csv

write_csv(df_points8, file = here(filepath_dfout, "tracking_data_1213_cleaned_and_annotated.csv"))


#--------------------------#
##6. Annotate dive data ####
#--------------------------#

## need to annotate dive data with trip information to calculate metrics
## proportion of trip time elapsed, inbound/outbound leg etc
## need to bind trip metrics to dives using full_join
## first subset GPS points data for relevant columns

df_metrics <- df_points8[, c("bird_trip_year", "Sttime", "Entime", "trip_dur",
                             "distmax", "disttime", "distdifftime", "distlon", "distlat", 
                             "TripMaxDisplacement", "TripDuration", "TripLength", 
                             "NoDivesPerformed", "DiveDurationUnderwater" )]

## select the first row for each bird_trip_year
## use the duplicated function
## convert bird_trip_year to a factor

df_metrics$bird_trip_year <- as.factor(as.vector(df_metrics$bird_trip_year))

## select first row for each trip

df_metrics2 <- df_metrics[!duplicated(df_metrics$bird_trip_year), ]

## join to dive data using full_join

df_dives2 <- full_join(df_dives, df_metrics2, with = c("bird_trip_year"))

## remove trips with no dives

df_dives2 <- subset(df_dives2, !is.na(df_dives2$BirdID))

## remove dives from incomplete trips
## those with NAs in trip stats columns from Sam Cox data

df_dives3 <- subset(df_dives2, !is.na(df_dives2$TripMaxDisplacement))

## calculate proportion of trip time elapsed for each dive
## create a data frame to loop in to

results <- as.data.frame(matrix(ncol = 49, nrow = 0))

## set up loop

for(j in unique(df_dives3$bird_trip_year)){
  
  trip <- df_dives3[df_dives3$bird_trip_year == j, ]
  
  ## calculate amount of time elapsed for each GPS location
  
  trip$difftime <- difftime(trip$DateTime2,trip$Sttime,units="mins")
  
  ## calculate proportion of trip time elapsed for each location
  
  trip$ppn_trip <- as.numeric(trip$difftime)/as.numeric(trip$trip_dur)
  
  ## create a column for total dives per trip
  
  trip$total_dives <- nrow(trip)
  
  ## make a cumulative dives column
  
  trip$ndives <- seq.int(nrow(trip))
  
  ## create a column for proportion of total dives already performed
  
  trip$ppn_dives <- trip$ndives/trip$total_dives
  
  ## create a column for number of dives previously performed
  ## calculate number of rows and minus 1
  
  trip$nprevdives <- trip$ndives - 1
  
  ## create a column for proportion of total dives already performed
  
  trip$ppn_prevdives <- trip$nprevdives/trip$total_dives
  
  ## append dataframe to results
  
  results <- rbind(results, trip)
  
}


## rename results df

df_dives4 <- results

## calculate distance from colony for each dive
## convert dives to an sf object

dives.sf <- st_as_sf(df_dives4, coords = c("Lon", "Lat"))

## set co-ordinate projection to lat/long with WGS84
## Use EPSG code - 4326

st_crs(dives.sf) <- 4326

## check co-ordinate projection has worked

st_crs(dives.sf)

## calaculate distance from colony for every GPS location
## great circle distance as default

dives.sf$distance <- st_distance(dives.sf$geometry, colony.sf)

## convert points back to a dataframe for annotation

df_dives5 <- sf_to_df(dives.sf, fill = TRUE)

## convert distance column to numeric

df_dives5$distance <- as.numeric(df_dives5$distance)

## assign dive locations to inbound/outbound leg

df_dives5$leg <- ifelse(df_dives5$DateTime2 < df_dives5$disttime, "Outbound",
                        ifelse(df_dives5$DateTime2 == df_dives5$disttime, "Distal", "Inbound"))

## calculate the distance between each dive and the distal trip location
## loop through for each trip and make this calculation
## first create data frame to loop into

results <- as.data.frame(matrix(ncol = 56, nrow = 0))

## create loop

for(j in unique(df_dives5$bird_trip_year)){
  
    trip <- df_dives5[df_dives5$bird_trip_year == j, ]
    
    ## calculate distance between all locations and distal point
    ## first convert trip to an sf object
    
    trip.sf <- st_as_sf(trip, coords = c("x", "y"))
    
    ## set co-ordinate projection to lat/long with WGS84
    ## Use EPSG code - 4326
    
    st_crs(trip.sf) <- 4326
  
    ## create an sf object for the distal point
    ## subset for first row of trip
    
    row <- trip[1, ]
    
    distal <- data.frame(x = row$distlon, y = row$distlat)
  
    distal.sf <- st_as_sf(distal, coords = c("x", "y"))
  
    st_crs(distal.sf) <- 4326
  
    ## calaculate distance from distal point for every GPS location
    ## great circle distance as default
  
    trip.sf$distaldistance <- st_distance(trip.sf$geometry, distal.sf)
  
    ## convert trip back to a dataframe
  
    trip2 <- sf_to_df(trip.sf, fill = TRUE)
  
    ## append data frame to results
  
    results <- rbind(results, trip2)
  
}

## rename results data frame

df_dives6 <- results

## remove columns related to sf objects

df_dives7 <- subset(df_dives6, select = -c(sfg_id, point_id, sfg_id..1, point_id..1))

## create a second column for distance from distal point
## negative values on outbound leg and positive values on inbound leg
## for plotting

df_dives7$distaldistance2 <- ifelse(df_dives7$leg == "Outbound", paste(0 - as.numeric(df_dives7$distaldistance)),
                                    paste(as.numeric(df_dives7$distaldistance)))


## standardise all dives to time before/after the distal location 
## in order to look at clustering around this point
## make column for time standardised to distal point

df_dives7$diststandard <- difftime(df_dives7$DateTime2, df_dives7$disttime,units="mins")

## add columns for difftime between start and end times and distal point

df_dives7$diffsttime <- difftime(df_dives7$Sttime, df_dives7$disttime,units="mins")

df_dives7$diffentime <- difftime(df_dives7$Entime, df_dives7$disttime,units="mins")

## create a column for the distance from the colony as a proportion of the maximum distance reached

df_dives7$ppn_distance <- df_dives7$distance/df_dives7$distmax

## create an hour column to visualise dive distribution across the diel cycle

df_dives7$hour <- hour(df_dives7$DateTime2)

## save as a csv file

write_csv(df_dives7, file = here(filepath_dfout, "dive_data_1213_cleaned_and_annotated.csv"))


#------------------#
##End of script ####
#------------------#