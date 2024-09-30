####Script to visualise spatial distirbution of social foraging behaviour
####Liam Langley
####Date created - 11/03/2024

#----------------------#
##1. Load libraries ####
#----------------------#

library(ggplot2)
library(grid)
library(mapdata)
library(mapproj)
library(rgdal)
library(raster)
library(rgeos)
library(RColorBrewer)
library(tidyverse)
library(patchwork)
library(sf)
library(sfheaders)
library(marmap)
library(lubridate)
library(ggnewscale)
library(ggpubr)
library(here)
library(patchwork)
library(magick)
library(cowplot)


#---------------------------#
##2. Read in camera data ####
#---------------------------#

## Set filepath for folder containing raw data files
## NB: this code will try to open all files matching the file pattern within this folder
## Therefore, it is best if this folder only contains the raw data files
filepath <- here("Data", "Camera_data") 

## Define common file pattern to look for
## An asterisk (*) matches any character except a forward-slash
## e.g., "*.csv" will import all files within filepath folders that end with ".csv"
filepattern <- "*.csv" 

## Let's view the file names, to check that we have the files we want & find ID position
## This will include names of sub-folders
ls_filenames <- list.files(path = filepath, recursive = TRUE, pattern = filepattern)
ls_filenames

## Find ID number from file name (excluding names of sub-folders)
## This will only work if ID numbers are the same length and position in all file names to be imported

IDstart <- 1 #start position of the ID in the filename 

IDend <- 4 #end position of the ID in the filename

## Now, let's inspect the data by reading in the top of the first data file as raw text

test <- fs::dir_ls(path = filepath, recurse = TRUE, type = "file",  glob = filepattern)[1]
read_lines(test, n_max = 5)  # change n_max to change the number of rows to read in

## number of lines at top of file to skip (e.g., if importing a text file with additional info at top)

skiplines <- 0

## define date format(s) used (for passing to lubridate)
## "d"=day as decimal, "m"=month as decimal, "y"=year w/o century (2 digits), "Y"=year w/ century (4 digits)
## Here, we've included common combinations, modify if your data include a different format

date_formats <- c("dmY", "Ymd") #specify date formats (e.g. "dmY" works for 01-12-2022 and 01/12/2022)
datetime_formats <- c("dmY HMS", "Ymd HMS") #specify date & time format 

## define time zone for tracking data 

trackingdatatimezone <- "GMT"

## By default, the below code will find column names from the first row of data: colnames <- TRUE

colnames <- TRUE

## Set delimiter to use within read_delim

user_delim <- ","
user_trim_ws <- TRUE # Should leading and trailing whitespace (ASCII spaces and tabs) be trimmed from each field before parsing it?

## Read in and merge camera data files

df_combined <- fs::dir_ls(path = filepath, glob = filepattern, #use our defined filepath and pattern
                          type = "file",  recurse = TRUE) %>% # recurse = T searches all sub-folders
  purrr::set_names(nm = basename(.)) %>% # removing path prefix (makes filename more manageable)
  purrr::map_dfr(read_delim, .id="filename", #read all the files in using filename as ID column
                 col_types = cols(.default = "c"), col_names = colnames, 
                 skip = skiplines, delim = user_delim, trim_ws = user_trim_ws,
                 skip_empty_rows = TRUE) %>% 
  mutate(ID = str_sub(string = filename, start = IDstart, end = IDend), #substring ID from the filename (start to end of substring)
         .after = filename) #position the new ID column after filename column
df_combined

##check column names

colnames(df_combined)

#-------------------------------------#
##3. Clean camera data for mapping ####
#-------------------------------------#

## some empty rows in G153 due to issues with excel files
## remove these
## replace NAs with "CAMERA OFF" in camera behaviour column
## Day column only filled in for some birds - update based on "light" column

df_cam <- subset(df_combined, !is.na(df_combined$bird_ID)) %>%
  mutate(CAMERA_BEH = replace_na(CAMERA_BEH, "CAMERA OFF")) %>%
  mutate(DAY = ifelse(light == TRUE, "D", "N"))

## remove colony points
## first check levels of CAMERA_BEH

levels(as.factor(df_cam$CAMERA_BEH))

df_trip <- df_cam %>%
  filter(CAMERA_BEH != "COLONY")

## make a composite column of camera behaviour and day/night


df_trip$temp <- ifelse(df_trip$DAY == "N", "N", paste(df_trip$CAMERA_BEH))

## make a column of camera behaviours during the day
## 7 level factor
## Camera off, Night, Rest, Solo Travel, Group Travel, Solo Foraging, Group Foraging

levels(as.factor(df_trip$temp))

df_trip$CamB_Clean <- ifelse(df_trip$temp == "CAMERA OFF", "Camera Off",
                             ifelse(df_trip$temp == "N", "Night",
                                    ifelse(df_trip$temp == "REST", "Rest", 
                                           ifelse(df_trip$temp == "TRAV", "Travel: solo", 
                                                  ifelse(df_trip$temp == "FOLLOWED", "Travel: group",
                                                         ifelse(df_trip$temp %in% c("OPPFOR", "SERFOR", "VESSEL", "FORAGE"), 
                                                                "Foraging: solo", "Foraging: group"))))))

##check no NA values in the new column

test <- subset(df_trip, is.na(df_trip$CamB_Clean))

## convert camera data to an sf object to add co-ordinate projection
## to match with basemap to set plot limits for mapping

trip.sf <- st_as_sf(df_trip, coords = c("LON", "LAT"))

## set co-ordinate projection to lat/long with WGS84
## Use EPSG code - 4326

st_crs(trip.sf) <- 4326

## check crs

st_crs(trip.sf)


#------------------------------------#
##4. Perform distance calculations####
#------------------------------------#

## calculate distance to colony for all points
## first create an sf object for the colony location

colony <- data.frame(x = -5.467, y = 51.716)

colony.sf <- st_as_sf(colony, coords = c("x", "y"))

st_crs(colony.sf) <- 4326

##calaculate distance from colony for every GPS location
##great circle distance as default

trip.sf$distance <- st_distance(trip.sf$geometry, colony.sf)

## convert dives.sf back to a data frame for annotation

df_distance <- sf_to_df(trip.sf, fill = TRUE)

## convert distance column to numeric

df_distance$distance <- as.numeric(df_distance$distance)

## loop through for each trip
## calculate max distance from colony, time stamp at distal location
## create a variable for inbound/outbound leg
## calculate distance to the distal point for all locations
## first create data frame to loop into

results <- as.data.frame(matrix(ncol = 38, nrow = 0))

## create loop

for(j in unique(df_distance$bird_ID)){
  
  trip <- df_distance[df_distance$bird_ID == j, ]
  
  ##get max value for distance
  
  distmax <- max(trip$distance)
  
  ##create a column for distmax
  
  trip$distmax <- distmax
  
  ##calculate timestamp of distal point
  ##subset data frame for distal location
  
  distal <- trip[trip$distance == distmax, ]
  
  ##create column for time at distal location
  
  trip$disttime <- distal$date
  
  ##create columns for latitude and longitude at the distal point
  
  trip$distlon <- distal$x
  
  trip$distlat <- distal$y
  
  
  ##create variable for leg - inbound/outbound
  ##before or after distal time stamp
  ##use ifelse function
  
  trip$leg <- ifelse(trip$date <= trip$disttime, "Outbound", "Inbound")
  
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



## remove points within 2km of the colony
## remove bird G149 trip 2 - no camera data
## create a second column for distance from distal point
## negative values on outbound leg and positive values on inbound leg
## for visualisation
## convert new distance to distal point column to kilometres

df_trip_final <- results %>%
  filter(distance > 2000) %>%
  mutate(trip = as.factor(trip)) %>%
  filter(trip != "G149_2") %>%
  mutate(distaldistance = as.numeric(distaldistance))%>%
  mutate(distaldistance2 = ifelse(leg == "Outbound", paste(0 - distaldistance),
                                   paste(distaldistance))) %>%
  mutate(distaldistance2 = as.numeric(distaldistance2))%>%
  mutate(distaldistkm = (distaldistance2/1000))


#----------------------------------------------#
##5. Save cleaned data for further analysis ####
#----------------------------------------------#

## specify filepath for read out

outpath <- paste0(here("Data_outputs", "Working_data_frames"), "/Camera_data_cleaned_all_birds.csv")

write.table(df_trip_final, outpath, sep = ",",
            row.names = FALSE, col.names = TRUE)

## filter data frame to retain birds with camera data for complete trips only
## G149 and G150 removed

df_trip_complete <- df_trip_final %>%
  filter(bird_ID %in% c("G145", "G147", "G151", "G152", "G153", "G156"))


## save as a data frame for further analysis
## specify filepath

outpath <- paste0(here("Data_outputs", "Working_data_frames"), "/Camera_data_cleaned_complete_trips.csv")

write.table(df_trip_complete, outpath, sep = ",",
            row.names = FALSE, col.names = TRUE)


#-------------------#
## End of script ####
#-------------------#