####Script to make a breakdown of the gannet social interactions for each behaviour from camera data
####Liam Langley

#----------------------#
##1. Load libraries ####
#----------------------#

library(tidyverse)
library(here)
library(sf)
library(sfheaders)


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


#-------------------------#
##3. Clean camera data ####
#-------------------------#

## convert CAMERA_BEH column to a factor

df_combined$CAMERA_BEH <- as.factor(as.vector(df_combined$CAMERA_BEH))

## some empty rows due to issues with excel files
## remove these
## also need to remove points where we don't have data from the cameras
## can't tell whether the behaviour is social
## remove colony points and points where camera stops
## make a social/ not column (1/0) based on camera data
## make a day/night column standardised across all birds by combining day with light column

df_cam <- subset(df_combined, !is.na(df_combined$bird_ID)) %>%
  filter(!is.na(CAMERA_BEH)) %>%
  filter(CAMERA_BEH != c("COLONY"))%>%
  filter(CAMERA_BEH != c("CAMERA STOPS"))%>%
  mutate(BEH = ifelse(PUT_BEH_24hr == "REST", "Rest",
               ifelse(PUT_BEH_24hr == "TRAV", "Travel", "Foraging"))) %>%
  mutate(SOCIAL = ifelse(CAMERA_BEH %in% c("REST", "OPPFOR", "SERFOR", "VESSEL", "TRAV", "GANNET ON WATER"), 0, 1)) %>%
  mutate(DAY = ifelse(light == TRUE, "D", "N"))

## convert camera data to an sf object to add co-ordinate projection
## to match with basemap to set plot limits for mapping

trip.sf <- st_as_sf(df_cam, coords = c("LON", "LAT"))

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

##convert distance column to numeric

df_distance$distance <- as.numeric(df_distance$distance)

##loop through for each trip
##calculate max distance from colony, time stamp at distal location
##create a variable for inbound/outbound leg
##first create data frame to loop into

results <- as.data.frame(matrix(ncol = 36, nrow = 0))

##create loop

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
  
  ##append data frame to results
  
  results <- rbind(results, trip)
  
}


## remove points within 2km of the colony

df_final <- results %>%
  filter(distance > 2000)


#--------------------------------------------#
##5. Summarise camera behaviours all birds####
#--------------------------------------------#

## first summarise the number of fixes in each behaviour and calculate as a percentage of the total fixes
## create a sum column to aggregate by

df_final$sum <- 1

## summarise number of fixes by behaviour
## foraging resting and travelling

a <- with(df_final, aggregate(sum, by = list(BEH), "sum"))

names(a)[1] <- "Behaviour"
names(a)[2] <- "Total_Fixes"

## summarise number of fixes by behaviour and social interactions

b <- with(df_final, aggregate(sum, by = list(BEH, SOCIAL), "sum"))

names(b)[1] <- "Behaviour"
names(b)[2] <- "Social"
names(b)[3] <- "N_fixes"

## join data frames together

df_beh <- left_join(a, b, with = Behaviour)

## calculate percentage of behaviours where social interactions occur

df_beh$percent_fixes <- (df_beh$N_fixes/df_beh$Total_Fixes)*100

## save as a csv file
## define filepath for saved file

filepath_dfout <- here("Data_outputs","Summary_data_frames")

## save file

write_csv(df_beh, file = here(filepath_dfout, "camera_data_summary_all_birds.csv"))

## calculate percentages for total behaviours
## for manuscript table

a$overall_fixes <- sum(df_final$sum)

a$percentage_fixes <- (a$Total_Fixes/a$overall_fixes)*100

## remove temporary objects

rm(a, b, df_beh)


#----------------------------------------------#
##6. Summarise camera behaviours individuals####
#----------------------------------------------#

## summarise number of fixes by behaviour and bird ID
## foraging resting and travelling

a <- with(df_final, aggregate(sum, by = list(bird_ID, BEH), "sum"))

names(a)[1] <- "birdID"
names(a)[2] <- "Behaviour"
names(a)[3] <- "Total_Fixes"

## summarise number of fixes by behaviour and social interactions

b <- with(df_final, aggregate(sum, by = list(bird_ID, BEH, SOCIAL), "sum"))

names(b)[1] <- "birdID"
names(b)[2] <- "Behaviour"
names(b)[3] <- "Social"
names(b)[4] <- "N_fixes"

## join data frames together

df_beh <- left_join(a, b, with = c(birdID, Behaviour))

## calculate percentage of behaviours where social interactions occur

df_beh$percent_fixes <- (df_beh$N_fixes/df_beh$Total_Fixes)*100

## save as a csv file
## define filepath for saved file

filepath_dfout <- here("Data_outputs","Summary_data_frames")

## save file

write_csv(df_beh, file = here(filepath_dfout, "camera_data_summary_individuals.csv"))


#---------------------------------------------------#
##7. Summarise camera behaviours by leg all birds####
#---------------------------------------------------#
 
## filter data to remove birds where camera fails before colony return

df_complete <- df_final %>%
  filter(bird_ID != "G149" & bird_ID != "G150")

## summarise number of fixes by behaviour and leg
## foraging resting and travelling

a <- with(df_complete, aggregate(sum, by = list(BEH, leg), "sum"))

names(a)[1] <- "Behaviour"
names(a)[2] <- "leg"
names(a)[3] <- "Total_Fixes"

## summarise number of fixes by behaviour, leg and social interactions

b <- with(df_complete, aggregate(sum, by = list(BEH, leg, SOCIAL), "sum"))

names(b)[1] <- "Behaviour"
names(b)[2] <- "leg"
names(b)[3] <- "Social"
names(b)[4] <- "N_fixes"

## join data frames together

df_beh <- left_join(a, b, with = c(Behaviour, leg))

## calculate percentage of behaviours where social interactions occur

df_beh$percent_fixes <- (df_beh$N_fixes/df_beh$Total_Fixes)*100

## save file

write_csv(df_beh, file = here(filepath_dfout, "camera_data_summary_all_birds_by_leg.csv"))

## calculate percentages for total behaviours on each leg
## for manuscript table
## calculate number of fixes by inbound vs. outbound

c <- with(df_complete, aggregate(sum, by = list(leg), "sum"))

names(c)[1] <- "leg"
names(c)[2] <- "Overall_fixes"

df_leg <- left_join(c, a, with = leg)

df_leg$percentage_fixes <- (df_leg$Total_Fixes/df_leg$Overall_fixes)*100

## remove temporary objects

rm(a, b, c, df_beh, df_leg)


#---------------------------------------------------#
## Summarise camera behaviours by leg individuals####
#---------------------------------------------------#

## summarise number of fixes by birdID, behaviour and leg
## foraging resting and travelling

a <- with(df_complete, aggregate(sum, by = list(bird_ID, BEH, leg), "sum"))

names(a)[1] <- "birdID"
names(a)[2] <- "Behaviour"
names(a)[3] <- "leg"
names(a)[4] <- "Total_Fixes"

## summarise number of fixes by birdID, behaviour, leg and social interactions

b <- with(df_complete, aggregate(sum, by = list(bird_ID, BEH, leg, SOCIAL), "sum"))

names(b)[1] <- "birdID"
names(b)[2] <- "Behaviour"
names(b)[3] <- "leg"
names(b)[4] <- "Social"
names(b)[5] <- "N_fixes"

## some individuals don't have social interactions for all behaviours on each leg
## add rows for these 0 values

b_expanded <- left_join(b%>%
                expand(birdID, Behaviour, leg, Social), b)

## join data frames together

df_beh <- left_join(a, b_expanded, with = c(birdID, Behaviour, leg))

## calculate percentage of behaviours where social interactions occur

df_beh$percent_fixes <- (df_beh$N_fixes/df_beh$Total_Fixes)*100

## save as a csv file
## define filepath for saved file

filepath_dfout <- here("Data_outputs","Summary_data_frames")

## save file

write_csv(df_beh, file = here(filepath_dfout, "camera_data_summary_individuals_by_leg.csv"))


#------------------#
## End of script####
#------------------#