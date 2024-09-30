####Script to create figure 4 for manuscript
####Liam Langley
####Date created - 26/02/2024

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

df_trip_final <- results %>%
  filter(distance > 2000)


#------------------------------------#
##5. Read in basemap for plotting ####
#------------------------------------#

## create file path for base map 

map_path <- here ("Base_map", "BI_countries approxGBprj_labelled.shp")

## read in base map

basemap <- st_read(map_path)

## basemap is in British National Grid projection - OSGB 1936
## transform basemap to lat long to be in same projection as the dives

basemap_ll <- st_transform(basemap, crs = st_crs(trip.sf))


#------------------------------------------------#
##6. Map all birds - different spatial scales ####
#------------------------------------------------#

##set same plot limits for all maps based on gannet tracking data

xlim = c(min(df_trip_final$x), max(df_trip_final$x))

ylim = c(min(df_trip_final$y), max(df_trip_final$y))

## create map with camera behaviour as a 7-level factor
## night, rest, solo foraging, group foraging, solo travelling, group travelling
## relevel factor variable

df_trip_final$CamB_Clean <- factor(df_trip_final$CamB_Clean, levels = c("Camera Off", "Night", "Rest", 
                                                                        "Travel: solo", "Travel: group",
                                                                        "Foraging: solo", "Foraging: group"))

## set up colour scale for camera behaviours

colours <- c("#FFFFFF", "#666666", "#3399FF", "#FFFF99", "#FF9900", "#FF66FF", "#990099")

names(colours) <- levels(df_trip_final$CamB_Clean)

colScale <- scale_fill_manual(name = "Behaviour",values = colours)

## make composite figures of maps from each bird
## subset to create a data frame for each bird

levels(as.factor(df_trip_final$bird_ID))

df_G145 <- df_trip_final %>% 
  filter(bird_ID == "G145")

df_G147 <- df_trip_final %>% 
  filter(bird_ID == "G147")

df_G149 <- df_trip_final %>% 
  filter(bird_ID == "G149") %>%
  filter(trip == "G149_1")

df_G150 <- df_trip_final %>% 
  filter(bird_ID == "G150")

df_G151 <- df_trip_final %>% 
  filter(bird_ID == "G151")

df_G152 <- df_trip_final %>% 
  filter(bird_ID == "G152")

df_G153 <- df_trip_final %>% 
  filter(bird_ID == "G153")

df_G156 <- df_trip_final %>% 
  filter(bird_ID == "G156")

## make maps for each bird
## map for G145

G145_map <- ggplot()+
  geom_sf(data = basemap_ll, aes(geometry = geometry, fill = "COUNTRY")) +
  scale_fill_manual(values = "#999999", guide = "none") +
  ##add new fill scale
  new_scale_fill() +
  ##add points and paths between then
  geom_path(data = df_G145, aes(x = x, y = y ), col = "#999999") +
  geom_point(data = df_G145, aes(x = x, y = y, fill = CamB_Clean), 
             size = 3, stroke = 0.05, shape = 21, col = "#CCCCCC") +
  ##add colour scale for points
  colScale +
  ##set plot limits
  coord_sf(xlim = c(max(df_G145$x + 0.05), min(df_G145$x)), ylim = c(max(df_G145$y), min(df_G145$y)), crs = 4326) +
  ##add colony point
  geom_point(data =  data.frame(x = -5.467, y = 51.716),
             aes(x = x, y = y), colour = "#FF3300",
             fill ="#FF3300", shape = 23, size = 4) +
  ##add direction arrows for inbound vs outbound
  geom_line(aes(x = c(-6.1, -6.0), y = c(51.5, 51.6)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "open")) +
  geom_line(aes(x = c(-5.8, -5.9), y = c(51.3, 51.2)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "open")) +
  ##add scalebar
  ggsn::scalebar(x.min = min(df_G145$x), x.max = max(df_G145$x),
                 y.min = min(df_G145$y), y.max = max(df_G145$y), 
                 location = "bottomright",
                 dist = 10, dist_unit = "km",
                 st.dist = 0.04, height = 0.03, st.size = 3.5,
                 st.bottom = FALSE,
                 st.color = "black", box.fill = c("black", "white"),
                 transform = TRUE, model = 'WGS84') +
  ## add title
  ggtitle("G145") +
  labs(x = "Longitude", y = "Latitude", fill = "Behaviour") +
  theme(axis.text=element_text(colour="black"),
        ##Set panel borders and remove grid lines
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        ##remove legend and axis ticks, text and labels
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


## make map for G147

G147_map <- ggplot()+
  geom_sf(data = basemap_ll, aes(geometry = geometry, fill = "COUNTRY")) +
  scale_fill_manual(values = "#999999", guide = "none") +
  ##add new fill scale
  new_scale_fill() +
  ##add points and paths between then
  geom_path(data = df_G147, aes(x = x, y = y ), col = "#999999") +
  geom_point(data = df_G147, aes(x = x, y = y, fill = CamB_Clean), 
             size = 3, stroke = 0.05, shape = 21, col = "#CCCCCC") +
  ##add colour scale for points
  colScale +
  ##set plot limits
  coord_sf(xlim = c(max(df_G147$x + 0.015), min(df_G147$x - 0.015)), ylim = c(max(df_G147$y + 0.01), min(df_G147$y - 0.01)), crs = 4326) +
  ##add direction arrows for inbound vs outbound
  geom_line(aes(x = c(-5.56, -5.56), y = c(51.58, 51.61)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.20,"cm"), ends="last", type = "open")) +
  geom_line(aes(x = c(-5.46, -5.46), y = c(51.58, 51.61)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "open")) +
  ##add scalebar
  ggsn::scalebar(x.min = min(df_G147$x - 0.015), x.max = max(df_G147$x),
                 y.min = min(df_G147$y - 0.01), y.max = max(df_G147$y + 0.01), 
                 location = "bottomright",
                 dist = 2, dist_unit = "km",
                 st.dist = 0.03, height = 0.03, st.size = 3,
                 st.bottom = FALSE,
                 st.color = "black", box.fill = c("black", "white"),
                 transform = TRUE, model = 'WGS84') +
  ##add colony point
  geom_point(data =  data.frame(x = -5.467, y = 51.716),
             aes(x = x, y = y), colour = "#FF3300",
             fill ="#FF3300", shape = 23, size = 4) +
  ## add title
  ggtitle("G147") +
  labs(x = "Longitude", y = "Latitude", fill = "Behaviour") +
  theme(axis.text=element_text(colour="black"),
        ##Set panel borders and remove grid lines
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        ##remove legend and axis ticks, text and labels
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


## make map for G149

G149_map <- ggplot()+
  geom_sf(data = basemap_ll, aes(geometry = geometry, fill = "COUNTRY")) +
  scale_fill_manual(values = "#999999", guide = "none") +
  ##add new fill scale
  new_scale_fill() +
  ##add points and paths between then
  geom_path(data = df_G149, aes(x = x, y = y ), col = "#999999") +
  geom_point(data = df_G149, aes(x = x, y = y, fill = CamB_Clean), 
             size = 3, stroke = 0.05, shape = 21, col = "#CCCCCC") +
  ##add colour scale for points
  colScale +
  ##set plot limits
  coord_sf(xlim = c(max(df_G149$x + 0.075), min(df_G149$x)), ylim = c(max(df_G149$y + 0.05), min(df_G149$y - 0.05)), crs = 4326) +
  ##add direction arrows for inbound vs outbound
  geom_line(aes(x = c(-5.65, -5.85), y = c(51.67, 51.67)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "open")) +
  geom_line(aes(x = c(-5.65, -5.85), y = c(51.82, 51.82)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.20,"cm"), ends="last", type = "open")) +
  ##add colony point
  geom_point(data =  data.frame(x = -5.467, y = 51.716),
             aes(x = x, y = y), colour = "#FF3300",
             fill ="#FF3300", shape = 23, size = 4) +
  ##add scalebar
  ggsn::scalebar(x.min = min(df_G149$x), x.max = max(df_G149$x + 0.05),
                 y.min = min(df_G149$y - 0.05), y.max = max(df_G149$y + 0.05), 
                 location = "bottomright",
                 dist = 5, dist_unit = "km",
                 st.dist = 0.05, height = 0.03, st.size = 3.5,
                 st.bottom = FALSE,
                 st.color = "black", box.fill = c("black", "white"),
                 transform = TRUE, model = 'WGS84') +
  ## add title
  ggtitle("G149") +
  labs(x = "Longitude", y = "Latitude", fill = "Behaviour") +
  theme(axis.text=element_text(colour="black"),
        ##Set panel borders and remove grid lines
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        ##remove legend and axis ticks, text and labels
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## make map for G150

G150_map <- ggplot()+
  geom_sf(data = basemap_ll, aes(geometry = geometry, fill = "COUNTRY")) +
  scale_fill_manual(values = "#999999", guide = "none") +
  ##add new fill scale
  new_scale_fill() +
  ##add points and paths between then
  geom_path(data = df_G150, aes(x = x, y = y ), col = "#999999") +
  geom_point(data = df_G150, aes(x = x, y = y, fill = CamB_Clean), 
             size = 3, stroke = 0.05, shape = 21, col = "#CCCCCC") +
  ##add colour scale for points
  colScale +
  ##set plot limits
  coord_sf(xlim = c(max(df_G150$x + 0.05), min(df_G150$x)), ylim = c(max(df_G150$y), min(df_G150$y)), crs = 4326) +
  ##add direction arrows for inbound vs outbound
  geom_line(aes(x = c(-6.5, -7.0), y = c(51.7, 51.5)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "open")) +
  geom_line(aes(x = c(-6.0, -6.5), y = c(51.4, 51.2)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.20,"cm"), ends="last", type = "open")) +
  ##add colony point
  geom_point(data =  data.frame(x = -5.467, y = 51.716),
             aes(x = x, y = y), colour = "#FF3300",
             fill ="#FF3300", shape = 23, size = 4) +
  ##add scalebar
  ggsn::scalebar(x.min = min(df_G150$x), x.max = max(df_G150$x),
                 y.min = min(df_G150$y), y.max = max(df_G150$y), 
                 location = "bottomright",
                 dist = 25, dist_unit = "km",
                 st.dist = 0.05, height = 0.03, st.size = 3.5,
                 st.bottom = FALSE,
                 st.color = "black", box.fill = c("black", "white"),
                 transform = TRUE, model = 'WGS84') +
  ## add title
  ggtitle("G150") +
  labs(x = "Longitude", y = "Latitude", fill = "Behaviour") +
  theme(axis.text=element_text(colour="black"),
        ##Set panel borders and remove grid lines
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        ##remove legend and axis ticks, text and labels
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## make map for G151

G151_map <- ggplot()+
  geom_sf(data = basemap_ll, aes(geometry = geometry, fill = "COUNTRY")) +
  scale_fill_manual(values = "#999999", guide = "none") +
  ##add new fill scale
  new_scale_fill() +
  ##add points and paths between then
  geom_path(data = df_G151, aes(x = x, y = y ), col = "#999999") +
  geom_point(data = df_G151, aes(x = x, y = y, fill = CamB_Clean), 
             size = 3, stroke = 0.05, shape = 21, col = "#CCCCCC") +
  ##add colour scale for points
  colScale +
  ##set plot limits
  coord_sf(xlim = c(max(df_G151$x + 0.05), min(df_G151$x)), ylim = c(max(df_G151$y), min(df_G151$y)), crs = 4326) +
  ##add direction arrows for inbound vs outbound
  geom_line(aes(x = c(-5.7, -6.0), y = c(51.5, 51.35)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "open")) +
  geom_line(aes(x = c(-6.4, -6.15), y = c(51.5, 51.65)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.20,"cm"), ends="last", type = "open")) +
  ##add colony point
  geom_point(data =  data.frame(x = -5.467, y = 51.716),
             aes(x = x, y = y), colour = "#FF3300",
             fill ="#FF3300", shape = 23, size = 3) +
  ##add scalebar
  ggsn::scalebar(x.min = min(df_G151$x), x.max = max(df_G151$x),
                 y.min = min(df_G151$y), y.max = max(df_G151$y), 
                 location = "bottomright",
                 dist = 10, dist_unit = "km",
                 st.dist = 0.04, height = 0.03, st.size = 3,
                 st.bottom = FALSE,
                 st.color = "black", box.fill = c("black", "white"),
                 transform = TRUE, model = 'WGS84') +
  ## add title
  ggtitle("G151") +
  labs(x = "Longitude", y = "Latitude", fill = "Behaviour") +
  theme(axis.text=element_text(colour="black"),
        ##Set panel borders and remove grid lines
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        ##remove legend and axis ticks, text and labels
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## make map for G152

G152_map <- ggplot()+
  geom_sf(data = basemap_ll, aes(geometry = geometry, fill = "COUNTRY")) +
  scale_fill_manual(values = "#999999", guide = "none") +
  ##add new fill scale
  new_scale_fill() +
  ##add points and paths between then
  geom_path(data = df_G152, aes(x = x, y = y ), col = "#999999") +
  geom_point(data = df_G152, aes(x = x, y = y, fill = CamB_Clean), 
             size = 3, stroke = 0.05, shape = 21, col = "#CCCCCC") +
  ##add colour scale for points
  colScale +
  ##set plot limits
  coord_sf(xlim = c(max(df_G152$x + 0.1), min(df_G152$x - 0.05)), ylim = c(max(df_G152$y), min(df_G152$y)), crs = 4326) +
  ##add direction arrows for inbound vs outbound
  geom_line(aes(x = c(-5.8, -6.0), y = c(51.4, 51.1)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "open")) +
  geom_line(aes(x = c(-5.6, -5.4), y = c(50.9, 51.2)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.20,"cm"), ends="last", type = "open")) +
  ##add colony point
  geom_point(data =  data.frame(x = -5.467, y = 51.716),
             aes(x = x, y = y), colour = "#FF3300",
             fill ="#FF3300", shape = 23, size = 4) +
  ##add scalebar
  ggsn::scalebar(x.min = min(df_G152$x - 0.05), x.max = max(df_G152$x + 0.05),
                 y.min = min(df_G152$y), y.max = max(df_G152$y), 
                 location = "bottomright",
                 dist = 10, dist_unit = "km",
                 st.dist = 0.03, height = 0.03, st.size = 3,
                 st.bottom = FALSE,
                 st.color = "black", box.fill = c("black", "white"),
                 transform = TRUE, model = 'WGS84') +
  ## add title
  ggtitle("G152") +
  labs(x = "Longitude", y = "Latitude", fill = "Behaviour") +
  theme(axis.text=element_text(colour="black"),
        ##Set panel borders and remove grid lines
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        ##remove legend and axis ticks, text and labels
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## make map for G153

G153_map <- ggplot()+
  geom_sf(data = basemap_ll, aes(geometry = geometry, fill = "COUNTRY")) +
  scale_fill_manual(values = "#999999", guide = "none") +
  ##add new fill scale
  new_scale_fill() +
  ##add points and paths between then
  geom_path(data = df_G153, aes(x = x, y = y ), col = "#999999") +
  geom_point(data = df_G153, aes(x = x, y = y, fill = CamB_Clean), 
             size = 3, stroke = 0.05, shape = 21, col = "#CCCCCC") +
  ##add colour scale for points
  colScale +
  ##set plot limits
  coord_sf(xlim = c(max(df_G153$x + 0.05), min(df_G153$x - 0.05)), ylim = c(max(df_G153$y), min(df_G153$y)), crs = 4326) +
  ##add direction arrows for inbound vs outbound
  geom_line(aes(x = c(-5.95, -5.95), y = c(50.7, 51.0)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "open")) +
  geom_line(aes(x = c(-5.4, -5.4), y = c(51.0, 51.3)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.20,"cm"), ends="last", type = "open")) +
  ##add colony point
  ##add colony point
  geom_point(data =  data.frame(x = -5.467, y = 51.716),
             aes(x = x, y = y), colour = "#FF3300",
             fill ="#FF3300", shape = 23, size = 4) +
  ##add scalebar
  ggsn::scalebar(x.min = min(df_G153$x - 0.05), x.max = max(df_G153$x),
                 y.min = min(df_G153$y), y.max = max(df_G153$y), 
                 location = "bottomright",
                 dist = 10, dist_unit = "km",
                 st.dist = 0.03, height = 0.03, st.size = 3,
                 st.bottom = FALSE,
                 st.color = "black", box.fill = c("black", "white"),
                 transform = TRUE, model = 'WGS84') +
  ## add title
  ggtitle("G153") +
  labs(x = "Longitude", y = "Latitude", fill = "Behaviour") +
  theme(axis.text=element_text(colour="black"),
        ##Set panel borders and remove grid lines
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        ##remove legend and axis ticks, text and labels
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## make map for G156

G156_map <- ggplot()+
  geom_sf(data = basemap_ll, aes(geometry = geometry, fill = "COUNTRY")) +
  scale_fill_manual(values = "#999999", guide = "none") +
  ##add new fill scale
  new_scale_fill() +
  ##add points and paths between then
  geom_path(data = df_G156, aes(x = x, y = y ), col = "#999999") +
  geom_point(data = df_G156, aes(x = x, y = y, fill = CamB_Clean), 
             size = 3, stroke = 0.05, shape = 21, col = "#CCCCCC") +
  ##add colour scale for points
  colScale +
  ##set plot limits
  coord_sf(xlim = c(max(df_G156$x), min(df_G156$x - 0.01)), ylim = c(max(df_G156$y + 0.01), min(df_G156$y)), crs = 4326) +
  ##add direction arrows for inbound vs outbound
  geom_line(aes(x = c(-5.3, -5.25), y = c(51.65, 51.63)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "open")) +
  geom_line(aes(x = c(-5.41, -5.42), y = c(51.60, 51.63)),
            colour = "black", alpha = 0.6, size = 0.8, arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "open")) +
  ##add colony point
  geom_point(data =  data.frame(x = -5.467, y = 51.716),
             aes(x = x, y = y), colour = "#FF3300",
             fill ="#FF3300", shape = 23, size = 4) +
  ##add scalebar
  ggsn::scalebar(x.min = min(df_G156$x - 0.01), x.max = max(df_G156$x),
                 y.min = min(df_G156$y), y.max = max(df_G156$y + 0.01), 
                 location = "bottomleft",
                 dist = 5, dist_unit = "km",
                 st.dist = 0.04, height = 0.03, st.size = 3,
                 st.bottom = FALSE,
                 st.color = "black", box.fill = c("black", "white"),
                 transform = TRUE, model = 'WGS84') +
  ## add title
  ggtitle("G156") +
  labs(x = "Longitude", y = "Latitude", fill = "Behaviour") +
  theme(axis.text=element_text(colour="black"),
        ##Set panel borders and remove grid lines
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        ##remove legend and axis ticks, text and labels
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


#-----------------------------#
##7. Make composite figure ####
#-----------------------------#

## use a nested ggarrange

a <- ggarrange(
  ggarrange(G145_map, G151_map, G156_map,  ncol = 3, nrow = 1),
  ggarrange(G149_map, G150_map, ncol = 2, common.legend = TRUE, legend = "right"),
  nrow = 2 ) 

composite_map2 <- ggarrange(a,
                   ggarrange(G147_map, G152_map, G153_map, ncol = 3),
                   nrow = 2)

## save figure as a tiff 
## Define parameters for reading out plot
## Define device to read plots out as e.g. tiff/jpeg

device <- "tiff"

## define units for plot size - usually mm

units <- "mm"

## define plot resolution in dpi - 300 usually minimum

dpi <- 300

## define filepath to read out plots 

out_path <- here("Figures")

## save figure

ggsave(plot = composite_map2, filename = "figure_1_all_birds.tiff",
       device = device,
       path = out_path ,units = units, width = 200, height = 250, dpi = dpi,   
)


#-------------------#
## End of script ####
#-------------------#