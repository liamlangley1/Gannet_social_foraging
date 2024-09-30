####Script to create manuscript Fig. 3
####Plot dive locations as a function of distance to distal point
####Look at distribution across trip
####Liam Langley


#----------------------#
##1. Load libraries ####
#----------------------#

library(tidyverse)
library(ggpubr)
library(here)


#-------------------------#
##2. Read in dive data ####
#-------------------------#

## all dives from tracked Gannets in 2012 and 2013
## cleaned and annotated with distance and time metrics

df_dives <- read_csv(here("Data_outputs", "Working_data_frames", "dive_data_1213_cleaned_and_annotated.csv"))

head(df_dives)
str(df_dives)

## convert distance from distal point and distmax to km for plots

df_dives$distaldistkm <- df_dives$distaldistance2/1000

df_dives$distmaxkm <- df_dives$distmax/1000

##create bird_trip column for plots

df_dives$bird_trip <- paste0(df_dives$BirdID, "_", df_dives$Trip)

## too many trips - split by year

df_dives12 <- subset(df_dives, df_dives$Year == 2012)

df_dives13 <- subset(df_dives, df_dives$Year == 2013)


#---------------------#
##3. Create Fig. 2 ####
#---------------------#

## plot dive locations as a function of distance to distal point
## standardised with 0 as the distal location
## starts and ends represent colony location
## first need to create a trip-level data frame containing only trip Id and distmaxkm
## to make boxes for each trip
## make a df taking the first row of each trip

df_trip <- df_dives[!duplicated(df_dives$bird_trip_year), ]

## remove excess columns

df_trip <- df_trip %>%
  select(c(bird_trip_year, bird_trip, distmaxkm, Year))

## subset by year

df_trip12 <- subset(df_trip, df_trip$Year == 2012)

df_trip13 <- subset(df_trip, df_trip$Year == 2013)

## create plot for 2012

a <- ggplot() +
      coord_flip() +
      geom_segment(data = df_trip12, aes(x = bird_trip, xend = bird_trip, y = (0 - distmaxkm), yend = (0 + distmaxkm)),
                   size = 8, alpha = 0.1, inherit.aes = FALSE) +
      ggtitle("a. 2012") +
      geom_jitter(data = df_dives12, aes(x = bird_trip, y = distaldistkm), 
                  col = "#FF9933", width = 0.2, alpha = 0.5) +
      geom_errorbar(data = df_dives12, aes(x = bird_trip, ymax=(0 - distmaxkm), ymin=(0 - distmaxkm))) +
      geom_errorbar(data = df_dives12, aes(x = bird_trip, ymax=distmaxkm, ymin=distmaxkm)) +
      labs(x = "Bird trip", y = "Distance from distal location (km)") +
      geom_hline(yintercept = 0, colour ="black", linetype = "dashed") +
      theme(axis.text=element_text(colour="black"),
            ##Hide panel borders and remove grid lines,
            legend.position = "none",
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_text(size = 15),
            axis.text.x = element_text(hjust=0.7),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 15),
            axis.text.y = element_text(hjust=0.7,vjust=0.3),
            strip.text.x = element_text(size = 15))

## save as a tiff
## Define parameters for reading out plot
## Define device to read plots out as e.g. tiff/jpeg

device <- "tiff"

## define units for plot size - usually mm

units <- "mm"

## define plot resolution in dpi - 300 usually minimum

dpi <- 300

## define filepath to read out plots 

out_path <- here("Outputs", "Plots", "Data_visualisation")

## save plot

ggsave(plot = a, filename = "gannet_dives_by_distance_from_distal_point_2012.tiff",
       device = device,
       path = out_path ,units = units, width = 250, height = 200, dpi = dpi,   
)


## create plot for 2013

b <- ggplot() +
      coord_flip() +
      geom_segment(data = df_trip13, aes(x = bird_trip, xend = bird_trip, y = (0 - distmaxkm), yend = (0 + distmaxkm)),
                   size = 6, alpha = 0.1, inherit.aes = FALSE) +
      geom_jitter(data = df_dives13, aes(x = bird_trip, y = distaldistkm),
                  col = "#FF9933", width = 0.2, alpha = 0.5) +
      geom_errorbar(data = df_dives13, aes(x = bird_trip, ymax=(0 - distmaxkm), ymin=(0 - distmaxkm))) +
      geom_errorbar(data = df_dives13, aes(x = bird_trip, ymax=distmaxkm, ymin=distmaxkm)) +
      scale_colour_manual(values = c("#660099", "#FF9933")) +
      labs(x = "Bird trip", y = "Distance to distal location (km)") +
      geom_hline(yintercept = 0, colour ="black", linetype = "dashed") +
      ggtitle("b. 2013") +
      theme(axis.text=element_text(colour="black"),
            ##Hide panel borders and remove grid lines
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_text(size = 15),
            axis.text.x = element_text(hjust=0.7),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 15),
            axis.text.y = element_text(hjust=0.7,vjust=0.3),
            strip.text.x = element_text(size = 15))

## save as a tiff

ggsave(plot = b, filename = "gannet_dives_by_distance_from_distal_point_2013.tiff",
       device = device,
       path = out_path ,units = units, width = 250, height = 200, dpi = dpi,   
)


## create a facet plot of 2012 and 2013 separately
## for Fig. 3 in manuscript
## use ggarrange function

c <- ggarrange(a, b, nrow = 1, ncol = 2)

## save as a tiff
## define filepath for saved figure

out_path <- here("Figures")

## save figure

ggsave(plot = c, filename = "Figure_2.tiff",
       device = "tiff",
       path = out_path, units = units, width = 250, height = 200, dpi = dpi,   
)

## save as a jpeg

ggsave(plot = c, filename = "Figure_2.jpeg",
       device = "jpeg",
       path = out_path, units = units, width = 250, height = 200, dpi = dpi,   
)


## get session info

sessionInfo()


#------------------#
## End of script####
#------------------#