####Create a year-level breakdown of the data used for the dive analysis
####Number of birds, complete trips, GPS fixes and dives
####Liam Langley


#----------------------#
##1. load libraries ####
#----------------------#

library(tidyverse)
library(ggplot2)
library(here)


#-------------------------#
##2. Read in dive data ####
#-------------------------#


df_dives <- read_csv(here("Data_outputs", "Working_data_frames", "dive_data_1213_cleaned_and_annotated.csv"))

head(df_dives)
str(df_dives)

## read in GPS data
## GPS data interpolated to 1 minute intervals
## with dive events annotated

df_points <- read_csv(here("Data_outputs", "Working_data_frames", "tracking_data_1213_cleaned_and_annotated.csv"))

head(df_points)
str(df_points)

## create sum columns to aggregate by

df_dives$sum <- 1

df_points$sum <- 1


#-----------------------------------#
##3. Create year-level breakdowns####
#-----------------------------------#

## create a breakdown of number of birds in each year
## first create a breakdown with a row per bird and year included

a <- with(df_points, aggregate(sum, by = list(BirdID, Year), "sum"))

names(a)[1] <- "BirdID"
names(a)[2] <- "year"
names(a)[3] <- "n.fixes"

## create sum column to aggregate by

a$sum <- 1

## aggregate a by year to get number of birds per year

b <- with(a, aggregate(sum, by = list(year), "sum"))

names(b)[1] <- "year"
names(b)[2] <- "n.birds"

## make a breakdown of number of complete trips in each year
## first make a data frame with a row for each bird_trip_year

c <- with(df_points, aggregate(sum, by = list(bird_trip_year, Year), "sum"))

names(c)[1] <- "bird_trip_year"
names(c)[2] <- "year"
names(c)[3] <- "n.fixes"

## create sum column to aggregate by

c$sum <- 1

## aggregate c by year to get number of birds per year

d <- with(c, aggregate(sum, by = list(year), "sum"))

names(d)[1] <- "year"
names(d)[2] <- "n.trips"

## make a breakdown of the number of GPS fixes per year

e <- with(df_points, aggregate(sum, by = list(Year), "sum"))

names(e)[1] <- "year"
names(e)[2] <- "n.fixes"

## make a breakdown of total dives per year

f <- with(df_dives, aggregate(sum, by = list(Year), "sum"))

names(f)[1] <- "year"
names(f)[2] <- "total.dives"

## combine data frames to make a single year-level breakdown data frame

df_1 <- left_join(b, d, with = year)

df_2 <- left_join(df_1, e, with = year)

df_breakdown <- left_join(df_2, f, with = year)

## save as a csv file
## define filepath for saved file

filepath_dfout <- here("Data_outputs","Summary_data_frames")

## save file

write_csv(df_breakdown, file = here(filepath_dfout, "all_data_summary_1_year_level.csv"))

## get session info

sessionInfo()


#-------------------#
##End of script####
#-------------------#