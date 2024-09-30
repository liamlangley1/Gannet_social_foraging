####Script to fit GAMMs to model dive characteristics as a function of colony distance
####Liam Langley

#----------------------#
##1. Load libraries ####
#----------------------#

library(tidyverse)
library(mgcv)
library(itsadug)
library(lubridate)
library(gamm4)


#-----------------------------------------------------------#
##2. Read in and manipulate in dive characteristics data ####
#-----------------------------------------------------------#

##dive information from TDRS from all dives
##from complete trips in 2012 and 2013

df_dives <- read.csv("dive_data_1_1213_cleaned_and_annotated.csv")

head(df_dives)
str(df_dives)

##convert colony distance to km 

df_dives$dist_km <- df_dives$distance/1000

##scale distance using the scale function
##Z-transforms the predictor

df_dives$dist_km_scaled <- scale(df_dives$dist_km)

##create a new two-level factor column for leg (inbound/outbound)
##distal location included in the outbound leg

df_dives$leg2 <- ifelse(df_dives$leg == "Distal", "Outbound", paste(df_dives$leg))

levels(as.factor(df_dives$leg2))

##re-level leg factor so that outbound is the reference level

df_dives$leg2 <- factor(df_dives$leg2, levels = c("Outbound", "Inbound"))

##convert sex character, year, birdID and bird_trip_year columns to factors for modelling

df_dives$sexchr <- as.factor(as.vector(df_dives$sexchr))

df_dives$Year <- as.factor(as.vector(df_dives$Year))

df_dives$BirdID <- as.factor(as.vector(df_dives$BirdID))

df_dives$bird_trip_year <- as.factor(as.vector(df_dives$bird_trip_year))

##convert DateTime2 to a POSIXct object to test for temporal autocorrelation

df_dives$DateTime2 <-  as.POSIXct(df_dives$DateTime2)

##create an hour of the day coloumn to control for diel cycle effects

df_dives$hour <- hour(df_dives$DateTime2)


#-------------------------------------------------------------------#
##3. Create GAMMs for dive depth as a function of colony distance####
#-------------------------------------------------------------------#

##first create a GAM for reference
##without a random intercept for bird ID
##smooth for colony distance by leg (outbound/inbound)
##categorical predictors - sex, year, leg

depth_gam <- gam(MaxDepth ~ s(dist_km, k = 6, by = leg2) + leg2 + Year + sexchr,
                 data = df_dives,
                 method = "REML")

##check model summary

summary(depth_gam)

##plot the smooths from the gam model for reference in base R

plot(depth_gam, pages = 1, shift = coef(depth_gam)[1],
     se = TRUE, seWithMean = TRUE, shade = TRUE, shade.col = "#0033CC")


##create model using gamm4 to check this effect is real
##and not a function of fitting birdID as a smooth term

##refit model using gamm4() function
##check result not a quirk of fitting BirdID as a smooth term

depth_gamm4 <- gamm4(MaxDepth ~ s(dist_km, by = leg2, k = 6) + sexchr + leg2 + Year,
                     data = df_dives,
                     random = ~ (1|BirdID))

##check model summary

summary(depth_gamm4$gam)

##plot model

plot(depth_gamm4$gam, pages = 1, shift = coef(depth_gamm4$gam)[1],
     se = TRUE, seWithMean = TRUE, shade = TRUE, shade.col = "#0033CC")

##try adding a quadratic term for hour of the day
##to deal with the fact that dive depth may peak towards midday
##based on results from Sam's paper
##first convert hour column to numeric

df_dives$hour <- as.numeric(df_dives$hour)

##create model

depth_gamm_time <- gamm4(MaxDepth ~ s(dist_km, k = 6, by = leg2) + leg2 + Year + sexchr
                         + I(hour^2),
                         data = df_dives,
                         random = ~ (1|BirdID))

##check model summary

summary(depth_gamm_time$gam)

##plot model 

tiff(filename = "Dive_depth_GAMM_smooth_estimates.tif",
     width = 150, height = 100, units = "mm", res = 200)
plot(depth_gamm_time$gam, pages = 1, shift = coef(depth_gamm_time$gam)[1],
     se = TRUE, seWithMean = TRUE, shade = TRUE, shade.col = alpha("#660099", 0.7))
dev.off()


#----------------------------------------------------------------------#
##4. Create GAMMs for dive duration as a function of colony distance####
#----------------------------------------------------------------------#

##first create a GAM for reference
##without a random intercept for bird ID
##smooth for colony distance by leg (outbound/inbound)
##categorical predictors - sex, year, leg

dur_gam <- gam(Duration ~ s(dist_km, k = 6, by = leg2) + leg2 + Year + sexchr,
                 data = df_dives,
                 method = "REML")

##check model summary

summary(dur_gam)

##plot the smooths from the gam model for reference in base R

plot(dur_gam, pages = 1, shift = coef(dur_gam)[1],
     se = TRUE, seWithMean = TRUE, shade = TRUE, shade.col = "#0033CC")


##fir GAMM model using gamm4() function
##BirdID as a random intercept
##Smooth on inbound term non-linear but also N.S.
##fit as GLMM

dur_gamm4 <- gamm4(Duration ~ s(dist_km, by = leg2, k = 6) + sexchr + leg2 + Year,
                         data = df_dives,
                         random = ~ (1|BirdID))

##check model summary

summary(dur_gamm4$gam)

##plot model

plot(dur_gamm4$gam, pages = 1, shift = coef(dur_gamm4$gam)[1],
     se = TRUE, seWithMean = TRUE, shade = TRUE, shade.col = "#0033CC")

##try adding a quadratic term for hour of the day
##to deal with the fact that dive depth may peak towards midday
##based on results from Sam's paper
##create model

dur_gamm_time <- gamm4(Duration ~ s(dist_km, k = 6, by = leg2) + leg2 + Year + sexchr
                       + I(hour^2),
                       data = df_dives,
                       random = ~ (1|BirdID))

##check model summary

summary(dur_gamm_time$gam)

##plot model 

tiff(filename = "Dive_duration_GAMM_smooth_estimates.tiff",
     width = 150, height = 100, units = "mm", res = 200)
plot(dur_gamm_time$gam, pages = 1, shift = coef(dur_gamm_time$gam)[1],
     se = TRUE, seWithMean = TRUE, shade = TRUE, shade.col = alpha("#660099", 0.7))
dev.off()


#------------------#
##End of script ####
#------------------#