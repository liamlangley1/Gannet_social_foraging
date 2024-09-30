####Script to fit GAMMs to model dive propensity as a function of colony distance
####Liam Langley


#----------------------#
##1. Load libraries ####
#----------------------#

library(tidyverse)
library(mgcv)
library(itsadug)
library(lubridate)
library(gamm4)
library(scales)
library(tidymv)
library(here)
library(magick)


#----------------------------------------#
##2. Read in and manipulate dive data ####
#----------------------------------------#

## all 2012 and 2013 GPS data interpolated to 1 minute fixes
## from complete trips only
## annotated with dive events - all dives associated with the closest temporal GPS location

df_gannets <- read.csv(here("Data_outputs", "Working_data_frames", "tracking_data_1213_cleaned_and_annotated.csv"))

head(df_gannets)
str(df_gannets)

## convert distance metrics to km 

df_gannets$dist_km <- df_gannets$distance/1000

df_gannets$distaldist_km <- df_gannets$distaldistance/1000

## scale distance metrics by using the scale function
## Z-transforms the predictor

df_gannets$dist_km_scaled <- scale(df_gannets$dist_km)

df_gannets$distaldist_km_scaled <- scale(df_gannets$distaldist_km)

## create a new two-level factor column for leg (inbound/outbound)
## distal location included in the outbound leg

df_gannets$leg2 <- ifelse(df_gannets$leg == "Distal", "Outbound", paste(df_gannets$leg))

levels(as.factor(df_gannets$leg2))

## re-level leg2 so outbound is the reference level

df_gannets$leg2 <- factor(df_gannets$leg2, levels = c("Outbound", "Inbound"))

## convert sex character, year, leg2, BirdID and bird_trip_year columns to factors for modelling

df_gannets$sexchr <- as.factor(as.vector(df_gannets$sexchr))

df_gannets$Year <- as.factor(as.vector(df_gannets$Year))

df_gannets$BirdID <- as.factor(as.vector(df_gannets$BirdID))

df_gannets$bird_trip_year <- as.factor(as.vector(df_gannets$bird_trip_year))

## convert DateTime2 to a POSIXct object to test for temporal autocorrelation

df_gannets$DateTime2 <-  as.POSIXct(df_gannets$DateTime2)

## create an hour of the day coloumn to control for diel cycle effects

df_gannets$hour <- hour(df_gannets$DateTime2)


#--------------------------------------------------------------------#
##3. Model dive propensity as a function of distance to the colony####
#--------------------------------------------------------------------#

## first create a GAM for reference
## without a random intercept for bird ID
## smooth for colony distance by leg (outbound/inbound)
## categorical predictors - sex, year, leg
## set max number of knots to 6 - advice from RS

propensity_gam <- gam(DiveEvent ~ s(dist_km, k = 6, by = leg2) + leg2 + Year + sexchr,
                      data = df_gannets,
                      family = binomial,
                      method = "REML")

## check model summary

summary(propensity_gam)

## plot the smooths from the gam model for reference in base R

plot(propensity_gam, pages = 1, shift = coef(propensity_gam)[1],
     se = TRUE, seWithMean = TRUE, shade = TRUE, shade.col = "#0033CC",
     trans = plogis)

## fit a GAMM using gamm4() function
## BirdID included as a random intercept to control for pseudoreplication

propensity_gamm4 <- gamm4(DiveEvent ~ s(dist_km, by = leg2, k = 6) + sexchr + leg2 + Year,
                     data = df_gannets,
                     family = binomial,
                     random = ~ (1|BirdID))

## check model summary

summary(propensity_gamm4$gam)


#------------------------------------------------------#
##4. Make prediction plot from the model - figure 3 ####
#------------------------------------------------------#

## make prediction plot of the GAMM smooths in Base R
## and save as a tiff


tiff(file = here("Figures","Figure_3.tiff"),
     width = 150, height = 100, units = "mm", res = 200)
layout(mat = matrix(c(1,2), nrow = 1, ncol = 2),
       widths = 1, 2)
par(mar = c(4.5, 4, 1.5, 0.5))
plot.gam(propensity_gamm4$gam, select = 1, shift = coef(propensity_gamm4$gam)[1],
     se = TRUE, seWithMean = TRUE, shade = TRUE, shade.col = alpha("#660099", 0.7),
     trans = plogis, xlab = "Distance to colony (km)", ylab = "Dive propensity")
title(main = "a. Outbound", adj = 0)
par(mar = c(4.5, 0.5, 1.5, 4))
plot.gam(propensity_gamm4$gam, select = 2,  shift = coef(propensity_gamm4$gam)[1],
     se = TRUE, seWithMean = TRUE, shade = TRUE, shade.col = alpha("#660099", 0.7),
     trans = plogis, xlab = "Distance to colony (km)", ylab = "", yaxt = "n",
     xlim = rev(range(df_gannets$dist_km)))
title(main = "b. Inbound", adj = 0)
dev.off()


#-------------------------------------------#
##5. Annotate figure with gannet graphics####
#-------------------------------------------#

## read composite map back in to annotate with the graphics

fig_img <- image_read(here("Figures", "Figure_3.tiff"))

## read in graphic of gannet diving

dive_image <- image_read(here("Gannet_graphics","plunge.png"))
dive_image_altered <- dive_image %>% image_scale("150x150") %>%image_flop()

## annotate image

a <- image_composite(fig_img, dive_image_altered, offset = "+200+100")

## save figure image

image_write(a, here("Figures", "Figure_3_annotated.png"))

##get session info

sessionInfo()


#------------------#
## End of script####
#------------------#