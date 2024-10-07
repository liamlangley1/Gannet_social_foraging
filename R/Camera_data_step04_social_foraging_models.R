####Script to model probability of social foraging as a function of distance from the colony
####Using GAMMs
####Liam Langley
####Date created - 25/03/2024

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


#-----------------------------------------------------#
##2. Read in and manipulate camera behaviours data ####
#-----------------------------------------------------#

## cleaned and annotated data for 6 birds with complete trips only
## create filepath for read in

df_cam <- read_csv(here("Data_outputs", "Working_data_frames", "Camera_data_cleaned_complete_trips.csv"))

str(df_cam)
head(df_cam)

## create a binary column for social foraging/not based on CamB_clean column
## convert distnce from colony to km
## convert BirdID into a factor for modelling
## convert leg into a factor for modelling
## set "Outbound to reference level
## convert DateTime to POSIXct object
## create hour column to test for diel cycle effects

df_cam <- df_cam %>%
  mutate(sofor = ifelse(CamB_Clean == "Foraging: solo", 1, 0)) %>%
  mutate(dist_km = distance/1000) %>%
  mutate(bird_ID = factor(bird_ID)) %>%
  mutate(leg = factor(leg, levels = c("Outbound", "Inbound")))%>%
  mutate(DateTime = as.POSIXct(date, format = "%d/%m/%Y %H:%M"))%>%
  mutate(hour = hour(DateTime))


#-------------------------------------------------------------------------------#
##3. Model social foraging propensity as a function of distance to the colony####
#-------------------------------------------------------------------------------#

## first create a GAM for reference
## without a random intercept for bird ID
## smooth for colony distance by leg (outbound/inbound)
## categorical predictors - sex, year, leg
## set max number of knots to 6 - advice from RS

sofor_gam <- gam(sofor ~ s(dist_km, k = 6, by = leg) + leg,
                 data = df_cam,
                 family = binomial,
                 method = "REML")

## check model summary

summary(sofor_gam)

## plot the smooths from the gam model for reference in base R

plot(sofor_gam, pages = 1, shift = coef(sofor_gam)[1],
     se = TRUE, seWithMean = TRUE, shade = TRUE, shade.col = "#0033CC",
     trans = plogis)


## fit a GAMM using gamm4() function
## BirdID included as a random intercept to control for pseudoreplication

sofor_gamm4 <- gamm4(sofor ~ s(dist_km, by = leg, k = 6) + leg,
                     data = df_cam,
                     family = binomial,
                     random = ~ (1|bird_ID))

## check model summary

summary(sofor_gamm4$gam)


## make prediction plot from the model
## and save as a tiff

tiff(file = here("Figures","Figure_6.tiff"),
     width = 150, height = 100, units = "mm", res = 200)
layout(mat = matrix(c(1,2), nrow = 1, ncol = 2),
       widths = 1, 2)
par(mar = c(4.5, 4, 1.5, 0.5))
plot.gam(sofor_gamm4$gam, select = 1, shift = coef(sofor_gamm4$gam)[1],
         se = TRUE, seWithMean = TRUE, shade = TRUE, shade.col = alpha("#660099", 0.7),
         trans = plogis, xlab = "Distance to colony (km)", ylab = "Social Foraging Propensity")
title(main = "a. Outbound", adj = 0)
par(mar = c(4.5, 0.5, 1.5, 4))
plot.gam(sofor_gamm4$gam, select = 2,  shift = coef(sofor_gamm4$gam)[1],
         se = TRUE, seWithMean = TRUE, shade = TRUE, shade.col = alpha("#660099", 0.7),
         trans = plogis, xlab = "Distance to colony (km)", ylab = "", yaxt = "n",
         xlim = rev(range(df_cam$dist_km)))
title(main = "b. Inbound", adj = 0)
dev.off()


#------------------------------------#
##4. Annotate with gannet graphics####
#------------------------------------#

## read composite map back in to annotate with the graphics

fig_img <- image_read(here("Figures", "Figure_6.tiff"))

## read in graphic of gannet diving

dive_image <- image_read(here("Gannet_graphics","plunge.png"))
dive_image_altered <- dive_image %>% image_scale("125x125") 

dive_image2 <- image_read(here("Gannet_graphics","plunge.png"))
dive_image_altered2 <- dive_image %>% image_scale("100x100") 

dive_image3 <- image_read(here("Gannet_graphics","plunge.png"))
dive_image_altered3 <- dive_image %>% image_scale("75x75") 

## annotate image

a <- image_composite(fig_img, dive_image_altered, offset = "+850+100")

b <- image_composite(a, dive_image_altered2, offset = "+925+150")

c <- image_composite(b, dive_image_altered3, offset = "+900+75")

## save figure image

image_write(c, here("Figures", "Figure_6.png"))


#------------------#
## End of script####
#------------------#