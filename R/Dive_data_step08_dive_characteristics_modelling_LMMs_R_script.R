####Script to fit LMMs to model dive characteristics as a function of colony distance
####Liam Langley

#----------------------#
##1. Load libraries ####
#----------------------#

library(tidyverse)
library(lme4)
library(MuMIn)
library(effects)
library(DHARMa)
library(ggplot2)
library(ggeffects)
library(patchwork)
library(here)
library(lmtest)


#--------------------------------------------------------#
##2. Read in and manipulate dive characteristics data ####
#--------------------------------------------------------#

## dive information from TDRS from all dives
## from complete trips in 2012 and 2013

df_dives <- read_csv(here("Data_outputs", "Working_data_frames", "dive_data_1213_cleaned_and_annotated.csv"))

head(df_dives)
str(df_dives)

## convert colony distance to km 

df_dives$dist_km <- df_dives$distance/1000

## scale distance using the scale function
## Z-transforms the predictor

df_dives$dist_km_scaled <- scale(df_dives$dist_km)

## create a new two-level factor column for leg (inbound/outbound)
## distal location included in the outbound leg

df_dives$leg2 <- ifelse(df_dives$leg == "Distal", "Outbound", paste(df_dives$leg))

levels(as.factor(df_dives$leg2))

## re-level leg factor so that outbound is the reference level

df_dives$leg2 <- factor(df_dives$leg2, levels = c("Outbound", "Inbound"))

## convert sex character, year, birdID and bird_trip_year columns to factors for modelling

df_dives$sexchr <- as.factor(as.vector(df_dives$sexchr))

df_dives$Year <- as.factor(as.vector(df_dives$Year))

df_dives$BirdID <- as.factor(as.vector(df_dives$BirdID))

df_dives$bird_trip_year <- as.factor(as.vector(df_dives$bird_trip_year))

## convert DateTime2 to a POSIXct object to test for temporal autocorrelation

df_dives$DateTime2 <-  as.POSIXct(df_dives$DateTime2)

## create an hour of the day coloumn to control for diel cycle effects

df_dives$hour <- hour(df_dives$DateTime2)


#-----------------------------------------------#
##3. Fit LMM of dive depth by colony distance####
#-----------------------------------------------#

## set global options
## models fail if they encounter an NA
## required for dredge to run

options(na.action = na.fail)

## Model dive depth by distance from colony
## Inbound/outbound as an FE - two-way interaction with distance
## Sex as a two-level factor - control for potential sex effects
## Year as a two-level factor to control for potential interannual variation
## BirdID as a random intercept to account for non-independence of observations

model1 <- lmer(MaxDepth ~  dist_km*leg2 + sexchr + Year +
               I(hour^2) +
               (1|BirdID),
               data = df_dives)

summary(model1)

## perform model selection based on AIC values
## use the dredge function from the MuMIn package

dredge <- dredge(model1, beta = "none", rank = "AIC")

## best model has sex and distance as the only FEs
## simplest model within dAIC < 6.0
## model weight = 0.115
## create model object

selected_model <- lmer(MaxDepth ~ dist_km + sexchr + 
                       (1|BirdID),
                       data = df_dives)

summary(selected_model)

## calculate pseudo R-squared values for the selected model

r.squaredGLMM(selected_model)

## R2m = 0.147
## R2c = 0.466
## good amount of variance explained by the model

## generate p-values for parameters in selected model using LRT
## distance from colony
## created model without colony distance

model_rmdist <- lmer(MaxDepth ~ sexchr + 
                    (1|BirdID),
                    data = df_dives)

## run LRT

lrtest(model_rmdist, selected_model)

## p < 0.001 for distance
## create model with sex removed

model_rmsex <- lmer(MaxDepth ~ dist_km + 
                   (1|BirdID),
                   data = df_dives)

## run LRT

lrtest(model_rmsex, selected_model)

## p = 0.00186

##look at model fit using DHARMa package
##first simulate the residuals from the fitted model

simulated <- simulateResiduals(fittedModel = selected_model, plot = F)

## plot a histogram of the simulated residuals
## should follow a uniform distribution
## slight peak towards the start and end of the distribution

hist(residuals(simulated))

## create a qqplot to detect overall deviations from the expected distribution
## KS test suggests signifcant deviation - overly sensitive
## Dispersion and Outlier tests fine

plotQQunif(simulated)

## create a plot of the residuals against the simulated values

plotResiduals(simulated)

## plot residuals against a predictor
## distance from colony

plotResiduals(simulated, df_dives$dist_km)

## test for under/overdispersion
## p-value = 0.624 and dispersion statistic very close to 1

testDispersion(simulated)

## create a prediction plot from the selected model
## create a prediction data frame using ggpredict function from the ggeffects package

df_fit <- ggpredict(selected_model, terms = c("dist_km[all]", "sexchr"), type = "random")

## create plot
## add raw data in the background

a <- ggplot() +
      geom_point(data = df_dives, aes(x = dist_km, y = MaxDepth, col = sexchr), alpha = 0.1) +
      geom_line(data = df_fit, aes(x = x, y = predicted, col = group)) +
      scale_colour_manual(values = c("#FF0033","#0033CC")) +
      geom_ribbon(data = df_fit, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.4) +
      scale_fill_manual(values = c("#FF0033","#0033CC")) +
      labs(x = "Distance to colony (km)", y = "Max dive depth (m)", col = "Sex", fill = "Sex") +
      theme_bw() +
      theme(##Hide panel borders and remove grid lines
            ##panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_text(size = 15),
            axis.text.x = element_text(hjust=0.7, size = 15),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 15),
            axis.text.y = element_text(hjust=0.7,angle=90,vjust=0.3, size = 15),
            strip.text.x = element_text(size = 15))

## save as a tiff
## Define device to read plots out as e.g. tiff/jpeg

device <- "tiff"

## define units for plot size - usually mm

units <- "mm"

## define plot resolution in dpi - 300 usually minimum

dpi <- 300

## define filepath to read out plots 

out_path <- here("Outputs", "Plots", "Model_outputs", "Dive_characteristics")

## save plot

ggsave(plot = a, filename = "dive_depth_by_colony_distance_model_estimates_updated_plot.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 125, dpi = dpi,   
)


## remove temporary objects

rm(dredge, model1, selected_model, simulated, df_fit)


#--------------------------------------------------#
##4. Fit LMM of dive duration by colony distance####
#--------------------------------------------------#

## Log transform response variable and fit as Gaussian for now
## By distance from colony - scaled for model convergence
## Sex as a FE - two-way interaction with distance
## Inbound/outbound as an FE - two-way interaction with distance
## Year as an FE to control for potential differences
## bird_trip_year as a random intercept to account for non-independence of observations

model1 <- lmer(Duration ~ + dist_km*leg2 + Year 
               + I(hour^2)
               + (1|bird_trip_year),
               data = df_dives)

summary(model1)


##perform model selection based on AIC values
##use the dredge function from the MuMIn package

dredge <- dredge(model1, beta = "none", rank = "AIC")

##null model is simplest model within dAIC < 6.0
##model weight = 0.234
##dAIC = 0.37
##no effect of colony distance on dive depth

##get session info

sessionInfo()


#-------------------#
##End of script####
#-------------------#