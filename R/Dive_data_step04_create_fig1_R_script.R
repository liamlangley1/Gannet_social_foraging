####Script to create Fig. 2 for manuscript
####Facet plot of GPS tracks, dive kernels
####And histograms of dive distance to colony
####Liam Langley

#----------------------#
##1. Load libraries ####
#----------------------#

library(ggplot2)
library(grid)
library(mapdata)
library(mapproj)
library(raster)
library(RColorBrewer)
library(tidyverse)
library(patchwork)
library(sf)
library(sfheaders)
library(marmap)
library(lubridate)
library(ggnewscale)
library(ggpubr)
library(cowplot)
library(here)
library(magick)
library(ggspatial)


#---------------------------------------------------#
##2. Read in an manipulate dive data and kernels ####
#---------------------------------------------------#

## load in dive data
## cleaned and annotated dives from birds tracked in 2012 and 2013 only

df_dives <- read_csv(here("Data_outputs", "Working_data_frames", "dive_data_1213_cleaned_and_annotated.csv"))

head(df_dives)
str(df_dives)

## convert distance to colony to km for plots

df_dives$dist_km <- df_dives$distance/1000

## convert dive data to an sf object to add co-ordinate projection
## to match with basemap to set plot limits for mapping

dives.sf <- st_as_sf(df_dives, coords = c("x", "y"))

## set co-ordinate projection to lat/long with WGS84
## Use EPSG code - 4326

st_crs(dives.sf) <- 4326

## check crs

st_crs(dives.sf)

## convert dives.sf back to a data frame for plotting

df_dives_final <- sf_to_df(dives.sf, fill = TRUE)

## read in GPS data
## all location points for birds tracked in 2012 and 2013 only
## interpolated to 1 minute intervals

df_points <- read_csv(here("Data_outputs", "Working_data_frames", "tracking_data_1213_cleaned_and_annotated.csv"))

head(df_points)
str(df_points)

## convert to an sf object

points.sf <- st_as_sf(df_points, coords = c("x", "y"))

## set co-ordinate projection to lat/long with WGS84
## Use EPSG code - 4326

st_crs(points.sf) <- 4326

## check crs

st_crs(points.sf)

## convert dives.sf back to a data frame for plotting

df_points_final <- sf_to_df(points.sf, fill = TRUE)

## load in dive kernel data frame for both sexes combined
## smoothing parameter (h) = 10km - following Carter et al. 2016

df_kernels <- read_csv(here("Data_outputs", "Working_data_frames", "gannet_kernels_all_dives.csv"))

## set co-ordinate projection to LAEA

sf_kernels <- sf::st_as_sf(df_kernels, coords = c("long", "lat"))
st_crs(sf_kernels) <- CRS("+proj=laea +lat_0=51.716 +lon_0=-5.467 +units=m")

## check CRS

st_crs(sf_kernels)

## ggplot won't recognise kernels as polygons in this format
## need to convert data frame to a multipolygons object
## use formula from Alice

sf_kernels_poly = st_sf(
  aggregate(
    sf_kernels$geometry,
    list(sf_kernels$piece, sf_kernels$percent, sf_kernels$id),
    function(g){
      st_cast(st_combine(g),"POLYGON")
    }
  ))

## convert %UD to a factor and relevel so 95% UD plotted first

sf_kernels_poly$Group.2 <- factor(sf_kernels_poly$Group.2, levels = c("95", "75", "50", "25"))

## remove temporary data frames

rm(df_points, df_dives, df_kernels, sf_kernels)


#-----------------------------------#
##3. Read in basemap for plotting####
#-----------------------------------#

## map of the outline of the British Isles
## read in as a multipolygon object
## create file path for base map 

map_path <- here ("Base_map", "BI_countries approxGBprj_labelled.shp")

## read in base map

basemap <- st_read(map_path)

## basemap is in British National Grid projection - OSGB 1936
## transform basemap to lat long to be in same projection as the dives

basemap_ll <- st_transform(basemap, crs = st_crs(points.sf))


#-----------------------------------------------------------#
##4. Create maps of raw GPS tracks and dives from gannets####
#-----------------------------------------------------------#

## map all GPS tracks and dives
## from complete trips only
## add inset map showing study area
## set plot limits based on gannet tracking data

xlim = c(min(df_points_final$x), max(df_points_final$x))

ylim = c(min(df_points_final$y), max(df_points_final$y))

## create bbox based on study area

SA_bbox <- st_as_sfc(st_bbox(c(xmin = min(df_points_final$x), xmax = max(df_points_final$x),
                               ymin = min(df_points_final$y), ymax = max(df_points_final$y)),
                               crs = st_crs(4326)))

## create map of entire British Isles for inset

a <- ggplot()+
      geom_sf(data = basemap_ll, aes(geometry = geometry, fill = "COUNTRY")) +
      scale_fill_manual(values = "#999999", guide = "none") +
      # add study area bbox
      geom_sf(data = SA_bbox, fill = NA, col = "red", size = 1.2) +
      # add north arrow using ggspatial
      annotation_north_arrow(location = "tl",
                             height = unit(1, "cm"),
                             width = unit(1, "cm"),
                            pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"),
                            style = north_arrow_nautical(fill = c("grey40", "white"),
                                                         line_col = "grey20")) +
      theme(axis.text=element_text(colour="black"),
            ##Add panel borders and remove grid lines
            panel.border = element_rect(colour = "black", fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            ##remove axis text and labels
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())

## create a map

b <- ggplot()+
      geom_sf(data = basemap_ll, aes(geometry = geometry, fill = "COUNTRY")) +
      scale_fill_manual(values = "#999999", guide = "none") +
      ##add GPS points and paths between then
      geom_point(data = df_points_final, aes(x = x, y = y), col = "#660099", alpha = 0.4, size = 0.1 ) +
      geom_path(data = df_points_final, aes(x = x, y = y), col = "#660099", alpha = 0.4, linewidth = 0.2) +
      ##add dive locations
      geom_point(data = df_dives_final, aes(x = x, y = y), col = "#FF9933", alpha = 0.4, size = 1) +
      ##add text for number of birds
      annotate(geom = "text", x = -7.5, y = 52.5, label = "N = 46 birds", color = "white") +
      ##set plot limits
      coord_sf(xlim = xlim, ylim = ylim, crs = 4326) +
      ##add colony point
      geom_point(data =  data.frame(x = -5.467, y = 51.716),
                 aes(x = x, y = y), colour = "#FF3300",
                 fill ="#FF3300", shape = 23, size = 2) +
      ##add title
      ggtitle("a.") +
      labs(x = "Longitude", y = "") +
      theme(axis.text=element_text(colour="black"),
            ##Add panel borders and remove grid lines
            panel.border = element_rect(colour = "black", fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=0.7),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 12),
            axis.text.y = element_text(hjust=0.7, angle = 45, vjust=0.3))

## save out as a tiff
## Define parameters for reading out plot

device <- "tiff"

## define units for plot size - usually mm

units <- "mm"

## define plot resolution in dpi - 300 usually minimum

dpi <- 300

## define filepath to read out plots 

out_path <- here("Outputs", "Maps", "Dives")

## save plot

ggsave(plot = b, filename = "all_gannet_tracks_and_dives_map.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 160, dpi = dpi,   
)


##create inset map
##using ggdraw() function from cowplot

c <- ggdraw() +
      draw_plot(b) +
      draw_plot(a, width = 0.65, height = 0.5)


#--------------------------------------------------#
##5. Create map of 50% UD of gannet dive kernels####
#--------------------------------------------------#

## subset kernel object for the 50% UD 

sf_kernels_poly50 <- subset(sf_kernels_poly, sf_kernels_poly$Group.2 == 50)

## set up distance buffers
## create a data frame for the colony centroid

df_centroid <- data.frame(x = 0, y = 0)

## convert to sf object and add CRS

centroidsf <- st_as_sf(df_centroid, coords = c("x", "y"))

st_crs(centroidsf) <- st_crs(sf_kernels_poly50)

## create buffers
## 50km, 100km and 150km from the colony

buffer50 <- st_buffer(centroidsf, dist = 50000, nQuadSegs = 500)

buffer100 <- st_buffer(centroidsf, dist = 100000, nQuadSegs = 500)

buffer150 <- st_buffer(centroidsf, dist = 150000, nQuadSegs = 500)

buffer200 <- st_buffer(centroidsf, dist = 200000, nQuadSegs = 500)

## create map of female dive kernels
## dives from complete trips only

d <- ggplot()+
      # add distance buffers around colony
      geom_sf(data = buffer50, aes(geometry = geometry), alpha = 0, col = "black") +
      geom_sf(data = buffer100, aes(geometry = geometry), alpha = 0, col = "black") +
      geom_sf(data = buffer150, aes(geometry = geometry), alpha = 0, col = "black") +
      geom_sf(data = buffer200, aes(geometry = geometry), alpha = 0, col = "black") +
      # add text labels for buffer distances
      annotate(geom = "text", x = -7.8, y = 50.8, label = "200", color = "black") +
      annotate(geom = "text", x = -7.1, y = 51, label = "150", color = "black") +
      annotate(geom = "text", x = -6.5, y = 51.2, label = "100", color = "black") +
      annotate(geom = "text", x = -5.7, y = 51.4, label = "50", color = "black") +
      ##add kernels
      geom_sf(data = sf_kernels_poly50, aes(geometry = geometry),
              fill = "#FF9933", lwd = 0.4, alpha = 0.6) +
      ##add basemap over the top
      geom_sf(data = basemap_ll, aes(geometry = geometry, fill = "COUNTRY")) +
      scale_fill_manual(values = "#999999", guide = "none") +
      ##set plot limits
      coord_sf(xlim = xlim, ylim = ylim, crs = 4326) +
      ##add colony point
      geom_point(data =  data.frame(x = -5.467, y = 51.716),
                 aes(x = x, y = y), colour = "#FF3300",
                 fill = "#FF3300", shape = 23, size = 2) +
      ggtitle("b.") +
      labs(x = "Longitude", y = "Latitude") +
      theme(axis.text=element_text(colour="black"),
            ##Add panel borders and remove grid lines
            panel.border = element_rect(colour = "black", fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(hjust=0.7),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())


#----------------------------------------------------------------#
##6. Create frequency histogram of dive distance from the colony####
#----------------------------------------------------------------#

## create a histogram of distance from the colony
## for all dives by all gannets on complete trips

e <- ggplot() +
      geom_histogram(data = df_dives_final, aes(x = dist_km, fill = leg ), binwidth = 5,
                     colour = "black") +
      scale_fill_manual(values = c("#CC0000", "#FF9933")) +
      geom_vline(xintercept = 50, linetype = 2) +
      geom_vline(xintercept = 100, linetype = 2) +
      geom_vline(xintercept = 150, linetype = 2) +
      geom_vline(xintercept = 200, linetype = 2) +
      ggtitle("c.") +
      ##add text for sample size
      annotate(geom = "text", x =25 , y = 125, label = "N = 1590 dives", color = "black") +
      labs(x = "Distance from colony (km)", y = "Total dives", fill = "Trip Leg") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            ##panel.border = element_blank(), 
            legend.position = "right",
            axis.title=element_text(size=12),
            text = element_text(size = 12),
            strip.text.x = element_text(size = 12))


## define filepath to read out plots 

out_path <- here("Outputs", "Plots", "Data_visualisation")

## save as a tiff   

ggsave(plot = e, filename = "all_gannet_dives_colony_distance_histogram.tiff",
       device = "tiff",
       path = out_path, units = units, width = 150, height = 125, dpi = dpi,   
)


#-----------------------------------------------#
##7. Create facet plot for manuscript Fig. 1 ####
#-----------------------------------------------#


## create facet plot using patchwork and cowplot with inset map
## first create facet plot for top row

g <- ggdraw() +
      draw_plot(b + d) +
      draw_plot(a, width = 0.2, height = 0.5)


h <- ggarrange(g, e, nrow = 2, ncol = 1)

## set filepath for reading out the plot

out_path <- here("Figures")


## save as a tiff

ggsave(plot = h, filename = "Figure_1_inset.tiff",
       device = "tiff",
       path = out_path, units = units, width = 180, height = 200, dpi = dpi,   
)

##save as a jpeg

ggsave(plot = h, filename = "Figure_1_inset.jpeg",
       device = "jpeg",
       path = out_path, units = units, width = 180, height = 200, dpi = dpi,   
)


#----------------------------------#
## Annotate with gannet graphics####
#----------------------------------#

## read composite map back in to annotate with the graphics

fig_img <- image_read(here("Figures", "Figure_1_inset.jpeg"))

## read in graphic of gannet diving

dive_image <- image_read(here("Gannet_graphics","plunge.png"))
dive_image_altered <- dive_image %>% image_scale("300x300") %>%image_flop()

## annotate image

a <- image_composite(fig_img, dive_image_altered, offset = "+1350+1300")

## save figure image

image_write(a, here("Figures", "Figure_1_annotated.png"))


##get session info

sessionInfo()


#-------------------#
##End of script####
#-------------------#