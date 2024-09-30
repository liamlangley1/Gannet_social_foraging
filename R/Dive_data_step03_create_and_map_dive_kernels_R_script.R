####Script to create and map kernels of gannet dive locations from TDR data
####Liam Langley
####Based on code from Alice Trevail

#----------------------#
##1. load libraries ####
#----------------------#


library(sf)
library(tidyverse)
library(marmap)
library(sp)
library(adehabitatHR)
library(ggspatial)
library(ggnewscale)
library(patchwork)
library(here)


#-------------------------#
##2. load in dive data ####
#-------------------------#

## all spatially referenced dives from tracked gannets in 2012 and 2013

df_dives <- read_csv(here("Data_outputs", "Working_data_frames", "dive_data_1213_cleaned_and_annotated.csv"))

head(df_dives)
str(df_dives)


#------------------------------------------------#
##3. Calculate kernels for all dive locations ####
#------------------------------------------------#

## convert df_gannets to a spatial points data frame for calculating kernels
## need to retain only the co-ordinates or Kernel UD will not work
## can also have a single id column - age/sex/behaviour
## not needed here

gannets.sp <- df_dives[, c("x", "y")]

## convert to spatial points data frame

coordinates(gannets.sp) <- c("x", "y")

## calculate kernels for all GPS points
## for all birds in all years
## convert data frame to an sf object

gannets.sf <- st_as_sf(gannets.sp, coords = c("x", "y"))

## set co-ordinates system for the data
## use EPSG code 4326
## Lat/long using WGS84 datum

st_crs(gannets.sf) <- 4326

## check projection system correct

st_crs(gannets.sf)

## convert projection to Lambert-Azimuth Equal Area Projection for calculating kernels
## centre point = Grassholm - x = -5.467, y = 51.716
## units in metres

gannets.laea <- st_transform(gannets.sf, CRS("+proj=laea +lat_0=51.716 +lon_0=-5.467 +units=m"))

## check projection system 

st_crs(gannets.laea)

## set bounding box as the limits of the tracking data

bbox <- st_bbox(gannets.laea)

## set up grid for calculating kernels over
## add a border of 35km for smoothing outside the bounding box
## set the grid size as 1km

x <- seq(as.numeric(bbox["xmin"])-50000,as.numeric(bbox["xmax"])+50000,by=1000)
y <- seq(as.numeric(bbox["ymin"])-50000,as.numeric(bbox["ymax"])+50000,by=1000)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE

## transform co-ordinate projection of the sp object to laea

gannets.sp <- as(gannets.laea, "Spatial")

## calculate kernels
## set smoothing parameter to 10km - following Carter et al. 2016

gannet_UD <- kernelUD(gannets.sp, h = 10000, kern = "bivnorm", grid = xy)

## warning about proj4 string - doesn't seem to be a problem
## extract the KDEs from the UD

gannet_kernel_25 <- getverticeshr(gannet_UD, percent = c(25))
gannet_kernel_50 <- getverticeshr(gannet_UD, percent = c(50)) 
gannet_kernel_75 <- getverticeshr(gannet_UD, percent = c(75)) 
gannet_kernel_95 <- getverticeshr(gannet_UD, percent = c(95))

## convert KDEs to data frames for mapping
## first convert individual kernels to data frames
## add a unique identifier for each KDE using the mutate function

df_gannet_kde_25 <- fortify(gannet_kernel_25) %>% mutate(percent = "25")
df_gannet_kde_50 <- fortify(gannet_kernel_50) %>% mutate(percent = "50")
df_gannet_kde_75 <- fortify(gannet_kernel_75) %>% mutate(percent = "75")
df_gannet_kde_95 <- fortify(gannet_kernel_95) %>% mutate(percent = "95")

## group into a single data frame

kernel_df <- do.call(rbind, lapply(ls(pattern = "df_gannet_kde_"), get))

## add column for smoothing parameter used
## useful to record for reference

kernel_df$h <- paste(gannet_UD@`h`$`h`)

## save data frame
## define filepath for saved file

filepath_dfout <- here("Data_outputs", "Working_data_frames")

## save file

write_csv(kernel_df, file = here(filepath_dfout, "gannet_kernels_all_dives.csv"))


#---------------------------------#
##4. Map kernels for all dives ####
#---------------------------------#

## convert co-ordinate projection of kernel data frame to lat/long

sf_kernels <- sf::st_as_sf(kernel_df, coords = c("long", "lat"))
st_crs(sf_kernels) <- CRS("+proj=laea +lat_0=51.716 +lon_0=-5.467 +units=m")
sf_kernels_ll <- st_transform(sf_kernels, 4326) 

## check CRS

st_crs(sf_kernels_ll)

## ggplot won't recognise kernels as polygons in this format
## need to convert data frame to a multipolygons object
## use formula from Alice

sf_kernels_poly = st_sf(
  aggregate(
    sf_kernels_ll$geometry,
    list(sf_kernels_ll$piece, sf_kernels_ll$percent),
    function(g){
      st_cast(st_combine(g),"POLYGON")
    }
  ))

## save kernels as a shape file using st_write function
## define filepath

filepath_kernels <- here("Data_outputs", "Kernel_files")

## save out kernels

st_write(sf_kernels_poly, here(filepath_kernels, "kernels_all_dives.shp"), driver = "ESRI Shapefile")


#------------------------------------#
##5. Read in basemap for plotting ####
#------------------------------------#

## create file path for base map 

map_path <- here ("Base_map", "BI_countries approxGBprj_labelled.shp")

## read in base map

basemap <- st_read(map_path)

## basemap is in British National Grid projection - OSGB 1936
## transform basemap to lat long to be in same projection as the kernels

basemap_ll <- st_transform(basemap, crs = st_crs(sf_kernels_ll))


#-------------------#
##6. Map kernels ####
#-------------------#

## set xlim and ylim values based on extent of tracking data

xlim = c(min(df_dives$x - 0.4), max(df_dives$x + 0.4))

ylim = c(min(df_dives$y - 0.4), max(df_dives$y + 0.4))

## convert %UD to a factor and relevel so 95% UD plotted first

sf_kernels_poly$Group.2 <- factor(sf_kernels_poly$Group.2, levels = c("95", "75", "50", "25"))

## set up distance buffers
## create a data frame for the colony centroid

df_centroid <- data.frame(x = -5.467, y = 51.716)

## convert to sf object and add CRS

centroidsf <- st_as_sf(df_centroid, coords = c("x", "y"))

st_crs(centroidsf) <- 4326

## create buffers
## 50km, 100km and 150km from the colony

buffer50 <- st_buffer(centroidsf, dist = 50000, nQuadSegs = 500)

buffer100 <- st_buffer(centroidsf, dist = 100000, nQuadSegs = 500)

## create map

a <- ggplot()+
      ##add distance buffers atround colony
      geom_sf(data = buffer50, aes(geometry = geometry), alpha = 0, col = "black") +
      geom_sf(data = buffer100, aes(geometry = geometry), alpha = 0, col = "black") +
      ##add kernels
      geom_sf(data = sf_kernels_poly, aes(geometry = geometry, alpha = Group.2),
              fill = "#FF9933", lwd = 0.4) +
      ##set scale for alpha - shading of kernels
      scale_alpha_manual(values = c(0.2, 0.4, 0.6, 0.9)) +
      ##add basemap of British Islaes outline
      geom_sf(data = basemap_ll, aes(geometry = geometry, fill = "COUNTRY")) +
      scale_fill_manual(values = "#CCCCCC", guide = "none") +
      ##set plot limits
      coord_sf(xlim = xlim, ylim = ylim, crs = 4326) +
      ##add colony point
      geom_point(data =  data.frame(x = -5.467, y = 51.716),
                 aes(x = x, y = y), colour = "#FF3300",
                 fill ="#FF3300", shape = 23, size = 2) +
      labs(x = "Longitude", y = "Latitude", alpha = "%UD") +
      theme(axis.text=element_text(colour="black"),
            ##Hide panel borders and remove grid lines
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "#CCFFFF"),
            axis.title.x = element_text(size = 15),
            axis.text.x = element_text(hjust=0.7),
            axis.title.y = element_text(angle=90, vjust = 0.4, size = 15),
            axis.text.y = element_text(hjust=0.7,angle=45,vjust=0.3))


## save map
## define device to read map out as e.g. tiff/jpeg

device <- "tiff"

## define units for plot size - usually mm

units <- "mm"

## define plot resolution in dpi - 300 usually minimum

dpi <- 300

## define filepath to read out maps

out_path <- here("Outputs", "Maps", "Kernels")

## save plot

ggsave(plot = a, filename = "gannet_kernel_map_all_dives.tiff",
       device = device,
       path = out_path ,units = units, width = 150, height = 160, dpi = dpi,   
)


#------------------#
##End of script ####
#------------------#