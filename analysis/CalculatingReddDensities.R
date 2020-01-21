library(dplyr)
library(raster)
library(sf)
library(sp)
library(ggplot2)

indir <- "F:/Redd_data/indir"
outdir <- "F:/Redd_data/outdir"

##Read in RKM files and pre-process##
lemhi_rkm <- st_read(paste0(indir, "/Lemhi_CL_1kmPoints.shp")) %>%
  dplyr::select(17, 18) %>%
  mutate(River = "Lemhi River")


pah_rkm <- st_read(paste0(indir, "/Pahsimeroi_CL_1kmPoints.shp")) %>%
  dplyr::select(4,5) %>%
  st_transform(crs = crs(lemhi_rkm)) %>%
  mutate(River = "Pahsimeroi River")

 

upsalmon_rkm <- st_read(paste0(indir, "/UpperSalmon_CL_1kmPoints.shp")) %>%
  dplyr::select(4,5) %>%
  st_transform(crs = crs(lemhi_rkm)) %>%
  mutate(River = "Salmon River")


mra_rkm <- lemhi_rkm %>%
  rbind(pah_rkm) %>%
  rbind(upsalmon_rkm)

##Read in/filter redd data covnert to sf object##

all_redds <- st_read(paste0(indir, "/F_G_redds_all_UpTo_2018.shp"))

mra_redds <- all_redds %>%
  filter(CountNew == 1) %>%
  filter(Waterbody == "Lemhi River" | 
           Waterbody == "Pahsimeroi River" |
           Waterbody == "Salmon River") %>%
  st_as_sf(coords = c("Latitude", "Longitude"),
           crs = 4326) %>%
  st_transform(crs = crs(lemhi_rkm))
  

  
##Read in norwest temp stuff##

all_norwest <- st_read(paste0(indir, "/NorWeST_temps.shp"))

mra_norwest <- all_norwest %>%
  filter(GNIS_NA == "Lemhi River" |
           GNIS_NA == "Pahsimeroi River" |
           GNIS_NA == "Salmon River") %>%
  st_transform(crs = crs(lemhi_rkm)) 


##Joining temperature and rkm to redd data##

redd_norw_rkm <- mra_redds %>%
  st_join(mra_rkm, 
          join = st_nearest_feature, 
          left = TRUE) %>%
  st_join(mra_norwest, 
          join = st_nearest_feature, 
          left = TRUE) %>%
  dplyr::select(7, 9, 13, 14, 17, 18, 23:27, 29, 31:33, 35:73)

##Joining RKM to Temp data##

norw_rkm <- mra_norwest %>%
  st_join(mra_rkm, 
          join = st_nearest_feature,
          left = TRUE)

##Hist of norwest temps at spawning locations##

hist <- ggplot(redd_norw_rkm,aes(x = S36_201,
                                 color = River, 
                                 fill = River)) +
  geom_histogram(alpha = 0.3)

hist

geom_dens <- ggplot(redd_norw_rkm, 
                    aes(x = S36_201, 
                        color = River, 
                        fill = River)) +
  geom_density(alpha = 0.3) +
  theme_bw() +
  labs(x = "Mean August Temperature",
       y = "Density")

geom_dens


##Plotting Temp/RKM and redd locations##








