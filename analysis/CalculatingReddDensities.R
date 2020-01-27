library(dplyr)
library(raster)
library(sf)
library(sp)
library(ggplot2)


##Read in RKM files and pre-process##
lemhi_rkm <- st_read("data/rkm/Lemhi_CL_1kmPoints.shp") %>%
  dplyr::select(1, 4) %>%
  mutate(River = "Lemhi River")


pah_rkm <- st_read("data/rkm/Pahsimeroi_CL_1kmPoints.shp") %>%
  dplyr::select(1,4) %>%
  st_transform(crs = crs(lemhi_rkm)) %>%
  mutate(River = "Pahsimeroi River")

 

upsalmon_rkm <- st_read("data/rkm/UpperSalmon_CL_1kmPoints.shp") %>%
  dplyr::select(1,4) %>%
  st_transform(crs = crs(lemhi_rkm)) %>%
  mutate(River = "Salmon River")


mra_rkm <- lemhi_rkm %>%
  rbind(pah_rkm) %>%
  rbind(upsalmon_rkm) %>%
  rename(rkm = Id)

##Read in/filter redd data covnert to sf object##

# all_redds <- st_read("data/redd/F_G_redds_all_UpTo_2018.shp")
# 
# mra_redds <- all_redds %>%
#   filter(CountNew == 1) %>%
#   filter(Waterbody == "Lemhi River" |
#            Waterbody == "Pahsimeroi River" |
#            Waterbody == "Salmon River") %>%
#   st_as_sf(coords = c("Latitude", "Longitude"),
#            crs = 4326) %>%
#   st_transform(crs = crs(lemhi_rkm))
# st_write(mra_redds, "data/redd/mra_redds.shp")

  
mra_redds <- st_read("data/redd/mra_redds.shp") %>%
  st_transform(crs = crs(lemhi_rkm))
  
##Read in norwest temp stuff##

# all_norwest <- st_read("data/norwest/NorWeST_temps.shp")
# 
# mra_norwest <- all_norwest %>%
#   filter(GNIS_NA == "Lemhi River" |
#            GNIS_NA == "Pahsimeroi River" |
#            GNIS_NA == "Salmon River") %>%
#   st_transform(crs = crs(lemhi_rkm))
# 
# st_write(mra_norwest, "data/norwest/mra_norwest.shp")

Lemhi_norw <- st_read("data/norwest/Lemhi_norw.shp")
Pah_norw <- st_read("data/norwest/Pah_norw.shp")
Salmon_norw <- st_read("data/norwest/Salmon_norw.shp")


mra_norwest <- Lemhi_norw %>%
  rbind(Pah_norw) %>%
  rbind(Salmon_norw)

##Joining temperature and rkm to redd data##

redd_norw_rkm <- mra_redds %>%
  st_join(mra_rkm, 
          join = st_nearest_feature, 
          left = TRUE) %>%
  st_join(mra_norwest, 
          join = st_nearest_feature, 
          left = TRUE) 

##Joining RKM to Temp data##

norw_rkm <- mra_norwest %>%
  st_join(mra_rkm, 
          join = st_nearest_feature,
          left = TRUE)

##Hist of norwest temps at spawning locations (hab use temp)##

spwn_selection <- ggplot(redd_norw_rkm,aes(x = S36_201,
                                 color = River, 
                                 fill = River)) +
  geom_histogram(alpha = 0.3, position = "dodge") +
  theme_bw() +
  labs(x = "Mean August Temperature",
       y = "Count") 

spwn_selection

spwn_geom_dens <- ggplot(redd_norw_rkm, 
                    aes(x = S36_201, ##Which scenario to use?
                        color = River, 
                        fill = River)) +
  geom_density(alpha = 0.3) +
  theme_bw() +
  labs(x = "Mean August Temperature",
       y = "Density")

spwn_geom_dens

##Plot of all available temps##
alltemps_dens <- ggplot(mra_norwest, 
                    aes(x = S21_201, ##Which scenario to use?
                        color = GNIS_NA, 
                        fill = GNIS_NA)) +
  geom_density(alpha = 0.3) +
  theme_bw() +
  labs(x = "Mean August Temperature",
       y = "Density")

alltemps_dens



##Plotting Temp/RKM and redd locations##

ggplot(norw_rkm, 
       aes(x = rkm, y = S36_201, group = GNIS_NA)) +
  geom_line(aes(color = GNIS_NA)) +
  theme_bw() +
  labs(x = "Mean August Temperature",
       y = "River Kilometer")


##Making plot for Lemhi only##

Lem_redd_norw <- redd_norw_rkm %>%
  filter(GNIS_NA == "Lemhi River")




