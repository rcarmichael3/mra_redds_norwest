## Calculate observed redd densities in MRA valley segments #
#
# Created by: Richie Carmichael
# Created date: January 2020
#
# Calculating redd densities for display in MRA reports along with 
# NorWest temperature data
#
#############################################################

## load necessary libraryies
library(dplyr)
library(sf)
library(raster)
# library(sp)
library(ggplot2)
library(tidyverse)

########################
# RIVER KILOMETER DATA #
########################
## read in RKM files
lemh_rkm = st_read("data/rkm/Lemhi_CL_1kmPoints.shp") %>%
  dplyr::select(Id) %>%
  mutate(River = "Lemhi River")

pahs_rkm = st_read("data/rkm/Pahsimeroi_CL_1kmPoints.shp") %>%
  dplyr::select(Id) %>%
  st_transform(crs = crs(lemh_rkm)) %>%
  mutate(River = "Pahsimeroi River")

upsa_rkm = st_read("data/rkm/UpperSalmon_CL_1kmPoints.shp") %>%
  dplyr::select(Id) %>%
  st_transform(crs = crs(lemh_rkm)) %>%
  mutate(River = "Salmon River")

# bind the above 3 sf objects together
mra_rkm = lemh_rkm %>%
  rbind(pahs_rkm) %>%
  rbind(upsa_rkm) %>%
  rename(rkm = Id)

# clean up the previous sf objects
rm(lemh_rkm, pahs_rkm, upsa_rkm)

# plot rkm data
rkm_p = mra_rkm %>%
  ggplot(aes(color = River, fill = River)) +
  geom_sf() +
  theme_bw() +
  labs(title = "River Kilometer Data")
rkm_p

#############
# REDD DATA #
#############
# read in, filter redd data and convert to an sf object
mra_redds = st_read("data/redd/F_G_redds_all_UpTo_2018.shp") %>%
  filter(CountNew == 1) %>%    # remove duplicate counted redds
  filter(Waterbody %in% c("Lemhi River", "Pahsimeroi River", "Salmon River")) %>%
  st_as_sf(coords = c("Latitude", "Longitude"),
           crs = 4326) %>%     # set redd data to WGS84
  st_transform(crs = crs(mra_rkm))

# plot redd data
redd_p = mra_redds %>%
  ggplot(aes(color = Waterbody, fill = Waterbody)) +
  geom_sf() +
  theme_bw() +
  labs(title = "Redd Data")
redd_p

# write out cleaned redd data
st_write(mra_redds, "data/redd/mra_redds.shp") 
         #, delete_layer = T) # to overwrite existing file

############################
# MCNYSET TEMPERATURE DATA #
############################
salmon_mcnyset_sf = st_read("data/mcnyset_temp/salmon_mcnyset_sf.shp")

# plot full mcnyset data
full_mcny_p = salmon_mcnyset_sf %>%
  ggplot() +
  geom_sf(colour = "blue") +
  theme_bw() +
  labs(title = "Modeled Temperature Data")
full_mcny_p

# now I trimmed the full dataset in ArcMap to only include MRA watershed mainstems
mra_mcnyset = st_read("data/mcnyset_temp/salmon_mcnyset_sf_mainstems.shp")

# plot full mcnyset data
mra_mcny_p = mra_mcnyset %>%
  ggplot() +
  geom_sf(colour = "blue") +
  theme_bw() +
  labs(title = "Modeled Temperature Data")
mra_mcny_p

############################
# NORWEST TEMPERATURE DATA #
############################
# ## read in norwest temp data
# mra_norwest = st_read("data/norwest/NorWeST_temps.shp") %>%
#   filter(GNIS_NA %in% c("Lemhi River", "Pahsimeroi River", "Salmon River")) %>%
#   st_transform(crs = crs(mra_rkm))
# 
# # write out cleaned mra norwest data
# st_write(mra_norwest, "data/norwest/mra_norwest.shp")
#          #, delete_layer = T) # to overwrite existing file
# 
# # read in the cleaned norwest temp data from each watershed
# lemh_norw = st_read("data/norwest/Lemhi_norw.shp")
# pahs_norw = st_read("data/norwest/Pah_norw.shp")
# upsa_norw = st_read("data/norwest/Salmon_norw.shp")
# 
# # and bind the above 3 sf norwest temp object
# mra_norw = lemh_norw %>%
#   rbind(pahs_norw) %>%
#   rbind(upsa_norw)
# 
# # clean up the previous sf norwest temp objects
# rm(lemh_norw, pahs_norw, upsa_norw)

###############################
# Join temp data and rkm data #
###############################
mra_mcnyset = mra_mcnyset %>%
  st_transform(crs = crs(mra_rkm))

salmon_mcnyset_sf = salmon_mcnyset_sf %>%
  st_transform(crs = crs(mra_rkm))

#################
# START W/ LEMHI
# rkm joined to temp
temp_rkm_sf = mra_mcnyset %>%
  dplyr::select(watershed, year, variable, RCAID, d241) %>%
  filter(watershed == "Lemhi") %>%
  st_join(mra_rkm,
          join = st_nearest_feature,
          left = TRUE) %>%
  dplyr::select(- River) %>%
  arrange(watershed, rkm) %>%
  group_by(rkm) %>%
  summarise(mn_max_8d_tmp_d241 = mean(d241)) %>%
  filter(!rkm == "92") # seems to be an errant estimate at confluence with Salmon

# temperature map, line type
temp_rkm_map = temp_rkm_sf %>%
  ggplot(aes(colour = mn_max_8d_tmp_d241)) +
  scale_colour_distiller(palette = "Spectral") +
  geom_sf() +
  theme_bw() +
  labs(title = "Lemhi River",
       colour = "Mean of 8-day Max Temp, Aug 29 (C)")
temp_rkm_map

# temperature map, line type, add redds
temp_rkm_redd_map = temp_rkm_sf %>%
  ggplot() +
  geom_sf(aes(colour = mn_max_8d_tmp_d241),
          lwd = 2) +
  scale_colour_distiller(palette = "Spectral") +
  # geom_sf(data = mra_redds %>%
  #           filter(Waterbody == "Lemhi River"),
  #         size = 1,
  #         shape = 1,
  #         position = position_nudge(x = 20000, y = 20000)) +
  geom_sf_label(data = mra_redds %>%
            filter(Waterbody == "Lemhi River"),
            label = '',
            nudge_x = 5000,
            size = 0.1,
            label.size = 3) +
  theme_bw() +
  labs(title = "Lemhi River",
       colour = "Mean of 8-day Max Temp, Aug 29 (C)")
temp_rkm_redd_map  

# longitudinal temperature profile
lemh_long_temp_p = temp_rkm_sf %>%
  ggplot() +
  geom_line(aes(x = rkm, y = mn_max_8d_tmp_d241), 
            size = 1.5, colour = "midnightblue") +
  geom_smooth(aes(x = rkm, y = mn_max_8d_tmp_d241),
              method = "loess", se = F) +
  theme_bw() +
  labs(x = "River Kilometer", 
       y = "Mean of 8-day Max Temp, Aug 29 (C)",
       title = "Lemhi River Longitudinal Temp Profile")
lemh_long_temp_p  

# calculate average redds per river km
redd_rkm_sf = mra_redds %>%
  filter(Waterbody == "Lemhi River") %>%
  dplyr::select(Waterbody, Species, SurveyYear, StartDate, Longitude, Latitude) %>%
  st_join(mra_rkm,
          join = st_nearest_feature,
          left = TRUE) %>%
  group_by(rkm, SurveyYear) %>%
  summarise(n_redds = n()) %>%
  ungroup() %>%
  complete(rkm = 0:92, SurveyYear, fill = list(n_redds = 0)) %>%
  dplyr::select(- geometry) %>%
  group_by(rkm) %>%
  summarise(mean_n_redds = mean(n_redds)) %>%
  ungroup()

# longitudinal temperature profile
coeff = 2.2
lemh_long_temp_redd_p = temp_rkm_sf %>%
  left_join(redd_rkm_sf) %>%
  ggplot(aes(x = rkm)) +
  geom_bar(aes(y = mean_n_redds), stat = "identity", alpha = 0.5,
           colour = "cornflowerblue", fill = "cornflowerblue") +
  geom_line(aes(y = (mn_max_8d_tmp_d241 - 12) * coeff),
            size = 1.2, colour = "midnightblue") +
  scale_y_continuous(sec.axis = sec_axis(~./coeff + 12,
                                         name = "Mean of 8-day Max Temp, Aug 29 (C)")) +
  theme_bw() +
  labs(x = "River Kilometer", 
       y = "Mean Redd Density (Redds/km)",
       title = "Lemhi River Longitudinal Temp Profile")
lemh_long_temp_redd_p  
  
# temp joined to rkm
rkm_temp_sf = mra_rkm %>%
  filter(River == "Lemhi River") %>%
  st_join(salmon_mcnyset_sf,
          join = st_nearest_feature,
          left = TRUE) %>%
  dplyr::select(watershed, rkm, year, variable, d241)

# temperature map, point type
# rkm_temp_p = rkm_temp_sf %>%
#   ggplot(aes(colour = d241)) +
#   geom_sf() +
#   scale_colour_distiller(palette = "Spectral") +
#   theme_bw() +
#   labs(colour = "8-day Mean Temperature, Aug 29 (C)")
# rkm_temp_p

############################################
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




