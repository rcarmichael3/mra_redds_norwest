#############################################################
# Prep temperature, redd, and rkm data in MRA watersheds 
# for analysis
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
library(ggplot2)
library(tidyverse)
library(readxl)

# load all of the prepped data
load("data/mra_temp_rkm_redd_data_prepped.Rdata")

##########################################
# LEMHI RIVER ANALYSIS AND VISUALIZATION #
##########################################
lemh_temp_rkm_sf = mra_mcnyset %>%
  filter(watershed == "Lemhi") %>%
  dplyr::select(watershed, year, variable, RCAID, d241) %>% # day 241 = Aug 29
  st_join(mra_rkm,
          join = st_nearest_feature,
          left = TRUE) %>%
  dplyr::select(- River) %>%
  arrange(rkm) %>%
  group_by(rkm) %>%
  summarise(mn_max_8d_temp_d241 = mean(d241)) %>%
  filter(!rkm == "92") # there was an odd model estimate at the confluence with Salmon R.

# map with McNyset Aug 29 modeled temps
lemh_temp_rkm_map = lemh_temp_rkm_sf %>%
  ggplot(aes(colour = mn_max_8d_temp_d241)) +
  scale_colour_distiller(palette = "Spectral") +
  geom_sf(size = 1.3) +
  theme_bw() +
  labs(title = "Lemhi River",
       colour = "Mean of 8-day Max Temps\nAug 29 (C)")
lemh_temp_rkm_map

# the above, but add redds
lemh_temp_rkm_redds_map = lemh_temp_rkm_sf %>%
  ggplot() +
  geom_sf(aes(color = mn_max_8d_temp_d241),
          size = 4) +
  scale_colour_distiller(palette = "Spectral") +
  geom_sf(data = mra_redds %>%
            filter(Waterbody == "Lemhi River"),
          size = 1,
          shape = 1) +
  #position = position_nudge(x = 20000, y = 20000)) +
  # geom_sf_label(data = mra_redds %>%
  #                 filter(Waterbody == "Lemhi River"),
  #               label = '',
  #               nudge_x = -5000,
  #               size = 0.01,
  #               label.size = 0.3) +
  theme_bw() +
  labs(title = "Lemhi River",
       colour = "Mean of 8-day Max Temps\nAug 29 (C)") +
  theme(axis.title = element_blank())
lemh_temp_rkm_redds_map

# basic longitudinal temperature profile
lemh_temp_rkm_sf %>%
  ggplot() +
  geom_line(aes(x = rkm, y = mn_max_8d_temp_d241),
            size = 1.3) +
  geom_smooth(aes(x = rkm, y = mn_max_8d_temp_d241),
              method = "loess", se = F) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 91, by = 5)) +
  labs(x = "River Kilometer",
       y = "Mean of 8-day Max Temps\nAug 29 (C)",
       title = "Lemhi River Longitudinal Temp Profile")

# calculate average redds per rkm
lemh_redd_rkm = mra_redds %>%
  filter(Waterbody == "Lemhi River") %>%
  dplyr::select(Waterbody, Species, SurveyYear, StartDate, Longitude, Latitude) %>%
  st_join(mra_rkm,
          join = st_nearest_feature,
          left = TRUE) %>%
  group_by(rkm, SurveyYear) %>%
  summarise(n_redds = n()) %>%
  ungroup() %>%
  complete(rkm = 0:91, SurveyYear, fill = list(n_redds = 0)) %>% # need to set max rkm here
  dplyr::select(- geometry) %>%
  group_by(rkm) %>%
  summarise(mean_redds_km = mean(n_redds)) %>%
  ungroup()

# longitudinal temperature profile, with redd densities
coeff = 2.2 # for data transformation for secondary y-axis
sec_x_0 = 12
lemh_temp_rkm_sf %>%
  left_join(lemh_redd_rkm) %>%
  ggplot(aes(x = rkm)) +
  geom_bar(aes(y = mean_redds_km), stat = "identity", alpha = 0.3,
           fill = "steelblue4", colour = "steelblue4") +
  geom_line(aes(y = (mn_max_8d_temp_d241 - sec_x_0) * coeff),
            size = 1.1, colour = "black") +
  geom_smooth(aes(y = (mn_max_8d_temp_d241 - sec_x_0) * coeff),
              method = "loess",
              se = F,
              size = 1, colour = "red3") +
  scale_y_continuous(sec.axis = sec_axis(~./coeff + sec_x_0,
                                         name = "Mean of 8-day Max Temp, Aug 29 (C)")) +
  labs(x = "River Kilometer",
       y = "Mean Redd Density (redds/km)",
       title = "Lemhi River") +
  theme_bw()

# removing objects that begin with
rm(list = ls(pattern = "^lemh_")) # might make sense to remove this later

# temp joined to rkm
# rkm_temp_sf = mra_rkm %>%
#   filter(River == "Lemhi River") %>%
#   st_join(salmon_mcnyset_sf,
#           join = st_nearest_feature,
#           left = TRUE) %>%
#   dplyr::select(watershed, rkm, year, variable, d241)
#
# temperature map, point type
# rkm_temp_p = rkm_temp_sf %>%
#   ggplot(aes(colour = d241)) +
#   geom_sf() +
#   scale_colour_distiller(palette = "Spectral") +
#   theme_bw() +
#   labs(colour = "8-day Mean Temperature, Aug 29 (C)")
# rkm_temp_p

###############################################
# PAHSIMEROI RIVER ANALYSIS AND VISUALIZATION #
###############################################
pahs_temp_rkm_sf = mra_mcnyset %>%
  filter(watershed == "Pahsimeroi") %>%
  dplyr::select(watershed, year, variable, RCAID, d241) %>% # day 241 = Aug 29
  st_join(mra_rkm,
          join = st_nearest_feature,
          left = TRUE) %>%
  dplyr::select(- River) %>%
  arrange(rkm) %>%
  group_by(rkm) %>%
  summarise(mn_max_8d_temp_d241 = mean(d241)) %>%
  filter(!rkm == "86") # observation with an NA at confluence with Salmon R.

# map with McNyset Aug 29 modeled temps
pahs_temp_rkm_map = pahs_temp_rkm_sf %>%
  ggplot(aes(colour = mn_max_8d_temp_d241)) +
  scale_colour_distiller(palette = "Spectral") +
  geom_sf(size = 1.3) +
  theme_bw() +
  labs(title = "Pahsimeroi River",
       colour = "Mean of 8-day Max Temps\nAug 29 (C)")
pahs_temp_rkm_map

# the above, but add redds
pahs_temp_rkm_redds_map = pahs_temp_rkm_sf %>%
  ggplot() +
  geom_sf(aes(color = mn_max_8d_temp_d241),
          size = 4) +
  scale_colour_distiller(palette = "Spectral") +
  geom_sf(data = mra_redds %>%
            filter(Waterbody == "Pahsimeroi River"),
          size = 1,
          shape = 1) +
  #position = position_nudge(x = 20000, y = 20000)) +
  # geom_sf_label(data = mra_redds %>%
  #                 filter(Waterbody == "Lemhi River"),
  #               label = '',
  #               nudge_x = -5000,
  #               size = 0.01,
  #               label.size = 0.3) +
  theme_bw() +
  labs(title = "Pahsimeroi River",
       colour = "Mean of 8-day Max Temps\nAug 29 (C)") +
  theme(axis.title = element_blank())
pahs_temp_rkm_redds_map
# ha, note that all the redds appear to be low, presumably below the weir
# I suspect these are largely hatchery redds

# basic longitudinal temperature profile
pahs_temp_rkm_sf %>%
  ggplot() +
  geom_line(aes(x = rkm, y = mn_max_8d_temp_d241),
            size = 1.3) +
  geom_smooth(aes(x = rkm, y = mn_max_8d_temp_d241),
              method = "loess", se = F) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 85, by = 5)) +
  labs(x = "River Kilometer",
       y = "Mean of 8-day Max Temps\nAug 29 (C)",
       title = "Pahsimeroi River Longitudinal Temp Profile")

# calculate average redds per rkm
pahs_redd_rkm = mra_redds %>%
  filter(Waterbody == "Pahsimeroi River") %>%
  dplyr::select(Waterbody, Species, SurveyYear, StartDate, Longitude, Latitude) %>%
  st_join(mra_rkm,
          join = st_nearest_feature,
          left = TRUE) %>%
  group_by(rkm, SurveyYear) %>%
  summarise(n_redds = n()) %>%
  ungroup() %>%
  complete(rkm = 0:85, SurveyYear, fill = list(n_redds = 0)) %>% # need to set max rkm here
  dplyr::select(- geometry) %>%
  group_by(rkm) %>%
  summarise(mean_redds_km = mean(n_redds)) %>%
  ungroup()

# longitudinal temperature profile, with redd densities
coeff = 2.5 # for data transformation for secondary y-axis
sec_x_0 = 11
pahs_temp_rkm_sf %>%
  left_join(pahs_redd_rkm) %>%
  ggplot(aes(x = rkm)) +
  geom_bar(aes(y = mean_redds_km), stat = "identity", alpha = 0.3,
           fill = "steelblue4", colour = "steelblue4") +
  geom_line(aes(y = (mn_max_8d_temp_d241 - sec_x_0) * coeff),
            size = 1.1, colour = "black") +
  geom_smooth(aes(y = (mn_max_8d_temp_d241 - sec_x_0) * coeff),
              method = "loess",
              se = F,
              size = 1, colour = "red3") +
  scale_y_continuous(sec.axis = sec_axis(~./coeff + sec_x_0,
                                         name = "Mean of 8-day Max Temp, Aug 29 (C)")) +
  labs(x = "River Kilometer",
       y = "Mean Redd Density (redds/km)",
       title = "Pahsimeroi River") +
  theme_bw()

# removing objects that begin with
rm(list = ls(pattern = "^pahs_")) # might make sense to remove this later

#################################################
# UPPER SALMON RIVER ANALYSIS AND VISUALIZATION #
#################################################
upsa_temp_rkm_sf = mra_mcnyset %>%
  filter(watershed == "Upper Salmon") %>%
  dplyr::select(watershed, year, variable, RCAID, d241) %>% # day 241 = Aug 29
  st_join(mra_rkm,
          join = st_nearest_feature,
          left = TRUE) %>%
  dplyr::select(- River) %>%
  arrange(rkm) %>%
  group_by(rkm) %>%
  summarise(mn_max_8d_temp_d241 = mean(d241))

# map with McNyset Aug 29 modeled temps
upsa_temp_rkm_map = upsa_temp_rkm_sf %>%
  ggplot(aes(colour = mn_max_8d_temp_d241)) +
  scale_colour_distiller(palette = "Spectral") +
  geom_sf(size = 1.3) +
  coord_sf(crs = 4326, # changing the coord_sf to widen the map
           xlim = c(-114.96, -114.65),
           expand = T) +
  theme_bw() +
  labs(title = "Upper Salmon River",
       colour = "Mean of 8-day Max Temps\nAug 29 (C)") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0))
upsa_temp_rkm_map

# the above, but add redds
# what is the easiest way to remove redds in the below plot that are x distance
# away from the mra_mcnyset line?
upsa_temp_rkm_redds_map = upsa_temp_rkm_sf %>%
  ggplot() +
  geom_sf(aes(color = mn_max_8d_temp_d241),
          size = 4) +
  scale_colour_distiller(palette = "Spectral") +
  geom_sf(data = mra_redds %>%
            filter(Waterbody == "Salmon River"),
          size = 1,
          shape = 1) +
  #position = position_nudge(x = 20000, y = 20000)) +
  # geom_sf_label(data = mra_redds %>%
  #                 filter(Waterbody == "Lemhi River"),
  #               label = '',
  #               nudge_x = -5000,
  #               size = 0.01,
  #               label.size = 0.3) +
  theme_bw() +
  labs(title = "Salmon River",
       colour = "Mean of 8-day Max Temps\nAug 29 (C)") +
  theme(axis.title = element_blank())
upsa_temp_rkm_redds_map

# basic longitudinal temperature profile
upsa_temp_rkm_sf %>%
  ggplot() +
  geom_line(aes(x = rkm, y = mn_max_8d_temp_d241),
            size = 1.3) +
  geom_smooth(aes(x = rkm, y = mn_max_8d_temp_d241),
              method = "loess", se = F) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  labs(x = "River Kilometer",
       y = "Mean of 8-day Max Temps\nAug 29 (C)",
       title = "Pahsimeroi River Longitudinal Temp Profile")

# calculate average redds per rkm
upsa_redd_rkm = mra_redds %>%
  filter(Waterbody == "Salmon River") %>%
  dplyr::select(Waterbody, Species, SurveyYear, StartDate, Longitude, Latitude) %>%
  st_join(mra_rkm,
          join = st_nearest_feature,
          left = TRUE) %>%
  filter(rkm <= 56) %>%
  group_by(rkm, SurveyYear) %>%
  summarise(n_redds = n()) %>%
  ungroup() %>%
  complete(rkm = 0:56, SurveyYear, fill = list(n_redds = 0)) %>% # need to set max rkm here
  dplyr::select(- geometry) %>%
  group_by(rkm) %>%
  summarise(mean_redds_km = mean(n_redds)) %>%
  ungroup()

# longitudinal temperature profile, with redd densities
coeff = 14 # for data transformation for secondary y-axis
sec_x_0 = 12.5
upsa_temp_rkm_sf %>%
  left_join(upsa_redd_rkm) %>%
  ggplot(aes(x = rkm)) +
  geom_bar(aes(y = mean_redds_km), stat = "identity", alpha = 0.3,
           fill = "steelblue4", colour = "steelblue4") +
  geom_line(aes(y = (mn_max_8d_temp_d241 - sec_x_0) * coeff),
            size = 1.1, colour = "black") +
  geom_smooth(aes(y = (mn_max_8d_temp_d241 - sec_x_0) * coeff),
              method = "loess",
              se = F,
              size = 1, colour = "red3") +
  scale_y_continuous(sec.axis = sec_axis(~./coeff + sec_x_0,
                                         name = "Mean of 8-day Max Temp, Aug 29 (C)")) +
  labs(x = "River Kilometer",
       y = "Mean Redd Density (redds/km)",
       title = "Upper Salmon River") +
  theme_bw()

# removing objects that begin with
rm(list = ls(pattern = "^upsa_")) # might make sense to remove this later

##################################################
# ALL MRA WATERSHEDS, ANALYSIS AND VISUALIZATION #
##################################################

# Joining redd, rkm, and norwest temp data
redd_norw_rkm = mra_redds %>%
  st_join(mra_rkm,
          join = st_nearest_feature,
          left = TRUE) %>%
  st_join(mra_norwest,
          join = st_nearest_feature,
          left = TRUE)

# Joining rkm to norwest temp data
norw_rkm = mra_norwest %>%
  st_join(mra_rkm,
          join = st_nearest_feature,
          left = TRUE)

# Histogram of norwest temps at spawning locations
redd_norw_rkm %>%
  ggplot(aes(x = S36_201,
             color = River,
             fill = River)) + # from Richie, presumably mean August temp?
  geom_histogram(alpha = 0.3, position = "dodge") +
  # geom_density(alpha = 0.3) + # option for density plot instead
  theme_bw() +
  labs(x = "Mean August Temp (C)",
       y = "Frequency",
       title = "Temperature at Redd Locations")

# plot of all available temps
mra_norwest %>%
  ggplot(aes(x = S21_201,      # which scenario to use, not sure which this is?
             color = GNIS_NA,
             fill = GNIS_NA)) +
  # geom_histogram(alpha = 0.3, position = "dodge") +
  geom_density(alpha = 0.3) +
  theme_bw() +
  labs(x = "Mean Aug Temp (C)", # not certain this label is correct
       y = "Frequency/Density",
       color = "River",
       fill = "River",
       title = "Norwest Temp Distributions")

# all long temp profiles
norw_rkm %>%
  ggplot(aes(x = rkm,
             y = S36_201,
             group = GNIS_NA)) +
  geom_line(aes(color = GNIS_NA)) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Mean August Temp",
       color = "River")

# grab some threshold values
chnk_spw_opt_low = mra_threshold %>%
  filter(species == "chinook",
         life_stage == "spawning",
         threshhold == "optimum_low") %>%
  dplyr::select(temp_c) %>%
  as.numeric()

chnk_spw_opt_high = mra_threshold %>%
  filter(species == "chinook",
         life_stage == "spawning",
         threshhold == "optimum_high") %>%
  dplyr::select(temp_c) %>%
  as.numeric()

chnk_sum_juv_opt_low = mra_threshold %>%
  filter(species == "chinook",
         life_stage == "summer_parr",
         threshhold == "optimum_low") %>%
  dplyr::select(temp_c) %>%
  as.numeric()

chnk_sum_juv_opt_high = mra_threshold %>%
  filter(species == "chinook",
         life_stage == "summer_parr",
         threshhold == "optimum_high") %>%
  dplyr::select(temp_c) %>%
  as.numeric()

# plot all August long temp profiles, add temp thresholds for summer lifestages
norw_rkm %>%
  filter((GNIS_NA == "Lemhi River") |
           (GNIS_NA == "Pahsimeroi River") |
           (GNIS_NA == "Salmon River" & rkm < 57)) %>%
  ggplot(aes(x = rkm,
             y = S21_201)) +
  geom_line(aes(color = GNIS_NA)) +
  geom_smooth(aes(color = GNIS_NA),
              method = "loess",
              se = F) +
  scale_x_continuous(breaks = seq(0, 90, by = 5)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  # optimum spawning temps
  geom_ribbon(aes(ymin = chnk_spw_opt_low,
                  ymax = chnk_spw_opt_high),
              fill = "orange",
              alpha = 0.2) +
  # optimum summer juvenile rearing temps
  geom_ribbon(aes(ymin = chnk_sum_juv_opt_low,
                  ymax = chnk_sum_juv_opt_high),
              fill = "blue",
              alpha = 0.2) +
  # geom_hline(yintercept = chnk_spw_opt_low:chnk_spw_opt_high,
  #            alpha = 0.5) +
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Mean August Temp",
       color = "Watershed",
       title = "Mean August Temperature Profile (NorWeST)")

# cleaning up
#rm(norw_rkm, redd_norw_rkm)
