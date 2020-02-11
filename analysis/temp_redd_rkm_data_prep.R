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
mra_rkm %>%
  ggplot(aes(color = River, fill = River)) +
  geom_sf() +
  theme_bw() +
  labs(title = "River Kilometer Data")

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
mra_redds %>%
  ggplot(aes(color = Waterbody, fill = Waterbody)) +
  geom_sf() +
  theme_bw() +
  labs(title = "Redd Data")

# write out cleaned redd data
st_write(mra_redds, "data/redd/mra_redds.shp") 
         #, delete_layer = T) # to overwrite existing file

############################
# MCNYSET TEMPERATURE DATA #
############################
salmon_mcnyset_sf = st_read("data/temperature/mcnyset/salmon_mcnyset_sf.shp")

# plot full mcnyset data
salmon_mcnyset_sf %>%
  ggplot() +
  geom_sf(colour = "blue") +
  theme_bw() +
  labs(title = "Modeled Temperature Data")
rm(salmon_mcnyset_sf)

# now I trimmed the full dataset in ArcMap to only include MRA watershed mainstems
mra_mcnyset = st_read("data/temperature/mcnyset/salmon_mcnyset_sf_mainstems.shp") %>%
  st_transform(crs = crs(mra_rkm))

# plot full mcnyset data
mra_mcnyset %>%
  ggplot() +
  geom_sf(colour = "blue", size = 1.2) +
  theme_bw() +
  labs(title = "Modeled Temperature Data")

############################
# NORWEST TEMPERATURE DATA #
############################
# ## read in norwest temp data
# mra_norwest = st_read("data/temperature/norwest/NorWeST_temps.shp") %>%
#   filter(GNIS_NA %in% c("Lemhi River", "Pahsimeroi River", "Salmon River")) %>%
#   st_transform(crs = crs(mra_rkm))
# 
# # write out cleaned mra norwest data
# st_write(mra_norwest, "data/temperature/norwest/mra_norwest.shp")
#          #, delete_layer = T) # to overwrite existing file

# read in the cleaned norwest temp data from each watershed
lemh_norw = st_read("data/temperature/norwest/Lemhi_norw.shp")
pahs_norw = st_read("data/temperature/norwest/Pah_norw.shp")
upsa_norw = st_read("data/temperature/norwest/Salmon_norw.shp")

# and bind the above 3 norwest sf objects
mra_norwest = lemh_norw %>%
  rbind(pahs_norw) %>%
  rbind(upsa_norw)

# clean up the previous sf norwest temp objects
rm(lemh_norw, pahs_norw, upsa_norw)

##############################
# LIFE STAGE TEMP THRESHOLDS #
##############################
mra_threshold = read_xlsx("data/temperature/carter_2005_temp_thresholds.xlsx")

##################################
# WRITE OUT ALL THE PREPPED DATA #
##################################
save(mra_rkm, mra_redds, mra_norwest, mra_mcnyset, mra_threshold,
     file = "data/mra_temp_rkm_redd_data_prepped.Rdata")

#-----------------------------------------------
# END DATA PREP
