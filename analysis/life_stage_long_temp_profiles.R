#############################################################
#
# Created by: Mike Ackerman
# Created date: January 2020
# Purpose:
#   I want to generate plots by watershed and life stage showing
#   the longitudinal temperature profile for the time period within 
#   that life stage and show optimum, max, and acute (other?) temps on the profile
#   with geom_ribbon and geom_hline. Will show rkms where temperatures exceeds 
#   (or subceeds) a given threshold.
#
#   To be used in MRA reports to help in project prioritization.
#
#############################################################

## load necessary libraries
library(lubridate)
library(dplyr)
library(sf)

# load all of the prepped data
load("data/mra_temp_rkm_redd_data_prepped.Rdata")

# trim year off of dates in mra_threshold
mra_threshold = mra_threshold %>%
  mutate(start_date = ymd(start_date) - years(1), # converting to a non-leap year
         end_date = ymd(end_date) - years(1)) %>%
  mutate(start_julian = yday(start_date),
         end_julian = yday(end_date))

# attach rkms to the mcnyset dataset and average temps across modeled years
temp_rkm = mra_mcnyset %>%
  st_join(mra_rkm,
          join = st_nearest_feature,
          left = TRUE) %>%
  dplyr::select(-variable, -RCAID, -River) %>%
  dplyr::select(watershed, year, rkm, everything()) %>%
  arrange(watershed, rkm, year) %>%
  group_by(watershed, rkm) %>%
  #summarise_at(vars(starts_with("d")), funs(mean))
  summarise_at(vars(starts_with("d")), list(mean))


         