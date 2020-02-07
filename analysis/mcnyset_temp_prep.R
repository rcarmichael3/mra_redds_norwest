## Prepping available temp results from McNyset et al. for analysis #
#
# Created by: Mike Ackerman
# Created date: February 2020
#
# Attemping to clean all of the temperature results we have from the
# McNyset et al. model to use for analysis. This could prove useful for 
# a number of other analyses/repos that we have as well.
#
#############################################################

## load necessary libraries
library(dplyr)
library(sf)

############################
# MCNYSET TEMPERATURE DATA #
############################

################
# LEMHI 8D MEAN
lemh_2011_8d_mn = st_read("../mra_fish_n_hydro/data/lemhi_temp_shapefiles/Lemhi_2011/Lem_2011_8D_mn.shp") %>%
  mutate_at(vars(starts_with("Tmn")), list(~na_if(., "-9999"))) %>%
  rename_at(vars(starts_with("Tmn")), funs(paste0("Tmn_", substring(., 8, 10)))) %>%
  mutate(year = "2011") %>%
  dplyr::select(RCAID, year, starts_with("Tmn"))

lemh_2012_8d_mn = st_read("../mra_fish_n_hydro/data/lemhi_temp_shapefiles/Lemhi_2012/Lem_2012_8D_mn.shp") %>%
  mutate_at(vars(starts_with("Tmn")), list(~na_if(., "-9999"))) %>%
  rename_at(vars(starts_with("Tmn")), funs(paste0("Tmn_", substring(., 8, 10)))) %>%
  mutate(year = "2012") %>%
  dplyr::select(RCAID, year, starts_with("Tmn"))

lemh_2013_8d_mn = st_read("../mra_fish_n_hydro/data/lemhi_temp_shapefiles/Lemhi_2013/Lem_2013_8D_mn.shp") %>%
  rename_at(vars(starts_with("X")), funs(paste0("Tmn_", substring(., 4, 6)))) %>%
  mutate(year = "2013") %>%
  rename(RCAID = LEM_RCAID) %>%
  dplyr::select(RCAID, year, starts_with("Tmn"))

# bind the lemhi 8d mean data together
lemh_8d_mn = lemh_2011_8d_mn %>%
  rbind(lemh_2012_8d_mn) %>%
  rbind(lemh_2013_8d_mn) %>%
  mutate(watershed = "Lemhi") %>%
  dplyr::select(watershed, year, RCAID, everything())

# clean up lemhi mcnyset data
rm(lemh_2011_8d_mn, lemh_2012_8d_mn, lemh_2013_8d_mn)

################
# LEMHI 8D MAX
lemh_2011_8d_mx = st_read("data/mcnyset_temp/lemhi/2011/Lem_2011_8D_Mx.shp") %>%
  dplyr::select(RCAID, starts_with("TMx")) %>%
  mutate(year = "2011") %>%
  rename_at(vars(starts_with("TMx")), funs(paste0("Tmx_", substring(., 8, 10))))

lemh_2012_8d_mx = st_read("data/mcnyset_temp/lemhi/2012/Lem_2012_8D_Mx.shp") %>%
  dplyr::select(RCAID, starts_with("TMx")) %>%
  mutate(year = "2012") %>%
  rename_at(vars(starts_with("TMx")), funs(paste0("Tmx_", substring(., 8, 10))))

lemh_2013_8d_mx = st_read("data/mcnyset_temp/lemhi/2013/Lem_2013_8D_Mx.shp") %>%
  dplyr::select(RCAID, starts_with("TMx")) %>%
  mutate(year = "2013") %>%
  rename_at(vars(starts_with("TMx")), funs(paste0("Tmx_", substring(., 8, 10))))

lemh_2014_8d_mx = st_read("data/mcnyset_temp/lemhi/2014/Lem_2014_8D_Mx.shp") %>%
  dplyr::select(RCAID, starts_with("TMx")) %>%
  mutate(year = "2014") %>%
  rename_at(vars(starts_with("TMx")), funs(paste0("Tmx_", substring(., 8, 10))))

lemh_2015_8d_mx = st_read("data/mcnyset_temp/lemhi/2015/Lem_2015_8D_Mx.shp") %>%
  dplyr::select(RCAID, starts_with("TMx")) %>%
  mutate(year = "2015") %>%
  rename_at(vars(starts_with("TMx")), funs(paste0("Tmx_", substring(., 8, 10))))

# bind the pahsimeroi 8d max data together
lemh_8d_mx = lemh_2011_8d_mx %>%
  rbind(lemh_2012_8d_mx) %>%
  rbind(lemh_2013_8d_mx) %>%
  rbind(lemh_2014_8d_mx) %>%
  rbind(lemh_2015_8d_mx) %>%
  mutate(watershed = "Lemhi") %>%
  dplyr::select(watershed, year, RCAID, everything())

# clean up pahsimeroi data
rm(lemh_2011_8d_mx, lemh_2012_8d_mx, lemh_2013_8d_mx, lemh_2014_8d_mx, lemh_2015_8d_mx)

###################
# PAHSIMEROI 8D MAX
pahs_2011_8d_mx = st_read("data/mcnyset_temp/pahsimeroi/2011/Pahs_2011_8D_Mx.shp") %>%
  dplyr::select(RCAID, starts_with("TMx")) %>%
  mutate(year = "2011") %>%
  rename_at(vars(starts_with("TMx")), funs(paste0("Tmx_", substring(., 8, 10))))

pahs_2013_8d_mx = st_read("data/mcnyset_temp/pahsimeroi/2013/Pahs_2013_8D_Mx.shp") %>%
  dplyr::select(RCAID, starts_with("TMx")) %>%
  mutate(year = "2013") %>%
  rename_at(vars(starts_with("TMx")), funs(paste0("Tmx_", substring(., 7, 9))))

# bind the pahsimeroi 8d max data together
pahs_8d_mx = pahs_2011_8d_mx %>%
  rbind(pahs_2013_8d_mx) %>%
  mutate(watershed = "Pahsimeroi") %>%
  dplyr::select(watershed, year, RCAID, everything())

# clean up pahsimeroi data
rm(pahs_2011_8d_mx, pahs_2013_8d_mx)

#####################
# UPPER SALMON 8D MAX
upsa_2011_8d_max = st_read("data/mcnyset_temp/upper_salmon/2011/USal_2011_8D_Mx.shp") %>%
  rename(RCAID = rca_id) %>%
  dplyr::select(RCAID, starts_with("TMx")) %>%
  mutate(year = "2011") %>%
  rename_at(vars(starts_with("TMx")), funs(paste0("Tmx_", substring(., 8, 10))))

upsa_2013_8d_max = st_read("data/mcnyset_temp/upper_salmon/2013/USal_2013_8D_Mx.shp") %>%
  dplyr::select(RCAID, starts_with("TMx")) %>%
  mutate(year = "2013") %>%
  rename_at(vars(starts_with("TMx")), funs(paste0("Tmx_", substring(., 8, 10))))

# bind the upper salmon 8d max data together
upsa_8d_mx = upsa_2011_8d_max %>%
  rbind(upsa_2013_8d_max) %>%
  mutate(watershed = "Upper Salmon") %>%
  dplyr::select(watershed, year, RCAID, everything())

# clean up upper salmon data
rm(upsa_2011_8d_max, upsa_2013_8d_max)

###################################
# COMBINE ALL OF THE ABOVE DATASETS
lemh_8d_mn = lemh_8d_mn %>%
  rename_at(vars(starts_with("Tmn")), funs(paste0("d", substring(., 5, 7)))) %>%
  mutate(variable = "8_day_mean_C")

lemh_8d_mx = lemh_8d_mx %>%
  rename_at(vars(starts_with("TMx")), funs(paste0("d", substring(., 5, 7)))) %>%
  mutate(variable = "8_day_max_C")

pahs_8d_mx = pahs_8d_mx %>%
  rename_at(vars(starts_with("TMx")), funs(paste0("d", substring(., 5, 7)))) %>%
  mutate(variable = "8_day_max_C")

upsa_8d_mx = upsa_8d_mx %>%
  rename_at(vars(starts_with("TMx")), funs(paste0("d", substring(., 5, 7)))) %>%
  mutate(variable = "8_day_max_C")

salmon_mcnyset_sf = lemh_8d_mn %>%
  rbind(lemh_8d_mx) %>%
  rbind(pahs_8d_mx) %>%
  rbind(upsa_8d_mx) %>%
  dplyr::select(watershed, year, variable, RCAID,
                everything())

# clean up all the datasets
rm(lemh_8d_mn, lemh_8d_mx, pahs_8d_mx, upsa_8d_mx)

# write out final results
st_write(salmon_mcnyset_sf, "data/mcnyset_temp/salmon_mcnyset_sf.shp")
         #, delete_layer = T) # to overwrite existing file) 

