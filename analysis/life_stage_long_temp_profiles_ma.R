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
library(tidyverse)
library(lubridate)
library(dplyr)
library(sf)
library(stringr)
library(ggpubr)

# load all of the prepped data
load("data/mra_temp_rkm_redd_data_prepped.Rdata")
rm(mra_norwest, mra_redds)

# trim year off of dates in mra_threshold
thresh_df = mra_threshold %>%
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
  summarise_at(vars(starts_with("d")), list(mean)) %>%
  ungroup() %>%
  mutate(watershed = recode(watershed, `Upper Salmon` = "Upper_Salmon"))

rm(mra_mcnyset, mra_rkm, mra_threshold)

# a helper function I use below
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

##########################################################################################
# MIKE'S APPROACH TO WATERSHED X SPECIES X LIFE STAGE LONGITUDINAL TEMP PLOTS, 2/13/2019 #
##########################################################################################

# set params for a scenario
# wtsd = "Lemhi"        # Lemhi, Pahsimeroi, Upper_Salmon
# spc  = "chinook"      # chinook, steelhead
# ls   = "adult_holding" # adult_holding, spawning, incubation, emergence, summer_parr, winter_presmolt, spring_smolt

wtsds = c("Lemhi", "Pahsimeroi", "Upper_Salmon")
spcs  = c("chinook", "steelhead")
lss   = c("adult_holding", "spawning", "incubation", "emergence",
          "summer_parr", "winter_presmolt", "spring_smolt")

# create blank data frame
all_data = NULL

# begin watershed x species x life stage loops
for(wtsd in wtsds) {
  for(spc in spcs) {
    for(ls in lss) {
    
      # start by setting rkms for the given watershed
      if(wtsd == "Lemhi")       { rkms = 0:91 }
      if(wtsd == "Pahsimeroi")  { rkms = 0:85 }
      if(wtsd == "Upper Salmon"){ rkms = 0:56 }
      
      # now, grab the thresholds for a given species x life stage
      spc_ls_thresh = thresh_df %>%
        filter(species == spc,
               life_stage == ls)
      
      # next, get the names of julians within temp_rkm that occur within species x life stage
      if(unique(spc_ls_thresh$start_julian) < unique(spc_ls_thresh$end_julian)) {
        spc_ls_julians = unique(spc_ls_thresh$start_julian):unique(spc_ls_thresh$end_julian)
      }
      if(unique(spc_ls_thresh$start_julian) > unique(spc_ls_thresh$end_julian)) {
        spc_ls_julians = c(unique(spc_ls_thresh$start_julian):365,
                           0:unique(spc_ls_thresh$end_julian))
      }
      spc_ls_julians = paste0("d", str_pad(spc_ls_julians, 3, pad = "0"))
      spc_ls_julians = spc_ls_julians[spc_ls_julians %in% colnames(temp_rkm)]
      
      # extract date_span out of life stage thresholds
      date_span = paste0(str_sub(unique(spc_ls_thresh$start_date), start = -5),
                         " to ", 
                         str_sub(unique(spc_ls_thresh$end_date), start = -5))
      
      # extract thresholds and assign as object
      for(i in 1:nrow(spc_ls_thresh)) {
        assign(as.character(spc_ls_thresh[i,"threshold"]),
               as.numeric(spc_ls_thresh[i,"temp_c"]))
      }
      
      # trim temp_rkm for watershed and columns in spc_ls_julians
      df = temp_rkm %>%
        st_drop_geometry() %>%
        filter(watershed == wtsd) %>%
        filter(rkm %in% rkms) %>%
        dplyr::select(rkm, one_of(spc_ls_julians)) %>%
        gather(key = "julian", value = "temp_c", c(-rkm)) %>%
        group_by(rkm) %>%
        summarise(mean_c = mean(temp_c),
                  min_c = min(temp_c),
                  max_c = max(temp_c),
                  sd_c = sd(temp_c),
                  n = n()) %>%
        ungroup() %>%
        mutate(watershed = wtsd,
               species = spc,
               life_stage = ls,
               min_th = minimum,
               opt_l_th = optimum_low,
               opt_h_th = optimum_high,
               max_th = maximum,
               acute_th = acute,
               ls_span = date_span) %>%
        mutate(min_obs_temp = min(min_c),
               max_obs_temp = max(max_c),
               max_rkm = max(rkm)) %>%
        mutate(label = paste(paste0(str_to_title(str_replace(life_stage, "_", " "))),
                             str_replace_all(date_span, "-", "/"),
                             sep = "\n"))
    
      all_data = rbind(all_data, df)
  
    } # end life stage loop
  } # end species loop
} # end watershed loop

##########################
### NOW BEGIN PLOTTING ###
##########################

# look at one species/watershed
wtsd = wtsds[1]
spc  = spcs[1]

# loop over watershed x species
for(wtsd in wtsds) {
  for(spc in spcs) {
    all_data_p = all_data %>%
      filter(watershed == wtsd,
             species == spc) %>%
      ggplot(aes(x = rkm)) +
      # longitudinal temp profile
      geom_line(aes(y = mean_c), colour = "grey40") +
      geom_smooth(aes(y = mean_c),
                  method = "loess",
                  colour = "black",
                  se = F) +
      geom_line(aes(y = min_c), colour = "black", linetype = "dashed") +
      geom_line(aes(y = max_c), colour = "black", linetype = "dashed") +
      # min temp threshold
      geom_hline(aes(yintercept = min_th),
                 colour = "blue", linetype = "longdash") +
      # optimum temp range
      geom_ribbon(aes(ymin = opt_l_th,
                      ymax = opt_h_th),
                  fill = "green2",
                  alpha = 0.2) +
      # max temp threshold
      geom_hline(aes(yintercept = max_th),
                 colour = "red", linetype = "longdash") +
      # acute temp threshold
      geom_hline(aes(yintercept = acute_th),
                 colour = "darkred") +
      # set axis breaks and limits
      scale_x_continuous(breaks = seq(0, 150, by = 5)) +
      scale_y_continuous(breaks = seq(0, 25, by = 2.5)) +
      # some formatting
      facet_wrap(~ label) +
      theme_bw() +
      labs(x = "River Kilometer",
           y = "Temperature (°C)",
           title = paste0(str_to_title(spc), ", ", wtsd))
    all_data_p
    
    # save plots
    ggsave(filename = paste0("figures/", wtsd, "_", spc, "_ls_long_temp_plots.pdf"),
           all_data_p)
    
  }
}


