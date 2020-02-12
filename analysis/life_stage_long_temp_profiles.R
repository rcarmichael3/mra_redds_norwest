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

#######################################
# OKAY, ONE SCENARIO AT A TIME... UGH #
#######################################
# set params for a single plot
wtsd = "Lemhi"         # Lemhi, Pahsimeroi, Upper_Salmon
spc  = "chinook"       # chinook, steelhead
ls   = "spring_smolt" # adult_holding, spawning, incubation, emergence, summer_parr, winter_presmolt, spring_smolt

# set rkms for watershed
if(wtsd == "Lemhi")       { rkms = 0:91 }
if(wtsd == "Pahsimeroi")  { rkms = 0:85 }
if(wtsd == "Upper Salmon"){ rkms = 0:56 }
      
# get thresholds for species x life stage
spc_ls_thresh = thresh_df %>%
  filter(species == spc,
         life_stage == ls)

# get names of julians within mra_mcnyset that occur within species x life_stage
if(unique(spc_ls_thresh$start_julian) < unique(spc_ls_thresh$end_julian)) {
  spc_ls_julians = unique(spc_ls_thresh$start_julian):unique(spc_ls_thresh$end_julian)
}
if(unique(spc_ls_thresh$start_julian) > unique(spc_ls_thresh$end_julian)) {
  spc_ls_julians = c(unique(spc_ls_thresh$start_julian):365,
                     0:unique(spc_ls_thresh$end_julian))
}
spc_ls_julians = paste0("d", str_pad(spc_ls_julians, 3, pad = "0"))
spc_ls_julians = spc_ls_julians[spc_ls_julians %in% colnames(mra_mcnyset)]

# trim temp_rkm for watershed and columns in spc_ls_julians
spc_ls_wtsd_temps = temp_rkm %>%
  st_drop_geometry() %>%
  filter(watershed == wtsd) %>%
  filter(rkm %in% rkms) %>%
  dplyr::select(watershed, rkm, one_of(spc_ls_julians)) %>%
  gather(key = "julian", value = "temp_c", c(-watershed, -rkm)) %>%
  group_by(rkm) %>%
  summarise(mean_c = mean(temp_c),
            min_c = min(temp_c),
            max_c = max(temp_c),
            sd_c = sd(temp_c),
            n = n()) %>%
  ungroup()

# grab rounded min and max temps for plotting
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
min_temp = round_any(min(spc_ls_wtsd_temps$min_c), 0.5, f = floor)
max_temp = round_any(max(spc_ls_wtsd_temps$max_c), 0.5, f = ceiling)
      
# extract date_span out of life stage thresholds
date_span = paste0(str_sub(unique(spc_ls_thresh$start_date), start = -5),
                   " to ", 
                   str_sub(unique(spc_ls_thresh$end_date), start = -5))
      
# extract thresholds and assign as object
for(i in 1:nrow(spc_ls_thresh)) {
  assign(as.character(spc_ls_thresh[i,"threshold"]),
         as.numeric(spc_ls_thresh[i,"temp_c"]))
}
      
# start watershed, species, life_stage plot
ls_p = spc_ls_wtsd_temps %>%
  ggplot(aes(x = rkm)) +
  # longitudinal temp profile
  geom_line(aes(y = mean_c), colour = "grey40") +
  geom_smooth(aes(y = mean_c),
              method = "loess",
              colour = "black",
              se = F) +
  geom_line(aes(y = min_c), colour = "red", linetype = "dashed") +
  geom_line(aes(y = max_c), colour = "red", linetype = "dashed") +
  # min temp threshold
  geom_hline(yintercept = minimum, colour = "blue", linetype = "dashed") +
  # optimum temp range
  geom_ribbon(aes(ymin = optimum_low,
                  ymax = optimum_high),
              fill = "green",
              alpha = 0.2) +
  # max temp threshold
  geom_hline(yintercept = maximum, colour = "red", linetype = "dashed") +
  # acute temp threshold
  geom_hline(yintercept = acute, colour = "red") +
  # set axis breaks and limits
  scale_x_continuous(breaks = seq(0, max(rkms), by = 5)) +
  scale_y_continuous(breaks = seq(min_temp, max_temp, by = 0.5)) +
  coord_cartesian(ylim = c(min_temp, max_temp),
                  expand = F) +
  # some formatting
  theme_bw() +
  labs(x = "River Kilometer",
       y = "Temperature (°C)",
       title = paste0(ls, ", ", date_span)) +
  theme(plot.title = element_text(hjust = 0.01, vjust = -8, size = 12))
ls_p

# name and assign plot
assign(paste0(wtsd,"_",spc,"_",ls,"_p"), ls_p)

##############################
# END ONE SCENARIO AT A TIME #
##############################
#wtsd = "Lemhi"
#spc  =  "chinook"

# EACH WATERSHED X SPECIES
wtsd_spc_ls_p = ggarrange(plotlist = list(get(paste0(wtsd,"_",spc,"_","adult_holding_p")),
                                          get(paste0(wtsd,"_",spc,"_","spawning_p")),
                                          get(paste0(wtsd,"_",spc,"_","incubation_p")),
                                          get(paste0(wtsd,"_",spc,"_","emergence_p")),
                                          get(paste0(wtsd,"_",spc,"_","summer_parr_p")),
                                          get(paste0(wtsd,"_",spc,"_","winter_presmolt_p")),
                                          get(paste0(wtsd,"_",spc,"_","spring_smolt_p"))),
                          nrow = 3,
                          ncol = 3)
wtsd_spc_ls_p
assign(paste0(wtsd,"_",spc,"_p"), wtsd_spc_ls_p) 
