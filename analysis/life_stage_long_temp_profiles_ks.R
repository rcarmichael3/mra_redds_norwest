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

# set default formating
theme_set(theme_bw())

# load all of the prepped data
load("data/mra_temp_rkm_redd_data_prepped.Rdata")
rm(mra_norwest, mra_redds)

# trim year off of dates in mra_threshold
thresh_df = mra_threshold %>%
  mutate(start_date = ymd(start_date) - years(1), # converting to a non-leap year
         end_date = ymd(end_date) - years(1)) %>%
  mutate(start_julian = yday(start_date),
         end_julian = yday(end_date)) %>%
  mutate_at(vars(temp_c),
            list(as.numeric))

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

#----------------------------------------------------------------
# create faceted plots for each watershed / species combination
#----------------------------------------------------------------
# put all data together
all_data = crossing(watershed = c('Lemhi',
                                  'Pahsimeroi',
                                  'Upper_Salmon'),
                    thresh_df %>%
                      select(species, life_stage, threshold, temp_c) %>%
                      spread(threshold, temp_c)) %>%
  left_join(thresh_df %>%
              select(species:end_date, matches('julian')) %>%
              distinct() %>%
              rowwise() %>%
              mutate(julians = if_else(start_julian < end_julian,
                                       list(start_julian:end_julian),
                                       list(c(start_julian:365, 0:end_julian))),
                     julians = list(paste0("d", str_pad(julians, 3, pad = "0")))) %>%
              ungroup() %>%
              mutate(julians = map(julians,
                                   .f = function(x) {
                                     x[x %in% colnames(mra_mcnyset)]
                                   }))) %>%
  left_join(temp_rkm %>%
              st_drop_geometry() %>%
              group_by(watershed) %>%
              nest() %>%
              rename(temp_data = data) %>%
              left_join(tibble(watershed = c('Lemhi',
                                             'Pahsimeroi',
                                             'Upper_Salmon'),
                               rkms = list(0:91,
                                           0:85,
                                           0:56))) %>%
              mutate(temp_data = map2(temp_data,
                                      rkms,
                                      function(x, y) {
                                        x %>%
                                          filter(rkm %in% y)
                                      })) %>%
              ungroup()) %>%
  mutate(plot_data = map2(temp_data,
                          julians,
                          .f = function(x, y) {
                            x %>%
                              select(rkm, one_of(y)) %>%
                              gather(julian, temp_c, -rkm) %>%
                              group_by(rkm) %>%
                              summarise(mean_c = mean(temp_c),
                                        min_c = min(temp_c),
                                        max_c = max(temp_c),
                                        sd_c = sd(temp_c),
                                        n = n()) %>%
                              ungroup()
                          })) %>%
  mutate(date_span = paste(month(start_date, label = T), day(start_date), 'to', 
                           month(end_date, label = T), day(end_date)),
         my_label = paste(str_to_title(str_replace(life_stage, "_", " ")), date_span, sep = '\n'))

# look at one species/watershed
wtsd = unique(all_data$watershed)[1]
spc = unique(all_data$species)[1]

for(wtsd in unique(all_data$watershed)) {
  for(spc in unique(all_data$species)) {

    all_ls_p = all_data %>%
      filter(watershed == wtsd,
             species == spc) %>%
      select(watershed:life_stage, 
             date_span, my_label,
             acute:optimum_low, 
             plot_data) %>%
      unnest(plot_data) %>%
      ggplot(aes(x = rkm)) +
      # optimum temp range
      geom_ribbon(aes(ymin = optimum_low,
                      ymax = optimum_high),
                  fill = "green",
                  alpha = 0.2) +
      # longitudinal temp profile
      geom_line(aes(y = mean_c), 
                colour = "grey28") +
      geom_smooth(aes(y = mean_c),
                  method = "loess",
                  colour = "black",
                  se = F) +
      geom_line(aes(y = min_c),
                colour = "gray40", 
                linetype = "dashed") +
      geom_line(aes(y = max_c),
                colour = "gray40", 
                linetype = "dashed") +
      # min temp threshold
      geom_hline(aes(yintercept = minimum),
                 colour = "blue", linetype = "dashed") +
      # max temp threshold
      geom_hline(aes(yintercept = maximum),
                 colour = "red", 
                 linetype = "dashed") +
      # acute temp threshold
      geom_hline(aes(yintercept = acute),
                 colour = "darkred") +
      # set axis breaks and limits
      # scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
      # scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
      facet_wrap(~ my_label) +
      labs(x = "River Kilometer",
           y = "Temperature (°C)",
           title = paste(str_to_title(spc), str_to_title(str_replace(wtsd, "_", " ")), sep = ', '))
    all_ls_p
    
    # save plots
    ggsave(filename = paste0("figures/", wtsd, "_", spc, "_ls_long_temp_plots_v2.pdf"),
           all_ls_p)
    
    
