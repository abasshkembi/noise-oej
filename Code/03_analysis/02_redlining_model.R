# Title: Redlining, mortgage discrimination model
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: Nov 22, 2024

setwd("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Noise OEJ/")
library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(MetBrewer)

# read in final dataframe file
final_noise_df <- readRDS("Data/Generated Data/final_noise_df.RDS")

# read in final shapefile file
oej_tracts_semi <- read_sf("Data/Generated Data/final_noise_sf.shp")

# fix column names
colnames(oej_tracts_semi)[10:28] <- c(colnames(final_noise_df)[2:18], "ALAND_miles", "pop_density")



# a little processing on the shapefile
### assign tract centroids
tract_centroids <- st_centroid(oej_tracts_semi) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  as_tibble() %>%
  select(GEOID, lon, lat)

oej_tracts_semi <- oej_tracts_semi %>%
  left_join(tract_centroids, by = "GEOID")

# shift geometry of alaska and hawaii
oej_tracts_full <- oej_tracts_semi %>%
  shift_geometry(geoid_column = "GEOID") %>%
  st_transform("EPSG:3082")











# figure 4 - redlining, HMDA, and average noise 

# read in historical redlining scores
hrs_2010 <- readxl::read_xlsx("Data/Historic Redlining Score 2010B.xlsx")
# get unique state fips
unique_red_states <- hrs_2010 %>% mutate(state_fip = str_extract(GEOID10, "^\\d\\d")) %>% .$state_fip %>% unique()


# get total number of households in the year 2000
hh00 <- NULL
for(i in 1:length(unique_red_states)) {
  temp_df <- tidycensus::get_decennial(
    geography = "tract",
    year = 2000,
    sumfile = "sf3",
    variable = "P010001",
    state = unique_red_states[i])
  
  hh00 <- rbind(hh00, temp_df)
}

# get the number of non-Hispanic White households in the year 2000
hh_nhW00 <- NULL
for(i in 1:length(unique_red_states)) {
  temp_df <- tidycensus::get_decennial(
    geography = "tract",
    year = 2000,
    sumfile = "sf3",
    variable = "P146I001",
    state = unique_red_states[i])
  
  hh_nhW00 <- rbind(hh_nhW00, temp_df)
}

# get census tract code crosswalk between the year 2000 and 2010
tract_cross <- read.csv("/Users/abasshkembi/Downloads/crosswalk_2000_2010.csv") %>%
  select(1, 2) %>%
  mutate(trtid00 = ifelse(str_length(trtid00) == 10, paste0("0", trtid00), trtid00),
         trtid10 = ifelse(str_length(trtid10) == 10, paste0("0", trtid10), trtid10))

# merge two household data together
household00 <- hh00 %>%
  select(GEOID, hh00 = value) %>%
  left_join(
    hh_nhW00 %>%
      select(GEOID, hh_nhW00 = value),
    by = "GEOID"
  )

# crosswalk to 2010 tract codes and calculate minority households
household00 <- household00 %>%
  left_join(tract_cross, by = c("GEOID" = "trtid00")) %>%
  select(trtid10, hh00, hh_nhW00) %>%
  group_by(trtid10) %>%
  summarise(hh00 = sum(hh00, na.rm = TRUE),
            hh_nhW00 = sum(hh_nhW00, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(hh_minor00 = hh00 - hh_nhW00) # minority households






# or use population count

pop00 <- read.csv("/Users/abasshkembi/Downloads/ltdb_std_all_fullcount/LTDB_Std_2000_fullcount.csv") %>%
  select(1, 9, 10) %>%
  mutate(TRTID10 = ifelse(str_length(TRTID10) == 10, paste0("0", TRTID10), TRTID10))

household00 <- household00 %>%
  left_join(pop00, by = c("trtid10" = "TRTID10"))







# read in HMDA decadal file
load(file = "Data/hmda_decades.rda")
hmda_decades <- da39093.0001 %>% filter(END_YEAR == 2000)

hmda_2000 <- hmda_decades %>% select(GEOID10 = TRACT_FIPS10, NUM_ORIGINATIONS, NUM_WHITE) %>%
  mutate(NUM_NONWHITE = NUM_ORIGINATIONS - NUM_WHITE)

# merge HRS data and HMDA data
hrs_hmda <- hrs_2010 %>%
  left_join(hmda_2000, by = "GEOID10")

# this one!
hrs_hmda_hh <- hrs_hmda %>%
  left_join(
    household00,
    by = c("GEOID10" = "trtid10")
  )

# get HOLC census tracts
oej_tracts_red <- oej_tracts_full %>%
  mutate(percentile_occ = percent_rank(occ_median_perc_exp),
         percentile_env = percent_rank(env_median_perc_exp)) %>%
  mutate(both_bin = ifelse(percentile_occ > 0.75 & percentile_env > 0.75, 1, 0)) %>%
  inner_join(hrs_hmda_hh, by = c("GEOID" = "GEOID10"))

oej_tracts_red_tibble <- oej_tracts_red %>% as_tibble()


# create decadal mortgage density index (DMDI)
oej_tracts_red_tibble3 <- oej_tracts_red_tibble %>%
  mutate(n_nhWhite = (nhWhite_pct/100)*totalpop,
         n_minor = (minor_pct/100)*totalpop) %>%
  # using 2000 data
  ### number of white loans / num of white people DIVIDED BY num nonwhite loans / num of nonwhite people
  mutate(hmda_index = (NUM_WHITE/NHWHT00)/(NUM_NONWHITE/(POP00 - NHWHT00))) %>%
  mutate(hmda_index = ifelse(is.infinite(hmda_index), NA, hmda_index)) %>%
  mutate(hmda_bin = ifelse(hmda_index >= 1, "yesDiscrim", "noDiscrim")) %>%
  mutate(dmdi = (NUM_NONWHITE/NUM_ORIGINATIONS)/(hh_minor00/hh00)) %>% # the true one
  mutate(dmdi = ifelse(is.infinite(dmdi), NA, dmdi)) %>%
  mutate(dmdi_bin = ifelse(dmdi < 1, "yesDiscrim", "noDiscrim"))

# create final dataset for analysis
## outcome will be `both` = average percentage of occ and env prevalence
fig5_df <- oej_tracts_red_tibble3 %>%
  mutate(both = (occ_median_perc_exp+env_median_perc_exp)/2) %>%
  mutate(n_both = ceiling(totalpop*(both/100))) %>%
  mutate(red = ifelse(INTERVAL2010 == 4, 1, 0)) %>%
  mutate(holc = as.character(INTERVAL2010)) %>%
  mutate(DI_cat = factor(DI_cat, levels = c("Low_moder", "High", "Extreme"))) %>%
  select(GEOID, both, dmdi_bin, HRS2010, INTERVAL2010, unemployed_pct, pop_density, lon, lat, lowinc_pct, noHS_pct, both_bin, red, occ_median_perc_exp, env_median_perc_exp) %>%
  na.omit()


# the final model I'm reporting on
mod_gam_red <- fig5_df %>%
  mgcv::gam(both ~ dmdi_bin:HRS2010 + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21), 
            #offset = log(totalpop),
            family = "gaussian",
            data = .)
summary(mod_gam_red)

# change from 1 to 4 among no discrimination
c(0.51695*4, 0.51695*4 + c(-1.96, 1.96)*0.09464)

# change from 1 to 4 among no discrimination
c(0.61138*4, 0.61138*4 + c(-1.96, 1.96)*0.08815)


# create final figure of regression for Figure 4
fig5_df %>% mutate(pred_both = predict(mod_gam_red)/100) %>%
  ggplot(aes(x = HRS2010, y = pred_both, linetype = dmdi_bin)) +
  #annotate(geom = "rect", xmin = 3.95, xmax = 4.05, ymin = 0.154, ymax = 0.168, fill = "white", color = "grey", alpha = 0) +
  annotate(geom = "text", x = 4, y = 0.11, color = "grey30", label = "Historical\nredlining (1930s)", size = 3) +
  #annotate(geom = "segment", x = 4, xend = 4, y = 0.168, yend = 0.175, color = "grey") +
  
  annotate(geom = "text", x = 1, y = 0.11, color = "grey30", label = "No historical\nredlining (1930s)", size = 3) +
  #annotate(geom = "segment", x = 1, xend = 1, y = 0.129, yend = 0.137, color = "grey") +
  
  annotate(geom = "text", x = 2.5, y = 0.17, color = "grey30", label = "Mortgage discrimination\nagainst minorities (1990-2000)", size = 3) +
  annotate(geom = "segment", x = 2.5, xend = 2.5, y = 0.165, yend = 0.155, color = "grey30", linetype = "dashed") +
  
  annotate(geom = "text", x = 2.5, y = 0.13, color = "grey30", label = "No mortgage discrimination\nagainst minorities (1990-2000)", size = 3) +
  annotate(geom = "segment", x = 2.5, xend = 2.5, y = 0.134, yend = 0.148, color = "grey30") +
  geom_smooth(method = "lm", se = TRUE, show.legend = FALSE, color = "white", fill = "black", size = 0.5, alpha = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels=LETTERS[1:4], breaks=1:4, limits=c(0.7,4.3)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.y = element_line(colour = "grey95"),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 9),
        axis.title = element_text(size = 10)) +
  coord_cartesian(ylim = c(0.106, 0.184)) +
  labs(x = "HOLC Grade", y = "Average percent of noise-exposed population",
       title = "Differences in workplace- and transportation-related noise pollution",
       subtitle = "By historical redlining (1930s) and racial/ethnic mortgage disrimination (1990s)")










# descriptive plots

fig5_df %>%
  ggplot(aes(x = interaction(dmdi_bin, INTERVAL2010), y = both)) +
  ggdist::stat_interval(
    .width = c(.5, .75), 
    size = 7
  ) +
  ggdist::stat_halfeye(
    adjust = .33, ## bandwidth
    width = .7, fill = "grey85",
    interval_colour = NA, point_colour = "black",
    shape = 23, stroke = 1.5, point_size = 5, point_fill = "white",
    position = position_nudge(x = .03),
    aes(thickness = stat(f*n))
  ) +
  theme_classic()
