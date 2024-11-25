# Title: Simulation of environmental noise measurements for every census tract in the US by state
# Author: Keshav Patel (keshavnp@umich.edu)
# Date: 02.20.2024

library(tidyverse)
library(tidycensus)
library(tigris)
library(leaflet)
library(raster)
library(sf)
# define point.in.poly function from point.in.poly.R
source("/Users/keshavpatel/University of Michigan Dropbox/Keshav Patel/Noise OEJ/Code/00_helper_functions/point.in.poly.R")

#list of state abbreviations
all_states <- c ("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
              "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
              "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
# No rail files: 2=AK, 12=HI, 42=SD, 51=WY
i <- 1
current_state <- all_states[i] 
current_state


road_filename <- paste0("/Users/keshavpatel/Dropbox (University of Michigan)/Noise OEJ/Data/DOT Noise Rasters/State_rasters/",current_state,"_road_noise_2018.tif")
rail_filename <- paste0("/Users/keshavpatel/Dropbox (University of Michigan)/Noise OEJ/Data/DOT Noise Rasters/State_rasters/",current_state,"_rail_noise_2018.tif")
aviation_filename <- paste0("/Users/keshavpatel/Dropbox (University of Michigan)/Noise OEJ/Data/DOT Noise Rasters/State_rasters/",current_state,"_aviation_noise_2018.tif")


Road_raster <- raster(road_filename, RAT = TRUE)
Road_points <- rasterToPoints(Road_raster, spatial = TRUE)
Road_points2 <- spTransform(Road_points, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
Rail_raster <- raster(rail_filename, RAT = TRUE)
Rail_points <- rasterToPoints(Rail_raster, spatial = TRUE)
Rail_points2 <- spTransform(Rail_points, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
Air_raster <- raster(aviation_filename, RAT = TRUE)
Air_points <- rasterToPoints(Air_raster, spatial = TRUE)
Air_points2 <- spTransform(Air_points, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

blocks_shp <- blocks(state = current_state, year = 2017)
bg_shp3 <- st_transform(blocks_shp, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# time = 6.379417 minutes


#road noise point in poly - time
road_shp <- point.in.poly(x = Road_points2, y = bg_shp3, sp = TRUE, duplicate = TRUE)
#rail noise point in poly 
rail_shp <- point.in.poly(x = Rail_points2, y = bg_shp3, sp = TRUE, duplicate = TRUE)
#air noise point in poly 
air_shp <- point.in.poly(x = Air_points2, y = bg_shp3, sp = TRUE, duplicate = TRUE)
#time = 10.11 minutes


Road_data <- road_shp@data %>%
  as_tibble() %>%
  rename(GEOID = GEOID10)
Rail_data <- rail_shp@data %>%
  as_tibble() %>%
  rename(GEOID = GEOID10)
Air_data <- air_shp@data %>%
  as_tibble() %>%
  rename(GEOID = GEOID10)

#group different types of data together
all_noisedata <- Road_data %>%
  rename("value" = paste0(current_state,"_road_noise_2018")) %>%
  mutate(source = "road") %>%
  rbind(
    Rail_data %>%
      rename("value" = paste0(current_state,"_rail_noise_2018")) %>%
      mutate(source = "rail") 
  ) %>%
  rbind(
    Air_data %>%
      rename("value" = paste0(current_state,"_aviation_noise_2018")) %>%
      mutate(source = "air") 
  )

all_data_small <- all_noisedata %>% dplyr::select(value, source, GEOID)

block_pop <- get_decennial(
  geography = "block",
  state = current_state,
  variables = "P001001", # total population
  year = 2010,
  geometry = FALSE
)


# filter data for blocks with at least 1 person living there
block_pop2 <- block_pop %>% filter(value > 0)

# get number of env samples by block
all_data_geoid <- all_data_small %>%
  group_by(GEOID) %>% count %>%
  ungroup()

# merge pop count with env sample count
block_pop_noise <- block_pop2 %>%
  left_join(all_data_geoid, by = "GEOID") %>%
  mutate(n = ifelse(is.na(n), 0, n))

# subset data into three
## 1. blocks with at least two env noise sample
## 2. blocks with one env noise sample
## 3. blocks with no env noise sample 
# value = population, n = number of env samples
block_pop_noise_2 <- block_pop_noise %>% filter(n > 1)
block_pop_noise_1 <- block_pop_noise %>% filter(n == 1)
block_pop_noise_0 <- block_pop_noise %>% filter(n == 0)

# create df with all people without env noise sample
# `above_55 = 0`
df_env_samp_0 <- tibble(GEOID = rep(block_pop_noise_0$GEOID, block_pop_noise_0$value)) %>%
  mutate(above_55 = 0)

nrow(df_env_samp_0) == sum(block_pop_noise_0$value) # TRUE

# create df with all people with one env noise sample
all_data_small_1 <- all_data_small %>%
  group_by(GEOID) %>%
  summarise(n = n(), mean = mean(value)) %>%
  ungroup() %>%
  filter(n == 1) %>%
  mutate(above_55 = ifelse(mean >= 55, 1, 0))

block_pop_noise_1_above55 <- block_pop_noise_1 %>%
  inner_join(all_data_small_1, by = c("GEOID", "n"))

#attribute probability to each person
df_env_samp_1 <- tibble(GEOID = rep(block_pop_noise_1_above55$GEOID, block_pop_noise_1_above55$value),
                        above_55 = rep(block_pop_noise_1_above55$above_55, block_pop_noise_1_above55$value))

nrow(df_env_samp_1) == sum(block_pop_noise_1$value) # TRUE

#start for loop

# create probablities of env noise sample >= 55 dBA for each GEOID
all_data_small_2 <- all_data_small %>%
  mutate(above_55 = ifelse(value >= 55, 1, 0)) %>%
  group_by(GEOID) %>%
  summarise(n = n(), n_55 = sum(above_55)) %>%
  ungroup() %>%
  filter(n > 1) %>%
  mutate(prob_55 = n_55/n)

all_data_small_2_below55 <- all_data_small_2 %>% filter(prob_55 == 0)

all_data_small_2_below55_df <- block_pop_noise_2 %>% inner_join(all_data_small_2_below55, by = c("GEOID", "n"))

df_env_samp_2_below55 <- tibble(GEOID = rep(all_data_small_2_below55_df$GEOID, all_data_small_2_below55_df$value)) %>%
  mutate(above_55 = 0)

# the blocks with some samples above 55
all_data_small_2_above55 <- all_data_small_2 %>% filter(prob_55 > 0)

all_data_small_2_above55_df <- block_pop_noise_2 %>% inner_join(all_data_small_2_above55, by = c("GEOID", "n"))


all_simul <- c()
Iter <- 100

filename_simul <- paste0("/Users/keshavpatel/Dropbox (University of Michigan)/Noise OEJ/Data/Generated Data/Simulation/Environmental Simulated data")
dir.create(file.path(filename_simul, current_state))

for (j in 1:Iter) {
  df_simul_above55 <- tibble(GEOID = rep(all_data_small_2_above55_df$GEOID, all_data_small_2_above55_df$value),
                             above_55 = rbinom(
                               n = rep(all_data_small_2_above55_df$GEOID, all_data_small_2_above55_df$value), 
                               size = 1, 
                               prob = rep(all_data_small_2_above55_df$prob_55, all_data_small_2_above55_df$value)))
  

  
  temp <- df_simul_above55 %>%
    rbind(df_env_samp_0) %>%
    rbind(df_env_samp_1) %>%
    rbind(df_env_samp_2_below55) %>%
    mutate(simulation = j) %>%
    mutate(GEOID_tract = str_extract(GEOID, "^\\d{11}")) %>% # get first 11 digits for tract GEOID
    group_by(GEOID_tract) %>%
    summarise(n = n(), n_above_55 = sum(above_55)) %>% 
    mutate(perc_above_55 = n_above_55/n)
  
filename_simulj <- paste0 (filename_simul, "/", current_state, "/env_simul", j, "_", current_state, ".Rds" )
saveRDS(temp, file = filename_simulj)
  
  all_simul <- rbind (all_simul, temp)
  
  
  # 409151 rows
  if((j %% 10) == 0) {
    print(paste0(current_state, " Iteration: ", paste0(j, paste0(" / ", Iter))))
  }

}
# loop time = 13.27324 minutes

all_env_data_summary <- all_simul %>%
  group_by(GEOID_tract) %>%
  summarise(value = mean (n), mean_above_55 = mean(perc_above_55),
            sd = sd(perc_above_55), median_above_55 = median(perc_above_55),
            q2.5 = quantile(perc_above_55, prob = 0.025),
            q97.5 = quantile(perc_above_55, prob = 0.975)
  )

filename_agg_simul <- paste0("/Users/keshavpatel/Dropbox (University of Michigan)/Noise OEJ/Data/Generated Data/Final state aggregated data/Transportation/env_finaldata_", current_state, "_s", Iter, ".Rds")
saveRDS(all_env_data_summary, file = filename_agg_simul)

assign(paste0(current_state,"_final_estimate_df"), all_env_data_summary)

#state map
for (k in current_state) {
  tracts <- tracts(k, cb = TRUE, year = 2017)
  noise_shp <- tracts %>%
    left_join(all_env_data_summary, by = c("GEOID" = "GEOID_tract"))
  
  

  equal_breaks <- classInt::classIntervals(noise_shp$mean_above_55,
                                           n=5,
                                           style = "jenks")
  cols <- RColorBrewer::brewer.pal(5, "YlOrRd")
  
  fig <- noise_shp %>%
    ggplot() +
    geom_sf(aes(fill = mean_above_55, color = mean_above_55), linewidth = 0.1) +
    scale_fill_stepsn(colors = cols,
                      breaks = equal_breaks$brks,
                      name = "Percent overexposed (%)", 
                      na.value = "#f0f0f0") +
    scale_color_stepsn(colors = cols,
                       breaks = equal_breaks$brks,
                       name = "Percent overexposed (%)", 
                       na.value = "#f0f0f0") +
    theme_void()
  
}
fig

## histograms for above_55 simulations, by tracts
library(ggplot2)
ggplot (all_env_data_summary, aes(mean_above_55)) + geom_histogram()
ggplot (all_env_data_summary, aes(median_above_55)) + geom_histogram()

ggplot (all_env_data_summary, aes(sd)) + geom_histogram()
ggplot (all_env_data_summary, aes (mean_above_55 - median_above_55)) + geom_histogram()

ggplot (all_env_data_summary, aes(q2.5)) + geom_histogram()
ggplot (all_env_data_summary, aes(q97.5)) + geom_histogram()







