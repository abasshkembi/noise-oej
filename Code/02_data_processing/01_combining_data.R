# Title: Data integration
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: Nov 22, 2024

setwd("/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Noise OEJ/")
library(tidyverse)
library(tidycensus)
library(sf)

env_files <- list.files("Data/Generated Data/Final state aggregated data/Transportation")
occ_files <- list.files("Data/Generated Data/Final state aggregated data/Workplace")
full_files <- c(env_files, occ_files)

# full files first
df_oej <- NULL
for(i in 1:length(full_files)) {
  temp_filename <- full_files[i]
  
  if(str_detect(temp_filename, "^env")) {
    temp_df <- readRDS(paste0("Data/Generated Data/Final state aggregated data/Transportation/", temp_filename))
    temp_df <- temp_df %>% 
      transmute(GEOID = GEOID_tract, 
                n = value, 
                mean_perc_exp = round(mean_above_55*100, 1),
                median_perc_exp = round(median_above_55*100, 1),
                sd_perc_exp = round(sd*100, 2),
                q2.5_perc_exp = round(q2.5*100, 1),
                q97.5_perc_exp = round(q97.5*100, 1)
      ) %>%
      mutate(noise = "env",
             threshold = 55)
  } else {
    temp_df <- readRDS(paste0("Data/Generated Data/Final state aggregated data/Workplace/", temp_filename))
    temp_df <- temp_df %>%
      transmute(GEOID = GEOID, 
                n = n_occ, 
                mean_perc_exp = mean_perc_85,
                median_perc_exp = median_perc_85,
                sd_perc_exp = sd_perc_85,
                q2.5_perc_exp = q2.5_perc_85,
                q97.5_perc_exp = q97.5_perc_85
      ) %>%
      mutate(noise = "occ",
             threshold = 85)
  }
  
  df_oej <- rbind(df_oej, temp_df)
}

# keep estimates with at least 20 workers or 20 residents

df_oej <- df_oej %>%
  mutate(filter_env = ifelse((noise == "occ" & n > 20) | (noise == "env" & n > 20), 1, 0)) %>%
  filter(filter_env == 1) %>%
  select(-filter_env)


# read in demographics data
load(file = "/Users/abasshkembi/University of Michigan Dropbox/Abas Shkembi/Noise OEJ/Data/acs2017_demos_tract.RData")
df_oej_demo <- df_oej %>%
  left_join(acs2017_demos, by = "GEOID") %>%
  filter(!is.na(totalpop))

df1 <- df_oej_demo %>%
  select(-mean_perc_exp, -sd_perc_exp, -q2.5_perc_exp, -q97.5_perc_exp, -threshold)

df2 <- df1 %>% filter(noise == "env") %>%
  mutate(n_env = n,
         env_median_perc_exp = median_perc_exp) %>%
  select(-noise, -n, -median_perc_exp) 

df3 <- df1 %>% filter(noise == "occ") %>%
  mutate(n_occ = n,
         occ_median_perc_exp = median_perc_exp) %>%
  select(GEOID, n_occ, occ_median_perc_exp) 

df4 <- full_join(df2, df3, by = "GEOID")

# read in the rural-urban census tract classifications
ruca_codes <- readxl::read_xlsx("Data/ruca2010revised.xlsx", skip = 1)
ruca_codes <- ruca_codes %>%
  select(GEOID = `State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)`,
         ruca = `Primary RUCA Code 2010`) %>%
  mutate(rural = ifelse(ruca > 3, "rural", "urban"))

df5 <- df4 %>% left_join(ruca_codes, by = "GEOID")

final_noise_df <- df5

# save the final dataframe
### as RDS
saveRDS(final_noise_df, "Data/Generated Data/final_noise_df.RDS")
### as CSV
write.csv(final_noise_df, "Data/Generated Data/final_noise_df.csv", row.names = FALSE)





# read in the census tract shapefiles
library(tigris)
options(tigris_use_cache = TRUE)

unique_state_fips <- df_oej_demo$GEOID %>% str_extract("^\\d\\d") %>% unique

us_tracts <- NULL
for(i in 1:length(unique_state_fips)) {
  state_tracts <- tracts(state = unique_state_fips[i], cb = T, year = 2017)
  
  us_tracts <- rbind(us_tracts, state_tracts)
}
  

oej_tracts_semi <- us_tracts %>% 
  left_join(df5, by = "GEOID") %>%
  mutate(ALAND_miles = ALAND/2.59e+6) %>%
  mutate(pop_density = round(totalpop/ALAND_miles, 1))

write_sf(oej_tracts_semi, "Data/Generated Data/final_noise_sf.shp")












### some basic nationwide prevalences

# environmental noise
df_oej_demo %>%
  filter(noise == "env") %>%
  mutate(n = ceiling((median_perc_exp/100)*totalpop),
         n_2.5 = ceiling((q2.5_perc_exp/100)*totalpop),
         n_97.5 = ceiling((q97.5_perc_exp/100)*totalpop)) %>%
  summarise(pop = sum(totalpop, na.rm = TRUE),
            n = sum(n, na.rm = TRUE),
            n_2.5 = sum(n_2.5, na.rm = TRUE),
            n_97.5 = sum(n_97.5, na.rm = TRUE)) %>%
  mutate(perc = 100*n/pop,
         perc_2.5 = 100*n_2.5/pop,
         perc_97.5 = 100*n_97.5/pop)

# occupational noise - among workers
df_oej_demo %>%
  filter(noise == "occ") %>%
  mutate(n2 = ceiling((median_perc_exp/100)*n),
         n_2.5 = ceiling((q2.5_perc_exp/100)*n),
         n_97.5 = ceiling((q97.5_perc_exp/100)*n)) %>%
  summarise(pop = sum(n, na.rm = TRUE),
            n = sum(n2, na.rm = TRUE),
            n_2.5 = sum(n_2.5, na.rm = TRUE),
            n_97.5 = sum(n_97.5, na.rm = TRUE)) %>%
  mutate(perc = 100*n/pop,
         perc_2.5 = 100*n_2.5/pop,
         perc_97.5 = 100*n_97.5/pop)

# occupational noise - among entire population
df_oej_demo %>%
  filter(noise == "occ") %>%
  mutate(n2 = ceiling((median_perc_exp/100)*n),
         n_2.5 = ceiling((q2.5_perc_exp/100)*n),
         n_97.5 = ceiling((q97.5_perc_exp/100)*n)) %>%
  summarise(pop = sum(totalpop, na.rm = TRUE),
            n = sum(n2, na.rm = TRUE),
            n_2.5 = sum(n_2.5, na.rm = TRUE),
            n_97.5 = sum(n_97.5, na.rm = TRUE)) %>%
  mutate(perc = 100*n/pop,
         perc_2.5 = 100*n_2.5/pop,
         perc_97.5 = 100*n_97.5/pop)

















































n_nhWhite%>%
  #filter(is.infinite(hmda_index)) %>% select(n_minor, n_nhWhite, pop_density_white, pop_density_minor, NUM_WHITE, NUM_NONWHITE)
  #.$hmda_index %>% summary()
  count(INTERVAL2010, hmda_bin) %>%
  na.omit() %>%
  group_by(INTERVAL2010) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(perc = 100*(n/sum))












library(rstanarm)

mod <- stan_glmer(both ~ minor_breaks + unemployed_pct + (1|GEOID_county) + (1|GEOID_state), 
                  data=fig4_df[subsample,], 
                  family = binomial(link = 'logit'),
                  prior_intercept = normal(0,1), 
                  prior = normal(0,3),
                  cores = 5)

mod

summary(mod, 
        pars = c("(Intercept)", "minor_breaks(10,20]", "minor_breaks(90,100]"),
        probs = c(0.025, 0.975),
        digits = 2)

yrep <- posterior_predict(stan_glm1)






















