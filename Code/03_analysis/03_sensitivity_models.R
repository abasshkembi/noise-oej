# Title: Sensitivity analysis model for overall model
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



df_neighbors <- oej_tracts_full %>%
  mutate(minor_breaks = Hmisc::cut2(minor_pct, g = 5)) %>%
  mutate(lowinc_breaks = Hmisc::cut2(lowinc_pct, g = 5)) %>%
  mutate(noHS_breaks = Hmisc::cut2(noHS_pct, g = 5)) %>%
  mutate(nhWhite_breaks = Hmisc::cut2(nhWhite_pct, g = 5)) %>%
  mutate(nhBlack_breaks = Hmisc::cut2(nhBlack_pct, g = 10)) %>%
  mutate(hispanic_breaks = Hmisc::cut2(hispanic_pct, g = 5)) %>%
  mutate(nhAsian_breaks = Hmisc::cut2(nhAsian_pct, g = 5)) %>%
  mutate(nhNHPI_breaks = Hmisc::cut2(nhNHPI_pct, g = 5)) %>%
  mutate(nhAIAN_breaks = Hmisc::cut2(nhAIAN_pct, g = 5)) %>%
  mutate(intersect_minor_lowinc = interaction(minor_breaks, lowinc_breaks)) %>%
  #mutate(minor_breaks = cut(minor_pct, breaks = seq(0, 100, by = 10))) %>%
  mutate(GEOID_county = str_extract(GEOID, "^\\d{5}"),
         GEOID_state = str_extract(GEOID, "^\\d{2}")) %>%
  mutate(percentile_occ = percent_rank(occ_median_perc_exp),
         percentile_env = percent_rank(env_median_perc_exp)) %>%
  mutate(both = ifelse(percentile_occ > 0.75 & percentile_env > 0.75, 1, 0)) %>%
  mutate(n_occ_exp = ceiling((occ_median_perc_exp/100)*n_occ)) %>%
  mutate(n_env_exp = ceiling((env_median_perc_exp/100)*n_env)) %>%
  na.omit(both, minor_breaks, unemployed_pct, pop_density, lon, lat)




### sensitivity analysis

mod_gam_both_full_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ minor_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_both_full_sens)

mod_gam_both_nhWhite_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ nhWhite_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21), 
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_both_nhWhite)

mod_gam_both_nhBlack_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ nhBlack_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21), 
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_both_nhBlack)

mod_gam_both_hispanic_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ hispanic_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21), 
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_both_hispanic)

mod_gam_both_nhAsian_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ nhAsian_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21), 
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_nhAsian)

mod_gam_both_nhAIAN_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ nhAIAN_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21), 
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_nhAIAN)

mod_gam_both_nhNHPI_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ nhNHPI_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21), 
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_nhNHPI)





# occupational - prevalence ratios
mod_gam_occ_full_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ minor_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_full)

mod_gam_occ_nhWhite_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ nhWhite_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_nhWhite)

mod_gam_occ_nhBlack_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ nhBlack_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_nhBlack)

mod_gam_occ_hispanic_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ hispanic_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_hispanic)

mod_gam_occ_nhAsian_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ nhAsian_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_nhAsian)

mod_gam_occ_nhAIAN_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ nhAIAN_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_nhAIAN)

mod_gam_occ_nhNHPI_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ nhNHPI_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_nhNHPI)







mod_gam_env_full_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ minor_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_full)

mod_gam_env_nhWhite_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ nhWhite_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_nhWhite)

mod_gam_env_nhBlack_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ nhBlack_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_nhBlack)

mod_gam_env_hispanic_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ hispanic_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_hispanic)

mod_gam_env_nhAsian_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ nhAsian_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_nhAsian)

mod_gam_env_nhAIAN_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ nhAIAN_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_nhAIAN_sens)

mod_gam_env_nhNHPI_sens <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ nhNHPI_breaks + lowinc_pct + noHS_pct + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_nhNHPI_sens)


# both data
fig4_sens_both_ors <- OR("mod_gam_both_full_sens", "full", round = 2) %>%
  rbind(OR("mod_gam_both_nhWhite_sens", "nhWhite", round = 2)) %>%
  rbind(OR("mod_gam_both_nhBlack_sens", "nhBlack", round = 2)) %>%
  rbind(OR("mod_gam_both_hispanic_sens", "hispanic", round = 2)) %>%
  rbind(OR("mod_gam_both_nhAsian_sens", "nhAsian", round = 2)) %>%
  rbind(OR("mod_gam_both_nhAIAN_sens", "nhAIAN", round = 2)) %>%
  rbind(OR("mod_gam_both_nhNHPI_sens", "nhNHPI", round = 2))

summary(mod_gam_both_full_sens)

fig4_both_ors
fig4_sens_both_ors

# occupational data
fig4_sens_occ_ors <- OR("mod_gam_occ_full_sens", "full", round = 2) %>%
  rbind(OR("mod_gam_occ_nhWhite_sens", "nhWhite", round = 2)) %>%
  rbind(OR("mod_gam_occ_nhBlack_sens", "nhBlack", round = 2)) %>%
  rbind(OR("mod_gam_occ_hispanic_sens", "hispanic", round = 2)) %>%
  rbind(OR("mod_gam_occ_nhAsian_sens", "nhAsian", round = 2)) %>%
  rbind(OR("mod_gam_occ_nhAIAN_sens", "nhAIAN", round = 2)) %>%
  rbind(OR("mod_gam_occ_nhNHPI_sens", "nhNHPI", round = 2))

fig4_occ_ors
fig4_sens_occ_ors

# environmental data
fig4_sens_env_ors <- OR("mod_gam_env_full_sens", "full", round = 2) %>%
  rbind(OR("mod_gam_env_nhWhite_sens", "nhWhite", round = 2)) %>%
  rbind(OR("mod_gam_env_nhBlack_sens", "nhBlack", round = 2)) %>%
  rbind(OR("mod_gam_env_hispanic_sens", "hispanic", round = 2)) %>%
  rbind(OR("mod_gam_env_nhAsian_sens", "nhAsian", round = 2)) %>%
  rbind(OR("mod_gam_env_nhAIAN_sens", "nhAIAN", round = 2)) %>%
  rbind(OR("mod_gam_env_nhNHPI_sens", "nhNHPI", round = 2))

fig4_env_ors
fig4_sens_env_ors



make_suppfig4 <- function(dataframe) {
  dataframe %>%
    ggplot(aes(x = clean_value, group = `Model Type`)) +
    geom_hline(yintercept = 1, linetype = "dotted") +
    geom_line(aes(y = OR, color = `Model Type`, alpha = `Model Type`, linetype = `Model Type`)) +
    geom_errorbar(aes(ymin = q2.5, ymax = q97.5, color = `Model Type`, alpha = `Model Type`), width = 0) +
    geom_point(aes(y = OR, color = `Model Type`, alpha = `Model Type`)) +
    facet_wrap(~var_clean, nrow = 1, scales = "free_x") +
    scale_color_manual(values = c("Main" = "grey50", "Sensitivity" = "black")) +
    scale_alpha_manual(values = c("Main" = 0.5, "Sensitivity" = 1)) +
    scale_linetype_manual(values = c("Main" = "dashed", "Sensitivity" = "solid")) +
    theme_classic() +
    theme(strip.background = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "bottom",
          legend.direction = "horizontal") +
    labs(x = "Quintile", y = "Odds Ratio (OR)")
}


suppFig_model_sensitivity1 <- make_suppfig4(dataframe = base_race_df %>% left_join(fig4_both_ors, by = "predictors") %>% make_fig4_df() %>% mutate(`Model Type` = "Main") %>%
                                              rbind(base_race_df %>% left_join(fig4_sens_both_ors, by = "predictors") %>% make_fig4_df() %>% mutate(`Model Type` = "Sensitivity"))) +
  scale_y_log10(breaks = c(0.1, 0.3, 0.6, 1, 2, 5, 9)) +
  coord_cartesian(ylim = c(0.1, 9)) +
  annotation_logticks(sides = "l")

suppFig_model_sensitivity2 <- make_suppfig4(dataframe = base_race_df %>% left_join(fig4_occ_ors, by = "predictors") %>% make_fig4_df() %>% mutate(`Model Type` = "Main") %>%
                                              rbind(base_race_df %>% left_join(fig4_sens_occ_ors, by = "predictors") %>% make_fig4_df() %>% mutate(`Model Type` = "Sensitivity"))) +
  coord_cartesian(ylim = c(0.8, 1.3))

suppFig_model_sensitivity3 <- make_suppfig4(dataframe = base_race_df %>% left_join(fig4_env_ors, by = "predictors") %>% make_fig4_df() %>% mutate(`Model Type` = "Main") %>%
                                              rbind(base_race_df %>% left_join(fig4_sens_env_ors, by = "predictors") %>% make_fig4_df() %>% mutate(`Model Type` = "Sensitivity"))) +
  coord_cartesian(ylim = c(0.8, 1.3))

ggarrange(
  suppFig_model_sensitivity1 + labs(title = "Odds of Cumulatively High Workplace and Transportation Noise Pollution by Race/Ethnicity", y = "Odds Ratio") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank()),
  suppFig_model_sensitivity2 + labs(title = "Prevalence Ratio of Workplace Noise Pollution by Race/Ethnicity", y = "Prevalence Ratio") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank()),
  suppFig_model_sensitivity3 + labs(title = "Prevalence Ratio of Transportation Noise Pollution by Race/Ethnicity", y = "Prevalence Ratio"),
  nrow = 3,
  common.legend = TRUE,
  heights = c(0.8, 0.8, 1.2),
  labels = c("a", "b", "c"),
  legend = "top"
)