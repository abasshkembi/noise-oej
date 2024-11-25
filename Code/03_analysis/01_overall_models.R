# Title: Main models
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














# figure 3 - models

oej_tracts_full$minor_pct %>% hist()
oej_tracts_full$minor_pct %>% quantile(probs = c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.975, 0.98, 0.99, 0.995, 1), na.rm = TRUE)
oej_tracts_full$nhWhite_pct %>% hist()
oej_tracts_full$nhBlack_pct %>% hist()
oej_tracts_full$nhBlack_pct %>% quantile(probs = c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.975, 0.98, 0.99, 0.995, 1), na.rm = TRUE)
oej_tracts_full$nhAsian_pct %>% hist()
oej_tracts_full$nhAsian_pct %>% quantile(probs = c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.975, 0.98, 0.99, 0.995, 1), na.rm = TRUE)
oej_tracts_full$nhAIAN_pct %>% hist()
oej_tracts_full$nhAIAN_pct %>% quantile(probs = c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.975, 0.98, 0.99, 0.995, 1), na.rm = TRUE)
oej_tracts_full$nhNHPI_pct %>% hist()
oej_tracts_full$nhNHPI_pct %>% quantile(probs = c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.975, 0.98, 0.99, 0.995, 1), na.rm = TRUE)

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

df_neighbors %>% as_tibble() %>% count(both)

df_neighbors$occ_median_perc_exp %>% quantile(probs = 0.75)
df_neighbors$env_median_perc_exp %>% quantile(probs = 0.75)





# main models to present
# by both

mod_gam_both_full <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ minor_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_both_full)

mod_gam_both_nhWhite <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ nhWhite_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21), 
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_both_nhWhite)

mod_gam_both_nhBlack <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ nhBlack_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21), 
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_both_nhBlack)

mod_gam_both_hispanic <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ hispanic_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21), 
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_both_hispanic)

mod_gam_both_nhAsian <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ nhAsian_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21), 
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_both_nhAsian)

mod_gam_both_nhAIAN <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ nhAIAN_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21), 
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_both_nhAIAN)

mod_gam_both_nhNHPI <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(both ~ nhNHPI_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21), 
            family = binomial(link = "logit"),
            data = .)
summary(mod_gam_both_nhNHPI)





# occupational - prevalence ratios
mod_gam_occ_full <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ minor_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_full)

mod_gam_occ_nhWhite <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ nhWhite_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_nhWhite)

mod_gam_occ_nhBlack <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ nhBlack_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_nhBlack)

mod_gam_occ_hispanic <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ hispanic_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_hispanic)

mod_gam_occ_nhAsian <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ nhAsian_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_nhAsian)

mod_gam_occ_nhAIAN <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ nhAIAN_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_nhAIAN)

mod_gam_occ_nhNHPI <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_occ_exp ~ nhNHPI_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_occ),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_occ_nhNHPI)






# environmental - prevalence ratio
mod_gam_env_full <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ minor_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_full)

mod_gam_env_nhWhite <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ nhWhite_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_nhWhite)

mod_gam_env_nhBlack <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ nhBlack_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_nhBlack)

mod_gam_env_hispanic <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ hispanic_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_hispanic)

mod_gam_env_nhAsian <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ nhAsian_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_nhAsian)

mod_gam_env_nhAIAN <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ nhAIAN_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_nhAIAN)

mod_gam_env_nhNHPI <- df_neighbors %>% as_tibble() %>%
  mgcv::gam(n_env_exp ~ nhNHPI_breaks + rural + unemployed_pct + s(pop_density, fx=TRUE, k=9) + s(lon, lat, fx=TRUE, k=21),
            offset = log(n_env),
            family = poisson(link = "log"),
            data = .)
summary(mod_gam_env_nhNHPI)








fig4_both_ors <- OR("mod_gam_both_full", "full") %>%
  rbind(OR("mod_gam_both_nhWhite", "nhWhite")) %>%
  rbind(OR("mod_gam_both_nhBlack", "nhBlack")) %>%
  rbind(OR("mod_gam_both_hispanic", "hispanic")) %>%
  rbind(OR("mod_gam_both_nhAsian", "nhAsian")) %>%
  rbind(OR("mod_gam_both_nhAIAN", "nhAIAN")) %>%
  rbind(OR("mod_gam_both_nhNHPI", "nhNHPI"))


base_race_df <- tibble(var = c(rep("minor_breaks", 5), 
                               rep("nhWhite_breaks", 5), 
                               rep("nhBlack_breaks", 5), 
                               rep("hispanic_breaks", 5), 
                               rep("nhAsian_breaks", 5), 
                               rep("nhAIAN_breaks", 5), 
                               rep("nhNHPI_breaks", 5)),
                       values = c(levels(df_neighbors$minor_breaks), 
                                  levels(df_neighbors$nhWhite_breaks), 
                                  levels(df_neighbors$nhBlack_breaks), 
                                  levels(df_neighbors$hispanic_breaks), 
                                  levels(df_neighbors$nhAsian_breaks), 
                                  c(levels(df_neighbors$nhAIAN_breaks)[1], levels(df_neighbors$nhAIAN_breaks)), 
                                  c(rep(levels(df_neighbors$nhNHPI_breaks)[1], 4), levels(df_neighbors$nhNHPI_breaks)[2])),
                       predictors = paste0(var, values),
                       quant = paste0("Q", rep(1:5, 7)))

fig4_both_df <- base_race_df %>%
  left_join(
    fig4_both_ors,
    by = "predictors"
  ) %>%
  mutate(OR = ifelse(is.na(OR), 1, OR),
         q2.5 = ifelse(is.na(q2.5), 1, q2.5),
         q97.5 = ifelse(is.na(q97.5), 1, q97.5)) %>%
  mutate(var_clean = case_when(
    var == "minor_breaks" ~ "R/E Minority",
    var == "nhWhite_breaks" ~ "NH White",
    var == "nhBlack_breaks" ~ "NH Black",
    var == "hispanic_breaks" ~ "Hispanic",
    var == "nhAsian_breaks" ~ "NH Asian",
    var == "nhAIAN_breaks" ~ "NH Am. Indian",
    var == "nhNHPI_breaks" ~ "NH Nat. Haw."
  )) %>%
  mutate(var_clean = factor(var_clean, levels = c("R/E Minority", "NH White", "NH Black", "Hispanic", "NH Asian", "NH Am. Indian", "NH Nat. Haw."))) %>%
  mutate(clean_value1 = as.numeric(str_remove(str_extract(values, "\\d+\\.\\d+,"), ","))) %>%
  mutate(clean_value1 = case_when(
    clean_value1 > 0 & clean_value1 < 0.05 ~ ">0",
    clean_value1 >= 0.05 & clean_value1 < 0.1 ~ as.character(round(clean_value1, 1)),
    .default = as.character(round(clean_value1, 0))
  )) %>%
  mutate(clean_value2 = as.numeric(str_remove(str_extract(values, "\\d+\\.\\d+(\\]|\\))"), "\\]|\\)"))) %>%
  mutate(clean_value2 = case_when(
    clean_value2 > 0 & clean_value2 < 0.05 ~ ">0",
    clean_value2 >= 0.05 & clean_value2 < 0.1 ~ as.character(round(clean_value2, 1)),
    .default = as.character(round(clean_value2, 0))
  )) %>%
  mutate(clean_value = paste0(clean_value1, "-", clean_value2, "%")) %>%
  mutate(clean_value = ifelse(is.na(clean_value1), "0-0%", clean_value)) %>%
  mutate(clean_value = paste0(quant, ". ", clean_value))


fig4_both_a <- fig4_both_df %>%
  filter(var == "minor_breaks") %>%
  ggplot(aes(x = clean_value, group = var)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0) +
  geom_line(aes(y = OR), size = 0.5, color = "grey80") +
  geom_point(aes(y = OR), size = 2, show.legend = TRUE, shape = 15) +
  facet_wrap(~var_clean, nrow = 1, scales = "free_x") +
  scale_y_log10(breaks = c(0.1, 0.3, 0.6, 1, 2, 5, 9)) +
  coord_cartesian(ylim = c(0.1, 9)) +
  annotation_logticks(sides = "l") +
  theme_classic() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Quintile", y = "Odds Ratio (OR)")

fig4_both_b <- fig4_both_df %>%
  filter(var != "minor_breaks") %>%
  ggplot(aes(x = clean_value, group = var)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0) +
  geom_line(aes(y = OR), size = 0.5, color = "grey80") +
  geom_point(aes(y = OR), size = 2, show.legend = TRUE, shape = 15) +
  facet_wrap(~var_clean, nrow = 1, scales = "free_x") +
  scale_y_log10(breaks = c(0.1, 0.3, 0.6, 1, 2, 5, 9)) +
  coord_cartesian(ylim = c(0.1, 9)) +
  annotation_logticks(sides = "l") +
  theme_classic() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Quintile", y = "Odds Ratio (OR)")

ggarrange(fig4_both_a, fig4_both_b, nrow = 1, widths = c(0.2, 0.85))





make_fig4_df <- function(dataframe) {
  dataframe %>%
    mutate(OR = ifelse(is.na(OR), 1, OR),
           q2.5 = ifelse(is.na(q2.5), 1, q2.5),
           q97.5 = ifelse(is.na(q97.5), 1, q97.5)) %>%
    mutate(var_clean = case_when(
      var == "minor_breaks" ~ "R/E Minority",
      var == "nhWhite_breaks" ~ "NH White",
      var == "nhBlack_breaks" ~ "NH Black",
      var == "hispanic_breaks" ~ "Hispanic",
      var == "nhAsian_breaks" ~ "NH Asian",
      var == "nhAIAN_breaks" ~ "NH Am. Indian",
      var == "nhNHPI_breaks" ~ "NH Nat. Haw."
    )) %>%
    mutate(var_clean = factor(var_clean, levels = c("R/E Minority", "NH White", "NH Black", "Hispanic", "NH Asian", "NH Am. Indian", "NH Nat. Haw."))) %>%
    mutate(clean_value1 = as.numeric(str_remove(str_extract(values, "\\d+\\.\\d+,"), ","))) %>%
    mutate(clean_value1 = case_when(
      clean_value1 > 0 & clean_value1 < 0.05 ~ ">0",
      clean_value1 >= 0.05 & clean_value1 < 0.1 ~ as.character(round(clean_value1, 1)),
      .default = as.character(round(clean_value1, 0))
    )) %>%
    mutate(clean_value2 = as.numeric(str_remove(str_extract(values, "\\d+\\.\\d+(\\]|\\))"), "\\]|\\)"))) %>%
    mutate(clean_value2 = case_when(
      clean_value2 > 0 & clean_value2 < 0.05 ~ ">0",
      clean_value2 >= 0.05 & clean_value2 < 0.1 ~ as.character(round(clean_value2, 1)),
      .default = as.character(round(clean_value2, 0))
    )) %>%
    mutate(clean_value = paste0(clean_value1, "-", clean_value2, "%")) %>%
    mutate(clean_value = ifelse(is.na(clean_value1), "0-0%", clean_value)) %>%
    mutate(clean_value = paste0(quant, ". ", clean_value))
}



fig4_occ_ors <- OR("mod_gam_occ_full", "full") %>%
  rbind(OR("mod_gam_occ_nhWhite", "nhWhite")) %>%
  rbind(OR("mod_gam_occ_nhBlack", "nhBlack")) %>%
  rbind(OR("mod_gam_occ_hispanic", "hispanic")) %>%
  rbind(OR("mod_gam_occ_nhAsian", "nhAsian")) %>%
  rbind(OR("mod_gam_occ_nhAIAN", "nhAIAN")) %>%
  rbind(OR("mod_gam_occ_nhNHPI", "nhNHPI"))



fig4_occ_df <- base_race_df %>%
  left_join(
    fig4_occ_ors,
    by = "predictors"
  ) 

make_fig4_df(fig4_occ_df)


fig4_occ_a <- make_fig4_df(fig4_occ_df) %>%
  filter(var == "minor_breaks") %>%
  ggplot(aes(x = clean_value, group = var)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0) +
  geom_line(aes(y = OR), size = 0.5, color = "grey80") +
  geom_point(aes(y = OR), size = 2, show.legend = TRUE, shape = 15) +
  facet_wrap(~var_clean, nrow = 1, scales = "free_x") +
  scale_y_continuous(limits = c(0.75, 1.3)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Quintile", y = "Prevalence Ratio (PR)")

fig4_occ_b <- make_fig4_df(fig4_occ_df) %>%
  filter(var != "minor_breaks") %>%
  ggplot(aes(x = clean_value, group = var)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0) +
  geom_line(aes(y = OR), size = 0.5, color = "grey80") +
  geom_point(aes(y = OR), size = 2, show.legend = TRUE, shape = 15) +
  facet_wrap(~var_clean, nrow = 1, scales = "free_x") +
  scale_y_continuous(limits = c(0.75, 1.3)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Quintile", y = "Prevalence Ratio (PR)")

ggarrange(fig4_occ_a, fig4_occ_b, nrow = 1, widths = c(0.2, 0.85))



# environmental data
fig4_env_ors <- OR("mod_gam_env_full", "full") %>%
  rbind(OR("mod_gam_env_nhWhite", "nhWhite")) %>%
  rbind(OR("mod_gam_env_nhBlack", "nhBlack")) %>%
  rbind(OR("mod_gam_env_hispanic", "hispanic")) %>%
  rbind(OR("mod_gam_env_nhAsian", "nhAsian")) %>%
  rbind(OR("mod_gam_env_nhAIAN", "nhAIAN")) %>%
  rbind(OR("mod_gam_env_nhNHPI", "nhNHPI"))



fig4_env_df <- base_race_df %>%
  left_join(
    fig4_env_ors,
    by = "predictors"
  ) 

make_fig4_df(fig4_env_df)


fig4_env_a <- make_fig4_df(fig4_env_df) %>%
  filter(var == "minor_breaks") %>%
  ggplot(aes(x = clean_value, group = var)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0) +
  geom_line(aes(y = OR), size = 0.5, color = "grey80") +
  geom_point(aes(y = OR), size = 2, show.legend = TRUE, shape = 15) +
  facet_wrap(~var_clean, nrow = 1, scales = "free_x") +
  scale_y_continuous(limits = c(0.75, 1.3)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Quintile", y = "Prevalence Ratio (PR)")

fig4_env_b <- make_fig4_df(fig4_env_df) %>%
  filter(var != "minor_breaks") %>%
  ggplot(aes(x = clean_value, group = var)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0) +
  geom_line(aes(y = OR), size = 0.5, color = "grey80") +
  geom_point(aes(y = OR), size = 2, show.legend = TRUE, shape = 15) +
  facet_wrap(~var_clean, nrow = 1, scales = "free_x") +
  scale_y_continuous(limits = c(0.75, 1.3)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Quintile", y = "Prevalence Ratio (PR)")

ggarrange(fig4_env_a, fig4_env_b, nrow = 1, widths = c(0.2, 0.85))






# final figures

ggarrange(fig4_both_a, fig4_both_b, nrow = 1, widths = c(0.2, 0.85))
ggarrange(fig4_occ_a, fig4_occ_b, nrow = 1, widths = c(0.2, 0.85))
ggarrange(fig4_env_a, fig4_env_b, nrow = 1, widths = c(0.2, 0.85))

# export 717w x 290h
# crop and remove AIAN and NHPI later in powerpoint
