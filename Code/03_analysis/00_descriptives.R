# Title: Data descriptives
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















# 1. distribution and plots

oej_tracts_semi$occ_median_perc_exp %>% 
  quantile(probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1), na.rm = T)
#  0%  10%  25%  50%  75%  90%  95% 100% 
# 0.0 11.1 12.6 14.6 16.8 18.9 20.4 60.0 

oej_tracts_semi$env_median_perc_exp %>% 
  quantile(probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1), na.rm = T)
#     0%     10%     25%     50%     75%     90%     95%    100% 
#  0.000   2.100   5.700  12.000  19.900  27.800  33.335 100.000 




### supplemental figure showing spread of occ and env values
oej_tracts_semi %>%
  as_tibble() %>%
  ggplot(aes(x = occ_median_perc_exp, y = env_median_perc_exp)) +
  geom_point(alpha = 0.1, color = "grey") +
  geom_point(data = oej_tracts_semi %>%
               as_tibble() %>%
               filter(occ_median_perc_exp >= 16.8 & env_median_perc_exp >= 19.900),
             inherit.aes = F,
             aes(x = occ_median_perc_exp, y = env_median_perc_exp, color = rural),
             alpha = 0.3) +
  geom_hline(yintercept = 14.3, linetype = "dashed") +
  geom_vline(xintercept = 14.5, linetype = "dashed") +
  geom_hline(yintercept = 19.900, linetype = "dotted") +
  geom_vline(xintercept = 16.8, linetype = "dotted") +
  annotate("text", x = 75, y = 12.0, label = "Median (dashed)", size = 3, hjust = 0) +
  annotate("text", x = 75, y = 23.0, label = "75th percentile (dotted)", size = 3, hjust = 0) +
  theme_classic() +
  theme(legend.position = "inside") +
  scale_color_manual(values = met.brewer("Hokusai1", n = 2),
                     name = "Urbanicity",
                     labels = c("Rural", "Urban")) +
  labs(x = "Workplace noise (% of workers)", y = "Transportation noise (% of residents)") +
  coord_cartesian(xlim = c(0, 100))












# supplemental figures of occ and env noise by tract

# get quantiles for cutting
v_quantiles <- c(0, 0.05, 0.25, 0.75, 0.95, 1)
plot_ntiles <- function(x) {
  vector_x <- oej_tracts_full[[x]]
  
  v_ntiles <- c()
  v_labels <- c()
  for(i in 1:length(v_quantiles)) {
    if(i == 6) {
      v_ntiles[i] <- quantile(vector_x, v_quantiles[i], na.rm = T) %>% as.numeric()  %>% ceiling
    } else if(i == 1) {
      v_ntiles[i] <- quantile(vector_x, v_quantiles[i], na.rm = T) %>% as.numeric()  %>% floor
    } else {
      v_ntiles[i] <- quantile(vector_x, v_quantiles[i], na.rm = T) %>% as.numeric() %>% round(0)
    }
    
  }
  
  j <- 0
  while(j < 6) {
    v_labels[j] <- paste0(v_ntiles[j], " - ", v_ntiles[j+1])
    j <- j + 1
  }
  
  return(
    list(v_ntiles, v_labels)
  )
}

cut_ntiles <- function(x) {
  cut(oej_tracts_full[[x]],
      breaks = plot_ntiles(x)[[1]],
      labels = plot_ntiles(x)[[2]], 
      include.lowest = T)
}

plot_ntiles("env_median_perc_exp")
cut_ntiles("env_median_perc_exp")





# plot the data

# occ noise
oej_tracts_full$breaks <- cut_ntiles("occ_median_perc_exp")

suppfig_occ_noise <- oej_tracts_full %>%
  filter(!(str_detect(GEOID, "^72"))) %>%
  filter(str_detect(GEOID, "^26")) %>%
  ggplot() +
  geom_sf(aes(fill = breaks, color = breaks), size = 0.001) +
  scale_fill_manual(values = met.brewer("Tam", 5),
                    name = "Workplace noise (% of workers)", 
                    labels = function(breaks) {breaks[is.na(breaks)] <- "Unknown"; breaks},
                    na.value = "#f0f0f0") +
  scale_color_manual(values = met.brewer("Tam", 5),
                     name = "Workplace noise (% of workers)", 
                     labels = function(breaks) {breaks[is.na(breaks)] <- "Unknown"; breaks},
                     na.value = "#f0f0f0") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.just = "left",
        legend.key.width = unit(2, 'cm'),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(colour = "black", size = 10)
  ) +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom", title.vjust = 0.8))

suppfig_occ_noise




# env noise
oej_tracts_full$breaks_env <- cut_ntiles("env_median_perc_exp")

suppfig_env_noise <- oej_tracts_full %>%
  filter(!(str_detect(GEOID, "^72"))) %>%
  #filter(str_detect(GEOID, "^26")) %>%
  #mutate(breaks_env = ifelse(env_median_perc_exp == 0, "0 - 1", breaks_env)) %>%
  #filter(is.na(breaks_env)) %>%
  ggplot() +
  geom_sf(aes(fill = breaks_env, color = breaks_env), size = 0.001) +
  scale_fill_manual(values = met.brewer("Tam", 5),
                    name = "Transportation noise (% of residents)", 
                    labels = function(breaks) {breaks[is.na(breaks)] <- "Unknown"; breaks},
                    na.value = "#f0f0f0"
  ) +
  scale_color_manual(values = met.brewer("Tam", 5),
                     name = "Transportation noise (% of residents)", 
                     labels = function(breaks) {breaks[is.na(breaks)] <- "Unknown"; breaks},
                     na.value = "#f0f0f0"
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.just = "left",
        legend.key.width = unit(2, 'cm'),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(colour = "black", size = 10)
  ) +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom", title.vjust = 0.8))

suppfig_env_noise





















# make figure 2 (summary map)

county_oej <- final_noise_df %>%
  transmute(GEOID = str_extract(GEOID, "^\\d{5}"),
            n_env = n_env,
            n_occ = n_occ,
            n_exp_env = ceiling((env_median_perc_exp/100)*n_env),
            n_exp_occ = ceiling((occ_median_perc_exp/100)*n_occ)
  ) %>%
  group_by(GEOID) %>%
  summarise(n_env_county = sum(n_env, na.rm = TRUE),
            n_occ_county = sum(n_occ, na.rm = TRUE),
            n_exp_env_county = sum(n_exp_env, na.rm = TRUE),
            n_exp_occ_county = sum(n_exp_occ, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(env_median_perc_exp = round(n_exp_env_county/n_env_county*100, 1),
         occ_median_perc_exp = round(n_exp_occ_county/n_occ_county*100, 1))



us_counties <- NULL
for(i in 1:length(unique_state_fips)) {
  state_tracts <- counties(state = unique_state_fips[i], cb = T, year = 2017)
  
  us_counties <- rbind(us_counties, state_tracts)
}

oej_tracts_full$occ_median_perc_exp %>% hist()
oej_tracts_full$env_median_perc_exp %>% hist()


oej_counties_full <- us_counties %>% 
  left_join(county_oej, by = "GEOID")

oej_counties_full <- oej_counties_full %>%
  shift_geometry(geoid_column = "GEOID") %>%
  st_transform("EPSG:3082")

states_shp <- states(cb = TRUE, year = 2017) %>%
  filter(STATEFP %in% unique_state_fips) %>%
  shift_geometry(geoid_column = "GEOID") %>%
  st_transform("EPSG:3082")


# load dependencies
library(biscale)

# create classes
data <- bi_class(oej_counties_full, x = occ_median_perc_exp, y = env_median_perc_exp, style = "quantile", dim = 2)

break_vals <- bi_class_breaks(oej_counties_full, style = "quantile",
                              x = occ_median_perc_exp, y = env_median_perc_exp, dim = 2, dig_lab = c(x = 3, y = 3),
                              split = TRUE)

map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class, color = bi_class), linewidth = 0.1, show.legend = FALSE) +
  #geom_sf(data = states_shp, color = "white", linewidth = 1, fill = NA) +
  bi_scale_fill(pal = "DkViolet2", dim = 2) +
  bi_scale_color(pal = "DkViolet2", dim = 2) +
  theme_void()

map

legend <- biscale::bi_legend(pal = "DkViolet2",
                             dim = 2,
                             xlab = "% Occ",
                             ylab = "% Env",
                             size = 8,
                             breaks = break_vals)
legend

library(cowplot)
finalFig1_A <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1)# +
#draw_plot(legend, 0.55, 0.05, 0.25, 0.25)

finalFig1_A

ca_places <- places(state = "CA", cb = T, year = 2017)
tx_places <- places(state = "TX", cb = T, year = 2017)
ny_places <- places(state = "NY", cb = T, year = 2017)

la_shp <- ca_places %>% filter(NAME == "Los Angeles") #%>% st_transform("EPSG:3082")
ht_shp <- tx_places %>% filter(NAME == "Houston")# %>% st_transform("EPSG:3082")
nyc_shp <- ny_places %>% filter(NAME == "New York")# %>% st_transform("EPSG:3082")


fig1_ca <- oej_tracts_semi %>% filter(STATEFP == "06")
fig1_ca <- st_intersection(la_shp, fig1_ca)

fig1_ht <- oej_tracts_semi %>% filter(STATEFP == "48")
fig1_ht <- st_intersection(ht_shp, fig1_ht)

fig1_nyc <- oej_tracts_semi %>% filter(STATEFP == "36")
fig1_nyc <- st_intersection(nyc_shp, fig1_nyc)


library(ggmap)
ggmap::register_stadiamaps("b053b5be-46d3-40e7-8e16-2cff4f826149", write = TRUE)


get_bbox <- function(dataframe) {
  fig0_bbox <- unname(st_bbox(dataframe))
  fig0_bbox2 <- c(bottom = fig0_bbox[2], top = fig0_bbox[4], right = fig0_bbox[3], left = fig0_bbox[1])
  return(fig0_bbox2)
}

la_ggmap <- get_stadiamap(maptype = "stamen_toner_lite", bbox = get_bbox(fig1_ca) + c(0, 0, 0.1, 0), zoom = 10)
ht_ggmap <- get_stadiamap(maptype = "stamen_toner_lite", bbox = get_bbox(fig1_ht) + c(-0.05, -0.05, -0.1, 0.2), zoom = 10)
nyc_ggmap <- get_stadiamap(maptype = "stamen_toner_lite", bbox = get_bbox(fig1_nyc) + c(0.08, 0, 0, 0.2), zoom = 11)

ggmap::ggmap(ht_ggmap)


fig1_dot_data <- tract_data %>% 
  mutate(GEOID_county = str_extract(GEOID, "^\\d{5}")) %>%
  filter(GEOID_county %in% c("06037", "36005", "36047", "36061", "36081", "48201")) %>%
  mutate(Minority = ceiling((minor_pct/100)*totalpop)) %>%
  mutate(White = totalpop-Minority) %>%
  select(GEOID, Minority, White) %>%
  gather(key = 'race', value = "count", -c(GEOID, geometry)) %>%
  as_dot_density(
    value = "count",
    values_per_dot = 100,
    group = "race"
  )

tract_data <- bi_class(oej_tracts_semi, x = occ_median_perc_exp, y = env_median_perc_exp, style = "quantile", dim = 2)

finalFig1_B1 <- ggmap::ggmap(la_ggmap) +
  geom_sf(data = tract_data %>% filter(STATEFP == "06") %>% mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class)), 
          mapping = aes(fill = bi_class, color = bi_class), 
          linewidth = 0, alpha = 0.7, 
          show.legend = FALSE, inherit.aes = FALSE) +
  bi_scale_fill(pal = "DkViolet2", dim = 2, na.value = "lightgrey") +
  bi_scale_color(pal = "DkViolet2", dim = 2, na.value = "grey50") +
  theme_void()

finalFig1_B2 <- ggmap::ggmap(la_ggmap) +
  geom_sf(data = fig1_dot_data, 
          mapping = aes(color = race), 
          size = 0.02, inherit.aes = FALSE, show.legend = F) +
  scale_color_manual(values = c("White" = "#D66929", "Minority" = "#4C357F")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

ggplot() +
  geom_sf(data = fig1_ca %>% filter(STATEFP == "06") %>% mutate(race = ifelse(nhWhite_pct > 50, "1", "0")), 
          mapping = aes(color = race, fill = race), 
          linewidth = 0.05, alpha = 0.85, inherit.aes = FALSE, show.legend = F) +
  scale_color_manual(values = c("1" = "grey", "0" = "grey")) +
  scale_fill_manual(values = c("1" = "#D66929", "0" = "#4C357F")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        panel.background = element_rect(fill = "lightgrey", color = "lightgrey")) +
  coord_sf(xlim = c(-118.66816, -118.05529), ylim = c(33.70403, 34.33716))



# houston

finalFig1_C1 <- ggmap::ggmap(ht_ggmap) +
  geom_sf(data = tract_data %>% filter(STATEFP == "48") %>% mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class)), 
          mapping = aes(fill = bi_class, color = bi_class), 
          linewidth = 0, alpha = 0.7, 
          show.legend = FALSE, inherit.aes = FALSE) +
  bi_scale_fill(pal = "DkViolet2", dim = 2, na.value = "lightgrey") +
  bi_scale_color(pal = "DkViolet2", dim = 2, na.value = "grey50") +
  theme_void()

finalFig1_C2 <- ggmap::ggmap(ht_ggmap) +
  geom_sf(data = fig1_dot_data, 
          mapping = aes(color = race), 
          size = 0.02, inherit.aes = FALSE, show.legend = F) +
  scale_color_manual(values = c("White" = "#D66929", "Minority" = "#4C357F")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")


ggplot() +
  geom_sf(data = oej_tracts_semi %>% filter(STATEFP == "48") %>% mutate(race = ifelse(nhWhite_pct > 50, "1", "0")), 
          mapping = aes(color = race, fill = race), 
          linewidth = 0.05, alpha = 0.85, inherit.aes = FALSE, show.legend = F) +
  scale_color_manual(values = c("1" = "grey", "0" = "grey")) +
  scale_fill_manual(values = c("1" = "#D66929", "0" = "#4C357F")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        panel.background = element_rect(fill = "lightgrey", color = "lightgrey")) +
  coord_sf(xlim = c(-95.70974, -95.11666), ylim = c(29.51628, 30.15591))



# NYC

finalFig1_D1 <- ggmap::ggmap(nyc_ggmap) +
  geom_sf(data = tract_data %>% filter(STATEFP == "36") %>% mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class)), 
          mapping = aes(fill = bi_class, color = bi_class), 
          linewidth = 0, alpha = 0.7, 
          show.legend = FALSE, inherit.aes = FALSE) +
  bi_scale_fill(pal = "DkViolet2", dim = 2, na.value = "lightgrey") +
  bi_scale_color(pal = "DkViolet2", dim = 2, na.value = "grey50") +
  theme_void()

finalFig1_D2 <- ggmap::ggmap(nyc_ggmap) +
  geom_sf(data = fig1_dot_data, 
          mapping = aes(color = race), 
          size = 0.02, inherit.aes = FALSE, show.legend = F) +
  #scale_color_brewer(type = "qual", palette = "Set1") +
  scale_color_manual(values = c("White" = "#D66929", "Minority" = "#4C357F")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

ggplot() +
  geom_sf(data = oej_tracts_semi %>% filter(STATEFP %in% c("36", "34")) %>% mutate(race = ifelse(nhWhite_pct > 50, "1", "0")), 
          mapping = aes(color = race, fill = race), 
          linewidth = 0.05, alpha = 0.85, inherit.aes = FALSE, show.legend = F) +
  scale_color_manual(values = c("1" = "grey", "0" = "grey")) +
  scale_fill_manual(values = c("1" = "#D66929", "0" = "#4C357F")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        panel.background = element_rect(fill = "lightgrey", color = "lightgrey")) +
  coord_sf(xlim = c(-74.05609, -73.70002), ylim = c(40.57610, 40.91758))



ggmap::ggmap(nyc_ggmap) +
  geom_sf(data = tract_data %>% filter(STATEFP %in% c("36", "34")) %>% mutate(env = cut(env_median_perc_exp, breaks = c(0, 25, 50, 75, 100))), 
          mapping = aes(fill = env, color = env), 
          linewidth = 0, alpha = 0.9, inherit.aes = FALSE) +
  scale_fill_brewer(palette = 1) +
  scale_color_brewer(palette = 1) +
  theme_void()




library(ggpubr)

finalFig1_B <- ggarrange(
  finalFig1_B1, finalFig1_C1, finalFig1_D1,
  nrow = 1, ncol = 3
)

finalFig1_B
# export 921w x 325h

finalFig1_C <- ggarrange(
  finalFig1_B2, finalFig1_C2, finalFig1_D2,
  nrow = 1, ncol = 3
)

finalFig1_C
# export 921w x 325h

finalFig1_A
# export 921w x 520h

### the final plot is produced in powerpoint under /Manuscript/Figures/figures_ppt.pptx












# get estimated exposure risk ratios

fig3_df <- final_noise_df %>%
  select(GEOID, totalpop, minor_pct, lowinc_pct, noHS_pct, nhBlack_pct, nhAIAN_pct, nhAsian_pct, hispanic_pct, nhNHPI_pct, nhWhite_pct, rural, occ_median_perc_exp, env_median_perc_exp) %>%
  mutate(both_median_perc_exp = occ_median_perc_exp*env_median_perc_exp)

# set up bootstrap for loop to estimate ERRs
est_RR <- function(var, type, dataframe, iterations) {
  temp_colnames <- colnames(dataframe)
  noise_column <- temp_colnames[which(str_detect(temp_colnames, type))]
  selected_colnames <- which(temp_colnames %in% c("GEOID", "totalpop", var, noise_column))
  temp_df <- dataframe[selected_colnames] %>% rename(subpop = 3) %>% rename(median_perc_exp = 4)
  
  if(var == "rural") { # note that this variable is looking for urban areas
    temp_df2 <- temp_df %>%
      mutate(n_subpop = ifelse(subpop == "urban", totalpop, 0)) %>%
      mutate(conc1 = median_perc_exp*n_subpop,
             conc2 = median_perc_exp*totalpop)
  } else {
    temp_df2 <- temp_df %>%
      mutate(n_subpop = ceiling((subpop/100)*totalpop)) %>%
      mutate(conc1 = median_perc_exp*n_subpop,
             conc2 = median_perc_exp*totalpop)
  }
  
  final_df <- NULL
  for(i in 1:iterations) {
    set.seed(8530 + i); B <- sample(1:nrow(temp_df2), nrow(temp_df2), replace = TRUE)
    dfB <- temp_df2[B,]
    
    temp_RR <- dfB %>%
      summarise(sum1 = sum(conc1, na.rm = TRUE),
                sum2 = sum(n_subpop, na.rm = TRUE),
                sum3 = sum(conc2, na.rm = TRUE),
                sum4 = sum(totalpop, na.rm = TRUE)) %>%
      transmute(demo = var, noise = type, RR = (sum1*sum4)/(sum2*sum3))
    
    final_df <- rbind(final_df, temp_RR)
  }
  return(final_df)
}

est_RR("rural", "env", fig3_df, 1)

fig3_RR_a <- est_RR("minor_pct", "env", fig3_df, 100) %>%
  rbind(est_RR("minor_pct", "occ", fig3_df, 100)) %>%
  rbind(est_RR("minor_pct", "both", fig3_df, 100)) %>%
  rbind(est_RR("lowinc_pct", "env", fig3_df, 100)) %>%
  rbind(est_RR("lowinc_pct", "occ", fig3_df, 100)) %>%
  rbind(est_RR("lowinc_pct", "both", fig3_df, 100)) %>%
  rbind(est_RR("noHS_pct", "env", fig3_df, 100)) %>%
  rbind(est_RR("noHS_pct", "occ", fig3_df, 100)) %>%
  rbind(est_RR("noHS_pct", "both", fig3_df, 100)) %>%
  rbind(est_RR("rural", "env", fig3_df, 100)) %>%
  rbind(est_RR("rural", "occ", fig3_df, 100)) %>%
  rbind(est_RR("rural", "both", fig3_df, 100))


df_fig3_RR_a <- fig3_RR_a %>% 
  group_by(demo, noise) %>% 
  summarise(RR50 = round(quantile(RR, probs = 0.5), 3),
            q2.5 = round(quantile(RR, probs = 0.025), 3),
            q97.5 = round(quantile(RR, probs = 0.975), 3)
  ) %>%
  ungroup() %>%
  group_by(demo) %>%
  mutate(min = min(c_across(RR50:q97.5)),
         max = max(c_across(RR50:q97.5))) %>%
  ungroup() %>%
  mutate(noise = factor(noise, levels = c("occ", "env", "both"), labels = c("Workplace", "Transportation", "Both")),
         demo = factor(demo, levels = c("minor_pct", "lowinc_pct", "noHS_pct", "rural"),
                       labels = c("R/E Minority", "Low Income", "No HS Diploma", "Urban Areas")))


fig3_RR_b <- est_RR("nhWhite_pct", "env", fig3_df, 100) %>%
  rbind(est_RR("nhWhite_pct", "occ", fig3_df, 100)) %>%
  rbind(est_RR("nhWhite_pct", "both", fig3_df, 100)) %>%
  
  rbind(est_RR("nhBlack_pct", "env", fig3_df, 100)) %>%
  rbind(est_RR("nhBlack_pct", "occ", fig3_df, 100)) %>%
  rbind(est_RR("nhBlack_pct", "both", fig3_df, 100)) %>%
  
  rbind(est_RR("nhAIAN_pct", "env", fig3_df, 100)) %>%
  rbind(est_RR("nhAIAN_pct", "occ", fig3_df, 100)) %>%
  rbind(est_RR("nhAIAN_pct", "both", fig3_df, 100)) %>%
  
  rbind(est_RR("nhAsian_pct", "env", fig3_df, 100)) %>%
  rbind(est_RR("nhAsian_pct", "occ", fig3_df, 100)) %>%
  rbind(est_RR("nhAsian_pct", "both", fig3_df, 100)) %>%
  
  rbind(est_RR("nhNHPI_pct", "env", fig3_df, 100)) %>%
  rbind(est_RR("nhNHPI_pct", "occ", fig3_df, 100)) %>%
  rbind(est_RR("nhNHPI_pct", "both", fig3_df, 100)) %>%
  
  rbind(est_RR("hispanic_pct", "env", fig3_df, 100)) %>%
  rbind(est_RR("hispanic_pct", "occ", fig3_df, 100)) %>%
  rbind(est_RR("hispanic_pct", "both", fig3_df, 100))


df_fig3_RR_b <- fig3_RR_b %>% 
  group_by(demo, noise) %>% 
  summarise(RR50 = round(quantile(RR, probs = 0.5), 3),
            q2.5 = round(quantile(RR, probs = 0.025), 3),
            q97.5 = round(quantile(RR, probs = 0.975), 3)
  ) %>%
  ungroup() %>%
  group_by(demo) %>%
  mutate(min = min(c_across(RR50:q97.5)),
         max = max(c_across(RR50:q97.5))) %>%
  ungroup() %>%
  mutate(noise = factor(noise, levels = c("occ", "env", "both"), labels = c("Workplace", "Transportation", "Both")),
         demo = factor(demo, levels = c("nhWhite_pct", "nhBlack_pct", "hispanic_pct", "nhAsian_pct", "nhAIAN_pct", "nhNHPI_pct"),
                       labels = c('NH White', "NH Black", "Hispanic", "NH Asian", "NH American Indian", "NH Native Hawaiian")))



# rearranging the plots to see if I can make them vertical and place them in the map figure
df_fig3_RR_a %>%
  mutate(shape = ifelse(noise == "Both", "Closed", "Open")) %>%
  ggplot(aes(x = fct_rev(noise))) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_rect(aes(xmin = 0.85, xmax = 3.15, ymin = min, ymax = max), alpha = 0.03) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0) +
  geom_point(aes(y = RR50, color = noise), size = 2, show.legend = FALSE) +
  facet_wrap(~demo, ncol = 1) +
  scale_color_manual(values = c("Workplace" = "#9E3448", "Transportation" = "#3F77AF", Both = "#2C1F38")) +
  coord_flip(ylim = c(0.78, 1.22)) +
  labs(x = "Noise Source", y = "Exposure-Risk Ratio") +
  theme_classic() +
  theme(strip.background = element_blank())

# export as 271w x 325h

df_fig3_RR_b %>%
  mutate(shape = ifelse(noise == "Both", "Closed", "Open")) %>%
  ggplot(aes(x = fct_rev(noise))) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_rect(aes(xmin = 0.85, xmax = 3.15, ymin = min, ymax = max), alpha = 0.03) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0) +
  geom_point(aes(y = RR50, color = noise), size = 2, show.legend = FALSE) +
  facet_wrap(~demo, ncol = 1) +
  scale_color_manual(values = c("Workplace" = "#9E3448", "Transportation" = "#3F77AF", Both = "#2C1F38")) +
  coord_flip(ylim = c(0.78, 1.22)) +
  labs(x = "Noise Source", y = "Exposure-Risk Ratio") +
  theme_classic() +
  theme(strip.background = element_blank())

# export as 271w x 471h










#### BACK UP ERR figures


df_fig3_RR_a %>%
  mutate(shape = ifelse(noise == "Both", "Closed", "Open")) %>%
  ggplot(aes(x = noise)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_rect(aes(xmin = 0.85, xmax = 3.15, ymin = min, ymax = max), alpha = 0.03) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0) +
  geom_point(aes(y = RR50, shape = shape), size = 2, show.legend = FALSE) +
  facet_wrap(~demo, nrow = 1) +
  scale_shape_manual(values = c("Closed" = 19, "Open" = 1)) +
  coord_cartesian(ylim = c(0.78, 1.22)) +
  labs(x = "Noise Source", y = "Exposure-Risk Ratio") +
  theme_classic() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# export 410w x 277h


df_fig3_RR_b %>%
  mutate(shape = ifelse(noise == "Both", "Closed", "Open")) %>%
  ggplot(aes(x = noise)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_rect(aes(xmin = 0.85, xmax = 3.15, ymin = min, ymax = max), alpha = 0.03) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0) +
  geom_point(aes(y = RR50, shape = shape), size = 2, show.legend = FALSE) +
  facet_wrap(~demo, nrow = 1) +
  scale_shape_manual(values = c("Closed" = 19, "Open" = 1)) +
  coord_cartesian(ylim = c(0.78, 1.22)) +
  labs(x = "Noise Source", y = "Exposure-Risk Ratio") +
  theme_classic() +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# export 604w x 277h










