# Title: Simulation of occupational noise measurements for every census tract in the US by state
# Author: Keshav Patel (keshavnp@umich.edu)
# Date: 09.26.2023


library(tidyverse)
#library("sampling")

# Suppress summarise info
# personal preference - don't need to run the following code
options(dplyr.summarise.inform = FALSE)

## crosswalk between major ACS and major SOC codes
crosswalk <- readxl::read_xlsx("/Users/keshavpatel/Dropbox (University of Michigan)/Noise OEJ/Data/ACS_SOC_crosswalk.xlsx")


# beginning process to read in working population count by major ACS code and census tract

# assign column names to file
acs2017_colnames <- 
  c(
    "TractCode",
    "St",
    "StCty",
    "Metro",
    "TotPop2017",
    "TotEmp2017",
    "MBSA002017",
    "MBSA102017",
    "MBSA112017",
    "MBSA122017",
    "MBSA202017",
    "MBSA212017",
    "MBSA222017",
    "MBSA232017",
    "MBSA302017",
    "MBSA312017",
    "MBSA322017",
    "MBSA342017",
    "MBSA352017",
    "MBSA402017",
    "MBSA412017",
    "MBSA422017",
    "SVC002017",
    "SVC102017",
    "SVC202017",
    "SVC212017",
    "SVC222017",
    "SVC302017",
    "SVC402017",
    "SVC502017",
    "SOF002017",
    "SOF102017",
    "SOF202017",
    "NRC002017",
    "NRC102017",
    "NRC202017",
    "NRC302017",
    "PTM002017",
    "PTM102017",
    "PTM202017",
    "PTM302017",
    "TotPop2012",
    "TotEmp2012",
    "MBSA002012",
    "MBSA102012",
    "MBSA112012",
    "MBSA122012",
    "MBSA202012",
    "MBSA212012",
    "MBSA222012",
    "MBSA232012",
    "MBSA302012",
    "MBSA312012",
    "MBSA322012",
    "MBSA342012",
    "MBSA352012",
    "MBSA402012",
    "MBSA412012",
    "MBSA422012",
    "SVC002012",
    "SVC102012",
    "SVC202012",
    "SVC212012",
    "SVC222012",
    "SVC302012",
    "SVC402012",
    "SVC502012",
    "SOF002012",
    "SOF102012",
    "SOF202012",
    "NRC002012",
    "NRC102012",
    "NRC202012",
    "NRC302012",
    "PTM002012",
    "PTM102012",
    "PTM202012",
    "PTM302012"
  )

# read in file
acs2017 <- read.csv("/Users/keshavpatel/Dropbox (University of Michigan)/Noise OEJ/Data/tract_occ_ACS_2017.csv", 
                    col.names = acs2017_colnames,
                    colClasses = c("TractCode"="character"))

# select column names that are in 2017
acs_codes <- unique(crosswalk$ACS_code) %>% paste0("2017")
acs2017_1 <- acs2017[c("TractCode", "St", "StCty", "Metro", "TotPop2017", "TotEmp2017", acs_codes)] %>% as_tibble()

# gather data in tidy format
acs2017_1 <- acs2017_1 %>%
  gather("ACS_code", "ACS_count", -c("TractCode", "St", "StCty", "Metro", "TotPop2017", "TotEmp2017"))

# can remove 2017 from column names now
acs2017_1 <- acs2017_1 %>%
  mutate(ACS_code = str_remove(ACS_code, "2017$"))

acs2017_1 <- acs2017_1 %>%
  # calculate proportion of tract that is a given major ACS code
  mutate(ACS_prop = ACS_count/TotEmp2017)


# need to now include data from the noiseJEM by census tract

# create dataset 
acs2017_mi <- acs2017_1 %>%
  left_join(crosswalk, by = "ACS_code") %>%
  # since we are crosswalking out to the noiseJEM data (which is major SOC codes),
  # we have to consider that the PTM00 ACS code = both SOC 51 and 53,
  # so their employee count would be double counted
  
  # as a result, I am splitting the PTM00 count in half for 51 and 53 SOCs
  mutate(ACS_count = ifelse(ACS_code == "PTM00", ACS_count/2, ACS_count),
         ACS_count = ifelse(SOC_code == "51-0000", as.integer(ceiling(ACS_count)), #using CEILING for 51
                            ifelse(SOC_code == "53-0000", as.integer(floor(ACS_count)), as.integer(ACS_count)))) #using FLOOR for 53



# posterior mean estimates of occupational noise exposure estimated from our noiseJEM that were reported in table 3 by Roberts et al. 2018
# by major SOC code
## NOTE: there are no estimates for Legal Occupations (23), so we are assuming they have 
## posterior estimates similar to office and administrative support occupations (78.4 dBA, major SOC code “43‐0000”)

# REFERENCE: Roberts B, Cheng W, Mukherjee B, Neitzel RL. 
###          Imputation of missing values in a large job exposure matrix using hierarchical information. 
###          J Exposure Sci Environ Epidemiol. 2018;28(6):615‐648. 
###          doi: 10.1038/s41370-018-0037-x

################################ occupational noise exposures

soc_noise_est <-
  tibble(
    major_soc = c("11-0000", "13-0000", "15-0000", "17-0000", "19-0000", "21-0000", "23-0000", "25-0000", "27-0000", 
                  "29-0000", "31-0000", "33-0000", "35-0000", "37-0000", "39-0000", "41-0000", "43-0000", 
                  "45-0000", "47-0000", "49-0000", "51-0000", "53-0000", "55-0000"),
    
    mu = c(81.8, 82.7, 80.9, 80.7, 82.8, 80.7, 78.4, 84.0, 82.1, 79.9, 82.3, 81.2, 82.7, 85.0, 84.8, 82.3, 78.4, 85.5, 83.5, 83.3, 85.2, 83.3, 78.9),
    
    sd = c(1.8, 2.4, 2.7, 1.6, 2.0, 2.8, 1.2, 2.9, 2.0, 1.8, 2.9, 1.8, 1.7, 2.5, 1.9, 2.1, 1.2, 2.0, 0.9, 1.2, 0.7, 0.9, 2.8)
    
  )


# remove rows with 0 employee count
acs2017_mi_0 <- acs2017_mi %>% filter(ACS_count != 0) %>% arrange(TractCode)
# left join the noise estimates mu and sd
acs2017_mi_0 <- acs2017_mi_0 %>% left_join(soc_noise_est, by = c("SOC_code" = "major_soc"))

# create column for the first two digits of tract code (which results in state FIPS code)
acs2017_mi_0 <- acs2017_mi_0 %>% mutate(StCd = str_extract(TractCode, "^\\d\\d"))

fips_st <- acs2017_mi_0 %>% mutate(StCd = str_extract(TractCode, "^\\d\\d")) %>% 
  dplyr::select(StCd, St) %>% distinct %>%
  filter(str_detect(St, "[A-Z]"))

acs2017_mi_0 <- acs2017_mi_0 %>% 
  select(-St) %>% # remove St abbreviation column
  left_join(fips_st, by = "StCd") # add in new State ABBR/CODE crosswalk

# note, 57 tracts will be missing since n_occ < 21

acs2017_mi_0 %>% count(TractCode)

all_states <- fips_st$St %>% unique %>% as.character()





# begin simulation for loop for each state in the US

#############
############# UPDATE THE FOLLOWING
i <- 51
#############
#############

current_state <- all_states[i]
current_state


# filtering dataset for current state selected
acs2017_mi_0_st <- acs2017_mi_0 %>% filter(St == current_state)

# set number of iterations
MC_iter <- 10000

# vector of each census tract in the dataset
## more efficient solution
tract_vector_mi <- rep(acs2017_mi_0_st$TractCode, acs2017_mi_0_st$ACS_count)

# vector of SOC codes in the dataset
## most efficient solution as of now
occ_vector_mi <- rep(acs2017_mi_0_st$SOC_code, acs2017_mi_0_st$ACS_count)

#final matrix
final_df <- c()
# for loop for every census tract

filename_simul <- paste0("/Users/keshavpatel/University of Michigan Dropbox/Keshav Patel/Noise OEJ/Data/Generated Data/Simulations/Occupational Simulated data")
dir.create(file.path(filename_simul, current_state))

  for(j in 1:MC_iter) {
    
    #extracting the noise measurements for i-th MC iteration
    final_iter_tract_occ_noise_vectorized <- rnorm(sum(acs2017_mi_0_st$ACS_count),
                                                   rep(acs2017_mi_0_st$mu, acs2017_mi_0_st$ACS_count),
                                                   rep(acs2017_mi_0_st$sd, acs2017_mi_0_st$ACS_count))
    
    
    #determining whether each measurement is >= 85 dBA
    final_iter_above85_occ_noise_vectorized <- final_iter_tract_occ_noise_vectorized >= 85
    
    
    ### compute summary data
    
    final_tibble <- tibble(simulation = j,
                           GEOID = tract_vector_mi,
                           above_85 = final_iter_above85_occ_noise_vectorized
    )
    
    final_tibble <- final_tibble %>%
      group_by(simulation, GEOID) %>%
      summarise(n = n(), 
                n_above_85 = sum(above_85)
      )
    
    # initiate filename
    filename_simulj <- paste0 (filename_simul, "/", current_state, "/simul", j, "_", current_state, ".Rds" )
    
    # svae file
    saveRDS(final_tibble, file = filename_simulj)

    # final information stored locally to create `final_occ_df_tract` dataframe below
    final_df <- rbind(final_df, final_tibble)
    
    # print every 100 iteration status
    if((j %% 100) == 0) {
      print(paste0(current_state, " Iteration: ", paste0(j, paste0(" / ", MC_iter))))
    }
    
    
  }



# aggregate the 10,000 simulations for every census tract within i-th state
final_occ_df_tract <- final_df %>% 
  ungroup() %>%
  
  mutate(perc_85 = n_above_85/n*100) %>%
  mutate(perc_85 = ifelse(n < 21, NA, perc_85)) %>%
  
  group_by(GEOID) %>%
  
  summarise(n_occ = mean(n), 
            mean_perc_85 = round(mean(perc_85, na.rm = TRUE), 1),
            median_perc_85 = round(median(perc_85, na.rm = TRUE), 1),
            sd_perc_85 = round(sd(perc_85, na.rm = TRUE), 1),
            q2.5_perc_85 = round(quantile(perc_85, 0.025, na.rm = TRUE), 1),
            q97.5_perc_85 = round(quantile(perc_85, 0.975, na.rm = TRUE), 1))

# initiate filename for aggregated data
filename_agg_simul <- filename_simul <- paste0("//Users/keshavpatel/University of Michigan Dropbox/Keshav Patel/Noise OEJ/Data/Generated Data/Final state aggregated data/Workplace/finaldata_", current_state, "_s", MC_iter, ".Rds")
# save aggregated data
saveRDS(final_occ_df_tract, file = filename_agg_simul)
  



