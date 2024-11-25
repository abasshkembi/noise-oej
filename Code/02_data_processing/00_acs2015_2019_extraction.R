# Title: Extracting 2015-2019 ACS demographic data
# Author: Abas Shkembi (ashkembi@umich.edu)
# Last updated: March 20, 2024

library(tidyverse)
library(tidycensus)

acs_vars <- load_variables(2019, dataset = "acs5", cache = TRUE)

state_abb <- c(state.abb, "DC")

acs2017_demos <- NULL
for(i in 1:length(state_abb)) {
  acs2017_tract <- get_acs(
    geography = "tract",
    year = 2019, #the endyear, gets 2015-2019 for 2017 estimates
    survey = "acs5",
    state = state_abb[i],
    variables = c(
      ## race/ethnicity
      "totalpop" = "B01001_001",
      "nhWhite" = "B03002_003",
      "nhBlack" = "B03002_004",
      "nhAIAN" = "B03002_005",
      "nhAsian" = "B03002_006",
      "nhNHPI" = "B03002_007",
      "hispanic" = "B03002_012",
      
      ## income
      "totalpop_income" = "C17002_001",
      "inc2pov_over2" = "C17002_008",
      
      ## unemployment
      "unemployed" = "B23025_005",
      "totallabor" = "B23025_003",
      
      ## education
      "totalpop_over25" = "B15003_001",
      "noschool" = "B15003_002",
      "nursery" = "B15003_003",
      "kindergarten" = "B15003_004",
      "first" = "B15003_005",
      "second" = "B15003_006",
      "third" = "B15003_007",
      "fourth" = "B15003_008",
      "fifth" = "B15003_009",
      "sixth" = "B15003_010",
      "seventh" = "B15003_011",
      "eighth" = "B15003_012",
      "ninth" = "B15003_013",
      "tenth" = "B15003_014",
      "eleventh" = "B15003_015",
      "twelfth" = "B15003_016"
    )
  )
  
  acs2017_demos_i <- acs2017_tract %>%
    select(-moe) %>%
    spread(variable, estimate) %>%
    group_by(GEOID) %>%
    transmute(
      totalpop = totalpop,
      # race ethnicity
      nhWhite_pct = round(nhWhite/totalpop*100, 3),
      nhBlack_pct = round(nhBlack/totalpop*100, 3),
      nhAIAN_pct = round(nhAIAN/totalpop*100, 3),
      nhAsian_pct = round(nhAsian/totalpop*100, 3),
      nhNHPI_pct = round(nhNHPI/totalpop*100, 3),
      hispanic_pct = round(hispanic/totalpop*100, 3),
      minor_pct = round((totalpop - nhWhite)/totalpop*100, 3),
      
      # low income
      lowinc_pct = round((totalpop_income - inc2pov_over2)/totalpop_income*100, 3),
      
      # unemployed
      unemployed_pct = round(unemployed/totallabor*100, 3),
      
      # no high school diploma
      noHS_pct = round((noschool + nursery + kindergarten + 
                          first + second + third + fourth + 
                          fifth + sixth + seventh + eighth + ninth + 
                          tenth + eleventh + twelfth)/totalpop_over25*100, 3)
    ) %>%
    ungroup()
  
  acs2017_demos <- rbind(acs2017_demos, acs2017_demos_i)
  
}

save(acs2017_demos, file = "/Users/abasshkembi/Dropbox (University of Michigan)/Noise OEJ/Data/acs2017_demos_tract.RData")


