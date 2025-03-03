# Load required libraries

library(tidyverse)
library(BasketballAnalyzeR)
library(nbastatR)


# Acquire data,wrangle, and filter

# Increase connection size for handling large datasets

Sys.setenv(VROOM_CONNECTION_SIZE = 500072)

# The goal here it to get shot data for all LBJ teams from 2003 to 2024

# Using a for-loop for each team and season lebron played in

# Team 1: Starting with the Cleveland Cavaliers

# Initialize an empty list to store shot data for each season
all_shots_cc <- list()

# Loop through each season from 2003 to 2010
for (season in 2003:2010) {
  # Fetch shot data for the current season
  season_shots_cc <- teams_shots(team_ids = 1610612739, seasons = season)
  
  # Append the data to the list
  all_shots_cc[[as.character(season)]] <- season_shots_cc
}

# Combine all cav seasons data into a single data frame
shots_cc <- bind_rows(all_shots_cc)

# Team 2: Miami Heat
all_shots_mh <- list()

# Loop through each season from 2003 to 2010
for (season in 2011:2014) {
  # Fetch shot data for the current season
  season_shots_mh <- teams_shots(team_ids = 1610612748, seasons = season)
  
  # Append the data to the list
  all_shots_mh[[as.character(season)]] <- season_shots_mh
}

# Combine all miami seasons data into a single data frame
shots_mh <- bind_rows(all_shots_mh)

# Team 3: Back to the Cavs

# Initialize an empty list to store shot data for each season
all_shots_cc2 <- list()

# Loop through each season from 2015 to 2018
for (season in 2015:2018) {
  # Fetch shot data for the current season
  season_shots_cc2 <- teams_shots(team_ids = 1610612739, seasons = season)
  
  # Append the data to the list
  all_shots_cc2[[as.character(season)]] <- season_shots_cc2
}

# Combine all cavs seasons data into a single data frame
shots_cc2 <- bind_rows(all_shots_cc2)

# Team 4/Current Team: The LosAngeles Lakers

# Initialize an empty list to store shot data for each season
all_shots_ll <- list()

# Loop through each season from 2019 to 2024
for (season in 2019:2024) {
  # Fetch shot data for the current season
  season_shots_ll <- teams_shots(team_ids = 1610612747, seasons = season)
  
  # Append the data to the list
  all_shots_ll[[as.character(season)]] <- season_shots_ll
}

# Combine all laker seasons data into a single data frame
shots_ll <- bind_rows(all_shots_ll)

# Combine all shots data frames above into one main data frame (to rule them all, haha)
shots <- rbind(shots_cc, shots_mh, shots_cc2, shots_ll)

# save to csv
#write.csv(shots,"./data/allshots-2003-2024.csv", row.names = FALSE)

# Filter only LeBron James' shots
lebron_shots <- shots |>
  filter(namePlayer == "LeBron James") |>
  mutate(season = as.factor(yearSeason))  # Add a season column, but make it a factor for ordering later

# save to csv
#write.csv(lebron_shots,"./data/lebron-shots-2003-2024.csv", row.names = FALSE)


