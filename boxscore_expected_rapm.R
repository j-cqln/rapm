# Load packages
library(readr)
library(lme4)
library(ModelMetrics)
library(tidyverse)
library(microbenchmark)

# Load functions
source("R/get_clean_nhl.R")
source("R/create_apm_data.R")
source("R/fit.R")

# Prepare data
play_by_play_data <- readRDS("data/play_by_play_data.rds")

players_data <- readRDS("data/players_data.rds")
player_order <- data.frame(player = get.player.order(play_by_play_data, players_data)) %>%
  mutate(order = row_number())

boxscore_data <- readRDS("data/boxscore_data.rds")

collapse_off <- FALSE
collapse_def <- FALSE

coefs <- readRDS(paste0("results/collapse_off_",
                        collapse_off,
                        "_collapse_def_",
                        collapse_def,
                        "_from_league_avg_",
                        from_league_avg,
                        "_coefs.rds"))

ridge_off <- coefs$ridge_off
ridge_def <- coefs$ridge_def

playmaking_ranks <- read_csv("data/ranks.csv")
playmaking_ranks <- playmaking_ranks %>%
  filter(season == 2022) %>%
  select(shooter_id, position_group, total_xg, playmaking_z_score) %>%
  rename(player_id = shooter_id) %>%
  group_by(player_id) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Merge boxscore and playmaking data for expected RAPM model
x <- boxscore_data %>%
  left_join(playmaking_ranks, by = "player_id") %>%
  # If playmaking score is NA replace with lowest value
  mutate(playmaking_score = ifelse(is.na(playmaking_z_score),
                                   min(playmaking_z_score, na.rm = TRUE),
                                   playmaking_z_score)) %>%
  select(player_id, player,
         goals, assists, takeaways, giveaways,
         total_xg, playmaking_score) %>%
  # Put in order of player matching order in ridge coefficients
  left_join(player_order, by = "player") %>%
  arrange(order)

off_results <- pseudo_bayes_model(x, ridge_off, pure_boxscore = TRUE)
def_results <- pseudo_bayes_model(x, ridge_def, pure_boxscore = TRUE)

off_results_alternate <- pseudo_bayes_model(x, ridge_off, pure_boxscore = FALSE)
def_results_alternate <- pseudo_bayes_model(x, ridge_def, pure_boxscore = FALSE)

# Average of ridge coefficients and predicted ridge coefficients
pseudo_bayes_off <- (ridge_off + off_results$predicted_values) / 2
pseudo_bayes_def <- (ridge_def + def_results$predicted_values) / 2

pseudo_bayes_off_alternate <- (ridge_off + off_results_alternate$predicted_values) / 2
pseudo_bayes_def_alternate <- (ridge_def + def_results_alternate$predicted_values) / 2

# Rank players based on new coefficients
ranked_players <- x %>%
  mutate(ridge_off = ridge_off,
         ridge_def = ridge_def,
         boxscore_based_off = off_results$predicted_values,
         pseudo_bayes_off = pseudo_bayes_off,
         boxscore_based_def = def_results$predicted_values,
         pseudo_bayes_def = pseudo_bayes_def,
         boxscore_based_off_alternate = off_results_alternate$predicted_values,
         pseudo_bayes_off_alternate = pseudo_bayes_off_alternate,
         boxscore_based_def_alternate = def_results_alternate$predicted_values,
         pseudo_bayes_def_alternate = pseudo_bayes_def_alternate)

# Save
saveRDS(ranked_players, "results/ranked_players.rds")

# Playmaking ranks miss 2 players who only played in the playoffs
# boxscore_data %>%
#   filter(!player_id %in% playmaking_ranks$player_id) %>%
#   select(player_id, player)
# Mackenzie MacEachern, Tye Kartye