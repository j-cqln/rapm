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
  # If total xG is NA replace with 0
  # These players don't have a shot on goal recorded in the 2022-2023 season
  # (2 of them only played in the playoffs; see end of file)
  mutate(playmaking_score = ifelse(is.na(playmaking_z_score),
                                   min(playmaking_z_score, na.rm = TRUE),
                                   playmaking_z_score),
         total_xg = ifelse(is.na(total_xg), 0, total_xg)) %>%
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

# Backward stepwise selection
intercept_only_off <- lm(ridge_off ~ 1, data = x)
intercept_only_def <- lm(ridge_def ~ 1, data = x)

# Model with all predictors
all_off <- lm(ridge_off ~ ., data = x %>% select(-player_id, -player, -order))
all_def <- lm(ridge_def ~ ., data = x %>% select(-player_id, -player, -order))

# Results of backward stepwise regression
backward_off <- step(all_off,
                     direction = "backward",
                     scope = formula(all_off),
                     trace = 0)

backward_def <- step(all_def,
                     direction = "backward",
                     scope = formula(all_def),
                     trace = 0)

backward_off$anova
# Step Df     Deviance Resid. Df Resid. Dev       AIC
# 1                    NA           NA       946   15.15768 -3932.476
# 2          - assists  1 4.258266e-05       947   15.15772 -3934.473
# 3 - playmaking_score  1 5.486108e-03       948   15.16321 -3936.128
# 4        - giveaways  1 9.651211e-03       949   15.17286 -3937.522

backward_def$anova
# Step Df     Deviance Resid. Df Resid. Dev       AIC
# 1                    NA           NA       946   12.00696 -4154.544
# 2        - takeaways  1 0.0004773867       947   12.00744 -4156.506
# 3 - playmaking_score  1 0.0027852059       948   12.01022 -4158.285
# 4          - assists  1 0.0020269212       949   12.01225 -4160.124
# 5        - giveaways  1 0.0173480764       950   12.02960 -4160.749

# Final models
backward_off$coefficients
# (Intercept)         goals     takeaways      total_xg 
# -0.0579611210 -0.0034079200  0.0007904995  0.0045052729

backward_def$coefficients
# (Intercept)        goals     total_xg 
# -0.008239970  0.002719674 -0.001674629

# Playmaking ranks miss 2 players who only played in the playoffs
# boxscore_data %>%
#   filter(!player_id %in% playmaking_ranks$player_id) %>%
#   select(player)
# Mackenzie MacEachern, Tye Kartye

# 28 other players missing total xG and playmaking score
# (no shots on goal that season)
# playmaking_ranks %>%
#   filter(is.na(total_xg)) %>%
#   left_join(boxscore_data, by = "player_id") %>%
#   select(player)
# Adam Ginning, Samuel Knazko, Dennis Cholowski, William Bitten, Luke Philp,
# Justin Richards, Damien Giroux, CJ Suess, Gabriel Fortier, Nick Swaney,
# Semyon Der-Arguchintsev, Filip Hallander, Hayden Hodgson, William Dufour,
# Henrik Borgstrom, Hugh McGing, Riley Sheahan, Chris Wagner, Steven Fogarty,
# Sheldon Rempal, Jonathan Gruden, Jan Jenik, Owen Beck, Tyler Benson,
# Oskar Olausson, Cole Bardreau, Bokondji Imama, Ondrej Kase