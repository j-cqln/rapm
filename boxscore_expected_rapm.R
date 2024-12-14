## Load packages
library(lme4)
library(ModelMetrics)
library(tidyverse)
library(microbenchmark)

## Load functions
source("R/get_clean_nhl.R")

# Linear model to predict the ridge coefficients using box score statistics
pseudo_bayes_model <- function(x, y, pure_boxscore = TRUE) {
  if (pure_boxscore == TRUE) {
    model <- lm(y ~ goals + assists + takeaways + giveaways, data = x)
  } else {
    model <- lm(y ~ xg + playmaking_score + takeaways + giveaways, data = x)
  }

  predicted_values <- predict(model, newdata = x)
  
  return(list(model = model, predicted_values = predicted_values))
}

# Sample data
sample_data <- data.frame(goals = c(0, 10, 5, 8, 3, 7, 0),
                          assists = c(1, 5, 3, 4, 2, 0, 10),
                          takeaways = c(3, 2, 1, 3, 1, 0, 15),
                          giveaways = c(2, 3, 2, 4, 1, 8, 5),
                          xg = c(0.5, 1.2, 0.8, 1.5, 0.7, 1.0, 0.3),
                          playmaking_score = c(0.8, 1.5, 1.0, 1.2, 0.9, 0.5, 1.8))

sample_ridge_coefficients <- c(0.05, 0.47, 0.4, 0.25, 0.21, 0.18, 0.17)

results <- pseudo_bayes_model(sample_data,
                              sample_ridge_coefficients,
                              pure_boxscore = TRUE)
results

# Average of ridge coefficients and predicted ridge coefficients
pseudo_bayes <- (sample_ridge_coefficients + results$predicted_values) / 2
pseudo_bayes

# Rank players based on new coefficients
ranked_players <- sample_data %>%
  mutate(ridge = sample_ridge_coefficients,
         boxscore_based = results$predicted_values,
         pseudo_bayes = pseudo_bayes) %>%
  arrange(desc(pseudo_bayes))

ranked_players