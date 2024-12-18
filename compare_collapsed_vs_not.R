SAVE_MODELS <- TRUE
BENCHMARK_MODELS <- TRUE

collapse_off <- FALSE
collapse_def <- FALSE
from_league_avg <- TRUE

# Load packages
library(fastRhockey)
library(SparseM)
library(glmnet)
library(rstan)
library(tidyverse)
library(microbenchmark)
library(pubtheme)

# Load functions
source("R/fit.R")
source("R/extract_player_coefs.R")

# Prepare data
pbp <- readRDS("data/play_by_play_data.rds")
apm_data <- readRDS(paste0("data/apm_data_collapse_off_",
                           collapse_off,
                           "_collapse_def_",
                           collapse_def,
                           "_from_league_avg_",
                           from_league_avg,
                           ".rds"))

X <- apm_data$x
y <- apm_data$y[, "xg"] ## We"ll use expected goals
w <- as.vector(apm_data$w)

# Visualize design matrix
source("R/visualize_design_matrix.R")

# Center y
yc <- y - mean(y)

# Lambdas
lambdas <- 10^seq(from = 2, to = -6, by = -.1)

if (SAVE_MODELS) {
  cat(paste(Sys.time(), "- Ordinary least squares...\n"));
  ols <- fit.ols(X, yc); cat(paste(Sys.time(), "- Weighted least squares...\n"));
  wols <- fit.wols(X, yc, w); cat(paste(Sys.time(), "- Ridge regression...\n"));
  ridge <- fit.ridge(X, yc, w, lambda = lambdas); cat(paste(Sys.time(), "- Lasso regression...\n"));
  lasso <- fit.lasso(X, yc, w, lambda = lambdas); cat(paste(Sys.time(), "- Fitting done.\n")) # cat(paste(Sys.time(), "- Poisson regression...\n"));
  # pois <- fit.pois(X, y, log(w), lambda = lambdas); cat(paste(Sys.time(), "- Fitting done.\n"))
  
  saveRDS(ols,
          file = paste0("models/collapse_off_",
                        collapse_off,"_collapse_def_",
                        collapse_def, "_from_league_avg_",
                        from_league_avg,
                        "_ols.rds"))
  
  saveRDS(wols,
          file = paste0("models/collapse_off_",
                        collapse_off,
                        "_collapse_def_",
                        collapse_def,
                        "_from_league_avg_",
                        from_league_avg,
                        "_wols.rds"))
  
  saveRDS(ridge,
          file = paste0("models/collapse_off_",
                        collapse_off,
                        "_collapse_def_",
                        collapse_def,
                        "_from_league_avg_",
                        from_league_avg,
                        "_ridge.rds"))
  
  saveRDS(lasso,
          file = paste0("models/collapse_off_",
                        collapse_off,
                        "_collapse_def_",
                        collapse_def,
                        "_from_league_avg_",
                        from_league_avg,
                        "_lasso.rds"))
  
  saveRDS(pois,
          file = paste0("models/collapse_off_",
                        collapse_off,
                        "_collapse_def_",
                        collapse_def,
                        "_from_league_avg_",
                        from_league_avg,
                        "_pois.rds"))
}

if (BENCHMARK_MODELS) {
  cat("Benchmarking...\n")
  microbenchmark(ols <- fit.ols(X, yc), times = 10)
  microbenchmark(wols <- fit.wols(X, yc, w), times = 10)
  microbenchmark(ridge <- fit.ridge(X, yc, w, lambda = lambdas), times = 10)
  microbenchmark(lasso <- fit.lasso(X, yc, w, lambda = lambdas), times = 10)
  # microbenchmark(pois <- fit.pois(X, y, log(w)), times = 10)
  
  timings <- microbenchmark(ols <- fit.ols(X, yc),
                            wols <- fit.wols(X, yc, w),
                            ridge <- fit.ridge(X, yc, w, lambda = lambdas),
                            lasso <- fit.lasso(X, yc, w, lambda = lambdas),
                            # pois <- fit.pois(X, y, log(w)),
                            times = 10)
  autoplot(timings)
  
  saveRDS(timings,
          file = paste0("models/collapse_off_",
                        collapse_off,
                        "_collapse_def_",
                        collapse_def,
                        "_from_league_avg_",
                        from_league_avg,
                        "_timings.rds"))
}

player_coefs <- extract.player.coefs(apm_data, ols, wols, ridge, lasso, NULL, # pois
                                     collapse = collapse_off || collapse_def,
                                     which_collapsed = ifelse(collapse_off,
                                                              "off",
                                                              "def"))

saveRDS(player_coefs,
        paste0("results/collapse_off_",
               collapse_off,
               "_collapse_def_",
               collapse_def,
               "_from_league_avg_",
               from_league_avg,
               "_coefs.rds"))

write.csv(player_coefs,
          paste0("results/collapse_off_",
                 collapse_off,
                 "_collapse_def_",
                 collapse_def,
                 "_from_league_avg_",
                 from_league_avg,
                 "_coefs.csv"),
          row.names = FALSE)
