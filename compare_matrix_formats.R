SAVE_MODELS <- FALSE
BENCHMARK_MODELS <- TRUE
MATRIX_TYPE <- "C" # "C", R", "T", "dense"

collapse_off <- FALSE
collapse_def <- TRUE
from_league_avg <- TRUE

# Load packages
library(SparseM)
library(glmnet)
library(tidyverse)
library(microbenchmark)

# Load functions
source("R/fit.R")
source("R/extract_player_coefs.R")

# Load data
apm_data <- readRDS(paste0("data/apm_data_collapse_off_",
                           collapse_off,
                           "_collapse_def_",
                           collapse_def,
                           "_from_league_avg_",
                           from_league_avg,
                           ".rds"))

# Extract design matrix, response vector, weights vector
X <- apm_data$x
y <- apm_data$y[, "xg"] # We"ll use expected goals
w <- as.vector(apm_data$w)

# Matrix type
X <- as.matrix(X)

if (MATRIX_TYPE == "C") {
  X <- as(X, "dgCMatrix")
} else if (MATRIX_TYPE == "R") {
  X <- as(X, "dgRMatrix")
} else if (MATRIX_TYPE == "T") {
  X <- as(X, "dgTMatrix")
}

# Center the y vector
yc <- y - mean(y)

# Fit models
lambdas <- 10^seq(from = 2, to = -6, by = -.1)

if (SAVE_MODELS) {
  cat(paste(Sys.time(), "- Ordinary least squares...\n"));
  ols <- fit.ols.svd(X, yc); cat(paste(Sys.time(), "- Weighted least squares...\n"));
  wols <- fit.wols.svd(X, yc, w); cat(paste(Sys.time(), "- Ridge regression...\n"))      ;
  ridge <- fit.ridge.svd(X, yc, lambda = lambdas); cat(paste(Sys.time(), "- Lasso regression...\n"));
  
  saveRDS(ols,
          file = paste0("models/collapse_off_",
                        collapse_off,
                        "_collapse_def_",
                        collapse_def,
                        "_from_league_avg_",
                        from_league_avg,
                        "_matrix_type_",
                        MATRIX_TYPE,
                        "_ols.rds"))
  
  saveRDS(wols,
          file = paste0("models/collapse_off_",
                        collapse_off,
                        "_collapse_def_",
                        collapse_def,
                        "_from_league_avg_",
                        from_league_avg,
                        "_matrix_type_",
                        MATRIX_TYPE,
                        "_wols.rds"))
  
  saveRDS(ridge,
          file = paste0("models/collapse_off_",
                        collapse_off,
                        "_collapse_def_",
                        collapse_def,
                        "_from_league_avg_",
                        from_league_avg,
                        "_matrix_type_",
                        MATRIX_TYPE,
                        "_ridge.rds"))
}

if (BENCHMARK_MODELS) {
  cat("Benchmarking...\n")
  microbenchmark(ols <- fit.ols.svd(X, yc), times = 10)
  microbenchmark(wols <- fit.wols.svd(X, yc, w), times = 10)
  microbenchmark(ridge <- fit.ridge.svd(X, yc, lambda = lambdas), times = 10)
  
  timings <- microbenchmark(ols <- fit.ols.svd(X, yc),
                            wols <- fit.wols.svd(X, yc, w),
                            ridge <- fit.ridge.svd(X, yc, lambda = lambdas),
                            times = 10)
  autoplot(timings)
  
  saveRDS(timings,
          file = paste0("models/collapse_off_",
                        collapse_off,
                        "_collapse_def_",
                        collapse_def,
                        "_from_league_avg_",
                        from_league_avg,
                        "_matrix_type_",
                        MATRIX_TYPE,
                        "_timings.rds"))
}

# Extract coefficients, put into data frame, along with minutes played
player_coefs <- extract.player.coefs(apm_data, ols, wols, ridge, NULL)

# Save the player coefficients
saveRDS(player_coefs,
        paste0("results/collapse_off_",
               collapse_off,
               "_collapse_def_",
               collapse_def,
               "_from_league_avg_",
               from_league_avg,
               "_matrix_type_",
               MATRIX_TYPE,
               "_coefs.rds"))

write.csv(player_coefs,
          paste0("results/collapse_off_",
                 collapse_off,
                 "_collapse_def_",
                 collapse_def,
                 "_from_league_avg_",
                 from_league_avg,
                 "_matrix_type_",
                 MATRIX_TYPE,
                 "_coefs.csv"),
          row.names = FALSE)
