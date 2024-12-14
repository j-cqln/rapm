library(fastRhockey)
library(Matrix)
library(SparseM)
library(glmnet)
library(rstan)
library(tidyverse)
library(microbenchmark)
library(pubtheme) ## devtools::install_github("bmacGTPM/pubtheme")

source("R/get_clean_nhl.R")
source("R/create_apm_data.R")

## Get data
season <- 2023
cat(paste(Sys.time(), "- Reading data...\n"));

## Get play_by_play_data and player data using the fastRhockey package
# play_by_play_raw = get.nhl.play_by_play_data(season)
# players_raw = get.nhl.players(season)
# saveRDS(play_by_play_raw, paste0("data/pbp_", season, ".rds"))
# saveRDS(players_raw, paste0("data/players_", season, ".rds"))
play_by_play_raw <- readRDS(paste0("data/pbp_", season, ".rds"))
players_raw <- readRDS(paste0("data/players_", season, ".rds"))

## Clean data
cat(paste(Sys.time(), "- Preparing data...\n"));
play_by_play_raw <- add.xg.to.data(play_by_play_raw)
play_by_play_data <- clean.nhl.play_by_play_data(play_by_play_raw)
players_data <- clean.nhl.players(players_raw)
boxscore <- clean.nhl.player.boxscore(play_by_play_raw)

saveRDS(play_by_play_data, "data/play_by_play_data.rds")
saveRDS(players_data, "data/players_data.rds")
saveRDS(boxscore, "data/boxscore.rds")

## Create (standard and weighted) design matrix and response vector
# Can collapse offense, defense, or neither (collapsing both throws error)
collapse_off <- FALSE
collapse_def <- FALSE
from_league_avg <- TRUE

apm_data <- create.apm.data(play_by_play_data,
                            players_data,
                            remove_goalies = TRUE,
                            collapse_off = collapse_off,
                            collapse_def = collapse_def,
                            from_league_avg = from_league_avg)

saveRDS(apm_data,
        paste0("data/apm_data_collapse_off_",
               collapse_off,
               "_collapse_def_",
               collapse_def,
               "_from_league_avg_",
               from_league_avg,
               ".rds"))

collapse_off <- TRUE
collapse_def <- FALSE
from_league_avg <- TRUE

apm_data <- create.apm.data(play_by_play_data,
                            players_data,
                            remove_goalies = TRUE,
                            collapse_off = collapse_off,
                            collapse_def = collapse_def,
                            from_league_avg = from_league_avg)

saveRDS(apm_data,
        paste0("data/apm_data_collapse_off_",
               collapse_off,
               "_collapse_def_",
               collapse_def,
               "_from_league_avg_",
               from_league_avg,
               ".rds"))

collapse_off <- FALSE
collapse_def <- TRUE
from_league_avg <- TRUE

apm_data <- create.apm.data(play_by_play_data,
                            players_data,
                            remove_goalies = TRUE,
                            collapse_off = collapse_off,
                            collapse_def = collapse_def,
                            from_league_avg = from_league_avg)

saveRDS(apm_data,
        paste0("data/apm_data_collapse_off_",
               collapse_off,
               "_collapse_def_",
               collapse_def,
               "_from_league_avg_",
               from_league_avg,
               ".rds"))