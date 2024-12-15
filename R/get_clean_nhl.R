get.nhl.pbp <- function(season = NULL) {
  d <- try(load_nhl_pbp(season)) %>% as.data.frame()
  return(d)
}

get.nhl.players <- function(season = NULL) {
  d <- try(load_nhl_rosters(season)) %>% as.data.frame()
  return(d)
}

get.nhl.player.boxscore <- function(season = NULL) {
  d <- try(load_nhl_player_box(season)) %>% as.data.frame()
  return(d)
}

clean.nhl.play.by.play <- function(d = NULL, final_teams_only = FALSE) {
  if (final_teams_only == TRUE) {
    d <- d %>% 
      filter((home_abbreviation == "VGK" & away_abbreviation == "FLA") | 
             (away_abbreviation == "VGK" & home_abbreviation == "FLA"))
  }
  
  # Keep only 5v5, non empty net situations, remove unnecessary columns
  # Create new columns needed for design matrix or response vector
  dd <- d %>%
    group_by(game_id) %>%
    # Create event_length column, which will serve as the weights
    # Create score_diff columns, as of the time of the event
    # Don't want to include goal in this row, if any, into the score_diff
    # so lag this by 1
    mutate(event_length = c(diff(game_seconds),0)) %>%
    mutate(home_score_diff = lag(home_score - away_score, default = 0), 
           away_score_diff = lag(away_score - home_score, default = 0)) %>%
    ungroup() %>%
    filter(strength_state == "5v5") %>%
    select(game_id, 
           strength_state,
           event_id,
           event_type, 
           event_team_abbr,
           home_abbreviation,
           away_abbreviation,
           home_id,
           away_id,
           home_on_1:home_on_5, home_goalie, 
           away_on_1:away_on_5, away_goalie, 
           home_score, 
           away_score,
           home_score_diff,
           away_score_diff,
           event_length,
           game_seconds,
           xg) %>%
    rename(home_team = home_abbreviation, 
           away_team = away_abbreviation, 
           home_on_6 = home_goalie, 
           away_on_6 = away_goalie, 
           event_team = event_team_abbr) %>%
    arrange(game_id, event_id) %>% 
    mutate(shift_id = cumsum(event_type == "CHANGE"),   
           face_id = cumsum(event_type == "FACEOFF")) %>%
    # Bin +4, +5, etc with +3, and -4, -5, etc with -3
    # Create home indicator
    # Create outcome variables
    mutate(home_score_diff = case_when(home_score_diff > 3 ~ 3,
                                       home_score_diff < -3 ~ -3, 
                                       TRUE ~ home_score_diff), 
           away_score_diff = case_when(away_score_diff > 3 ~ 3,
                                       away_score_diff < -3 ~ -3, 
                                       TRUE ~ away_score_diff),
           ha = case_when(event_team == home_team ~ "home", 
                          event_team == away_team ~ "away", 
                          TRUE ~ ""), 
           home_goal = ifelse(event_type == "GOAL" & ha == "home", 1, 0), 
           away_goal = ifelse(event_type == "GOAL" & ha == "away", 1, 0), 
           home_shot = ifelse(event_type %in% c("GOAL", "SHOT") & ha == "home", 1, 0), 
           away_shot = ifelse(event_type %in% c("GOAL", "SHOT") & ha == "away", 1, 0), 
           home_xg = ifelse(!is.na(xg) & ha=="home", xg, 0), 
           away_xg = ifelse(!is.na(xg) & ha=="away", xg, 0)) %>%
    as.data.frame()

  # Remove unnecessary columns
  # Collapse rows with same game_id, shift_id, face_id, etc.
  # Create a primary key
  dd <- dd %>%
    select(-event_type, -event_team) %>%
    group_by(game_id, home_id, away_id,
             shift_id, face_id,
             home_score_diff, 
             away_score_diff, 
             home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6,
             away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6) %>%
    summarise(home_goal = sum(home_goal),
              away_goal = sum(away_goal),
              home_shot = sum(home_shot),
              away_shot = sum(away_shot),
              home_xg = sum(home_xg),
              away_xg = sum(away_xg),
              event_length = sum(event_length), .groups="keep"#, 
              ) %>%
    ungroup() %>%
    group_by(game_id) %>%
    mutate(row = 1:n(), 
           row = str_pad(row, width = 4, side = "left", pad = "0"),
           key = paste0(game_id, row)) %>% # create a primary key
    ungroup() %>%
    select(-shift_id, -face_id, -row, -game_id) %>%
    filter(event_length != 0) %>%
    as.data.frame()
  
  # Remove unnecessary rows
  # Convert goals into "goals per 60 minutes"
  dd <- dd %>% 
    mutate(home_goal = home_goal / event_length * 3600, 
           home_shot = home_shot / event_length * 3600, 
           home_xg = home_xg / event_length * 3600,
           away_goal = away_goal / event_length * 3600, 
           away_shot = away_shot / event_length * 3600, 
           away_xg = away_xg / event_length * 3600)

  # Reorder cols
  dd <- dd %>%
    rename(sec = event_length) %>%
    select(key, home_id, away_id,
           home_on_1, home_on_2, home_on_3, home_on_4, home_on_5, home_on_6,
           away_on_1, away_on_2, away_on_3, away_on_4, away_on_5, away_on_6, 
           home_goal, away_goal, 
           home_shot, away_shot, 
           home_xg, away_xg, 
           sec, 
           home_score_diff,
           away_score_diff)
  
  # Temporary workaround for players with the same name
  dd <- dd %>% 
    mutate(across(home_on_1:home_on_6,
                  ~ case_when(.x == "Sebastian.Aho" & home_id == 2 ~ "Sebastian.Aho.NYI",
                              .x == "Sebastian.Aho" & home_id == 12 ~ "Sebastian.Aho.CAR",
                              .x == "Matt.Murray" & home_id == 25 ~ "Matt.Murray.DAL",
                              .x == "Matt.Murray" & home_id == 10 ~ "Matt.Murray.TOR",
                              TRUE ~ .x))) %>%
    mutate(across(away_on_1:away_on_6,
                  ~ case_when(.x == "Sebastian.Aho" & away_id == 2 ~ "Sebastian.Aho.NYI",
                              .x == "Sebastian.Aho" & away_id == 12 ~ "Sebastian.Aho.CAR",
                              .x == "Matt.Murray" & away_id == 25 ~ "Matt.Murray.DAL",
                              .x == "Matt.Murray" & away_id == 10 ~ "Matt.Murray.TOR",
                              TRUE ~ .x)))
  
  return(dd)
}

# Build simple xG model, add xG to play-by-play data
add.xg.to.data <- function(d) {
  # Prep data for model
  dm <- d %>%
    filter(!is.na(shot_angle), !is.na(shot_distance)) %>%
    mutate(goal = ifelse(event == "Goal", 1, 0))
  
  # Fit xg model, including missed shots
  xg <- glm(goal ~ shot_distance + shot_angle,
            data = dm,
            family = binomial)
  
  dm$xg <- predict(xg, type = "response")
  dm <- dm %>% select(event_id, xg)
  d <- d %>% left_join(dm, by = "event_id")
  
  return(d)
}

# https://fastrhockey.sportsdataverse.org/reference/load_nhl_rosters.html
clean.nhl.players <- function(p = NULL) {
  d <- p %>%
    rename(player = player_full_name) %>%
    mutate(player = gsub(" ", ".", player), 
           position_type = substr(position_type, 1, 1)) %>%
    select(player, position_type, player_id, team_id) %>%
    # Change some goalie names to match the pbp file
    mutate(player = case_when(player == "Cal.Peterson" ~ "Calvin.Peterson",
                              player == "Marc-Andre.Fleury" ~ "Marc.Andre.Fleury",
                              player == "Ukko-Pekka.Luukkonen" ~ "Ukko.Pekka.Luukkonen",
                              TRUE ~ player)) %>% 
    unique()
  
  # Find all players with the same name but different player id
  # d %>% 
  #   group_by(player) %>%
  #   summarise(n = n_distinct(player_id)) %>%
  #   filter(n > 1)
  # Matt Murray, Sebastian Aho in 2023
  # This is a temporary workaround, ideally player ids will be used
  d <- d %>%
    mutate(player = case_when(player == "Sebastian.Aho" & team_id == "2" ~ "Sebastian.Aho.NYI",
                              player == "Sebastian.Aho" & team_id == "12" ~ "Sebastian.Aho.CAR",
                              player == "Matt.Murray" & team_id == "25" ~ "Matt.Murray.DAL",
                              player == "Matt.Murray" & team_id == "10" ~ "Matt.Murray.TOR",
                              TRUE ~ player))
  
  return(d)
}

clean.nhl.player.boxscore <- function(d = NULL, p_cleaned = NULL) {
  dd <- d %>%
    filter(position_code != "G") %>%
    select(player_id,
           player_full_name,
           skater_stats_assists,
           skater_stats_goals,
           skater_stats_power_play_assists,
           skater_stats_power_play_goals,
           skater_stats_short_handed_assists,
           skater_stats_short_handed_goals,
           skater_stats_takeaways,
           skater_stats_giveaways) %>%
    # 5v5 only
    mutate(skater_stats_goals = skater_stats_goals - skater_stats_power_play_goals - skater_stats_short_handed_goals,
           skater_stats_assists = skater_stats_assists - skater_stats_power_play_assists - skater_stats_short_handed_assists) %>%
    rename(player = player_full_name,
           goals = skater_stats_goals,
           assists = skater_stats_assists,
           takeaways = skater_stats_takeaways,
           giveaways = skater_stats_giveaways) %>%
    group_by(player_id) %>%
    summarise(goals = sum(goals),
              assists = sum(assists),
              giveaways = sum(giveaways),
              takeaways = sum(takeaways)) %>%
    left_join(p_cleaned %>% select(player, player_id, team_id), by = "player_id") %>%
    # Temporary workaround for players with the same name
    mutate(player = case_when(player == "Sebastian.Aho" & team_id == "2" ~ "Sebastian.Aho.NYI",
                              player == "Sebastian.Aho" & team_id == "12" ~ "Sebastian.Aho.CAR",
                              player == "Matt.Murray" & team_id == "25" ~ "Matt.Murray.DAL",
                              player == "Matt.Murray" & team_id == "10" ~ "Matt.Murray.TOR",
                              TRUE ~ player)) %>%
    group_by(player_id) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(player_id, player, goals, assists, takeaways, giveaways) %>%
    as.data.frame()
  
  return(dd)
}
