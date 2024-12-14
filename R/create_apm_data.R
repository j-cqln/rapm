create.apm.play_by_play_data = function(play_by_play_data = NULL, players_data = NULL,
                           remove_goalies = TRUE,
                           collapse_off = FALSE,
                           collapse_def = FALSE,
                           from_league_avg = FALSE) {
  
  if (collapse_off == TRUE & collapse_def == TRUE){
    stop("Cannot collapse both offense and defense.")
  }

  # Put play_by_play_data in long form first, seems non-intuitive but makes use of xtabs
  # (fairly fast way to create sparse design matrix) 
  # play_by_play_data one row per player-shift, "key" is unique identifier for each shift
  
  # First, put in long format
  home_players <- play_by_play_data %>%
    select(key, starts_with("home_on"), sec, home_id) %>%
    pivot_longer(cols = starts_with("home_on")) %>%
    rename(player = value,
           team_id = home_id) %>%
    select(key, player, sec, team_id)
  
  away_players <- play_by_play_data %>%
    select(key, starts_with("away_on"), sec, away_id) %>%
    pivot_longer(cols = starts_with("away_on")) %>%
    rename(player = value,
           team_id = away_id) %>%
    select(key, player, sec, team_id)
  
  players <- rbind(home_players, away_players) %>%
    group_by(player, team_id) %>%
    summarize(sec = sum(sec, na.rm = TRUE)) %>%
    select(player, team_id, sec)
  
  players$team_id <- as.integer(players$team_id)
  
  # For traded players, use their highest ice time as affiliated team
  players_data <- left_join(players_data, players, by = c("player", "team_id")) %>%
    arrange(desc(sec))
  players_data <- players_data[!duplicated(players_data$player), ]
  players_data <- players_data[with(players_data, order(team_id, position_type)), ]
  
  long_data <- play_by_play_data %>% 
    arrange(key) %>% 
    select(key, starts_with("home_on"), starts_with("away_on")) %>%
    pivot_longer(cols = -key, 
                 names_to = "ha",
                 values_to = "player") %>%
    mutate(ha = gsub("_.+", "", ha), 
           player = factor(player, levels = players_data$player))
  
  # Home and away players
  xh = long_data %>% filter(ha == "home")
  xa = long_data %>% filter(ha == "away")
  
  # Score differentials for home and away team
  sh = play_by_play_data  %>% select(key, home_score_diff)
  sa = play_by_play_data  %>% select(key, away_score_diff)
  sh = sh %>% mutate(home_score_diff = case_when(home_score_diff > 0 ~ paste0("up", home_score_diff),
                                                 home_score_diff < 0 ~ gsub("-", "down", home_score_diff),
                                                 TRUE ~ "tie"))
  sa = sa %>% mutate(away_score_diff = case_when(away_score_diff > 0 ~ paste0("up", away_score_diff),
                                                 away_score_diff < 0 ~ gsub("-", "down", away_score_diff), 
                                                 TRUE ~ "tie"))
  sh = sh %>% mutate(home_score_diff = factor(home_score_diff,
                                              levels = c("down3", "down2", "down1",
                                                         "tie",
                                                         "up1", "up2", "up3")))
  sa = sa %>% mutate(away_score_diff = factor(away_score_diff,
                                              levels = c("down3", "down2", "down1",
                                                         "tie",
                                                         "up1", "up2", "up3")))
  
  if (collapse_off) {
    # Collapse offense into average xg for for offense players on ice
    home_players <- play_by_play_data %>%
      select(key, starts_with("home_on"), home_xg, sec) %>%
      pivot_longer(cols = starts_with("home_on")) %>%
      rename(player = value,
             xg_for = home_xg) %>%
      mutate(xg_for = xg_for / 3600 * sec) %>%
      select(key, player, xg_for, sec)
    
    away_players <- play_by_play_data %>%
      select(key, starts_with("away_on"), away_xg, sec) %>%
      pivot_longer(cols = starts_with("away_on")) %>%
      rename(player = value,
             xg_for = away_xg) %>%
      mutate(xg_for = xg_for / 3600 * sec) %>%
      select(key, player, xg_for, sec)

    players <- rbind(home_players, away_players) %>%
      group_by(player) %>%
      summarize(xg_for = sum(xg_for, na.rm = TRUE),
                sec = sum(sec, na.rm = TRUE)) %>%
      mutate(xg_for = xg_for / sec * 3600) %>%
      select(player, xg_for, sec)
    
    if (from_league_avg) {
      league_avg <- mean(players$xg_for, na.rm = TRUE)
      players <- players %>% mutate(xg_for = xg_for - league_avg)
    }
    
    xa2 <- play_by_play_data %>%
      select(key, starts_with("away_on")) %>%
      left_join(players,
                by = c("away_on_1" = "player")) %>%
      left_join(players,
                by = c("away_on_2" = "player")) %>%
      left_join(players,
                by = c("away_on_3" = "player")) %>%
      left_join(players,
                by = c("away_on_4" = "player")) %>%
      left_join(players,
                by = c("away_on_5" = "player"))
    
    # Average of row values from columns which begin with xg_for
    xa2$xg_for <- rowMeans(xa2[, grepl("xg_for", colnames(xa2))], na.rm = TRUE)
    xa2 <- xa2 %>% select(key, xg_for)
    
    xh2 <- play_by_play_data %>%
      select(key, starts_with("home_on")) %>%
      left_join(players,
                by = c("home_on_1" = "player")) %>%
      left_join(players,
                by = c("home_on_2" = "player")) %>%
      left_join(players,
                by = c("home_on_3" = "player")) %>%
      left_join(players,
                by = c("home_on_4" = "player")) %>%
      left_join(players,
                by = c("home_on_5" = "player"))
    
    # Average of row values from columns which begin with xg_for
    xh2$xg_for <- rowMeans(xh2[, grepl("xg_for", colnames(xh2))], na.rm = TRUE)
    xh2 <- xh2 %>% select(key, xg_for)
    
    xh2 <- Matrix(matrix(xh2$xg_for), sparse = TRUE)
    xa2 <- Matrix(matrix(xa2$xg_for), sparse = TRUE)
  }
  
  if (collapse_def) {
    # Collapse defense into average xg against for defense players on ice
    home_players <- play_by_play_data %>%
      select(key, starts_with("home_on"), away_xg, sec) %>%
      pivot_longer(cols = starts_with("home_on")) %>%
      rename(player = value,
             xg_against = away_xg) %>%
      mutate(xg_against = xg_against / 3600 * sec) %>%
      select(key, player, xg_against, sec)
    
    away_players <- play_by_play_data %>%
      select(key, starts_with("away_on"), home_xg, sec) %>%
      pivot_longer(cols = starts_with("away_on")) %>%
      rename(player = value,
             xg_against = home_xg) %>%
      mutate(xg_against = xg_against / 3600 * sec) %>%
      select(key, player, xg_against, sec)
    
    players <- rbind(home_players, away_players) %>%
      group_by(player) %>%
      summarize(xg_against = sum(xg_against, na.rm = TRUE),
                sec = sum(sec, na.rm = TRUE)) %>%
      mutate(xg_against = xg_against / sec * 3600) %>%
      select(player, xg_against)
    
    if (from_league_avg == TRUE) {
      league_avg <- mean(players$xg_against, na.rm = TRUE)
      players <- players %>% mutate(xg_against = xg_against - league_avg)
    }
    
    xa2 <- play_by_play_data %>%
      select(key, starts_with("away_on")) %>%
      left_join(players,
                by = c("away_on_1" = "player")) %>%
      left_join(players,
                by = c("away_on_2" = "player")) %>%
      left_join(players,
                by = c("away_on_3" = "player")) %>%
      left_join(players,
                by = c("away_on_4" = "player")) %>%
      left_join(players,
                by = c("away_on_5" = "player"))
    
    # Average of row values from columns which begin with xg_against
    xa2$xg_against <- rowMeans(xa2[, grepl("xg_against", colnames(xa2))],
                               na.rm = TRUE)
    
    xa2 <- xa2 %>% select(key, xg_against)
    
    xh2 <- play_by_play_data %>%
      select(key, starts_with("home_on")) %>%
      left_join(players,
                by = c("home_on_1" = "player")) %>%
      left_join(players,
                by = c("home_on_2" = "player")) %>%
      left_join(players,
                by = c("home_on_3" = "player")) %>%
      left_join(players,
                by = c("home_on_4" = "player")) %>%
      left_join(players,
                by = c("home_on_5" = "player"))
    
    # Average of row values from columns which begin with xg_against
    xh2$xg_against <- rowMeans(xh2[, grepl("xg_against", colnames(xh2))],
                               na.rm = TRUE)
    
    xh2 <- xh2 %>% select(key, xg_against)
    
    xh2 <- Matrix(matrix(xh2$xg_against), sparse = TRUE)
    xa2 <- Matrix(matrix(xa2$xg_against), sparse = TRUE)
  }
  
  # Create pieces of the design matrix
  # Uses xtabs converts play-by-play directly into sparse matrix form
  xh <- xtabs(~ key + player,
              play_by_play_data = xh,
              sparse = TRUE,
              drop.unused.levels = FALSE)
  
  xa <- xtabs(~ key + player,
              play_by_play_data = xa,
              sparse = TRUE,
              drop.unused.levels = FALSE)
  
  sh <- xtabs(~ key + home_score_diff,
              play_by_play_data = sh,
              sparse = TRUE,
              drop.unused.levels = FALSE)
  
  sa <- xtabs(~ key + away_score_diff,
              play_by_play_data = sa,
              sparse = TRUE,
              drop.unused.levels = FALSE)
  
  # Remove goalies if asked to
  if (remove_goalies){
    goalies <- players_data %>%
      filter(position_type == "G") %>%
      select(player) %>%
      unlist() %>%
      as.character() %>%
      unique()
    
    xh <- xh[, !colnames(xh) %in% goalies] # keep columns that are not goalies
    xa <- xa[, !colnames(xa) %in% goalies]
  }
  
  # Different names for offensive, defensive variables for matrices O and play_by_play_data
  colnames(xh) <- paste0(colnames(xh), "_off")
  colnames(xa) <- paste0(colnames(xa), "_def")
  
  # Create vectors needed for the home indicator variable
  # 1 corresponds to home team offense, 0 is away team on offense
  n.row <- nrow(xh)
  ones <- play_by_play_data.frame(home = rep(1, n.row)) %>% as.matrix()
  zeros <- play_by_play_data.frame(home = rep(0, n.row)) %>% as.matrix()
  
  # Create design matrix with home indicator, score effects, 
  # players on offense, and players on defense
  if (collapse_off == FALSE && collapse_def == FALSE) {
    x <- rbind(cbind(ones, sh, xh, xa), # home team offense, away team defense
               cbind(zeros, sa, xa, xh)) # away team offense, home team defense
    
  } else if (collapse_off) {
    x <- rbind(cbind(ones, sh, xh2, xa), # home team offense, away team defense
               cbind(zeros, sa, xa2, xh)) # away team offense, home team defense
    colnames(x)[ncol(ones) + ncol(sh) + 1] = "collapsed_off"
    
  } else if (collapse_def) {
    x <- rbind(cbind(ones,  sh, xh, xa2), # home team offense, away team defense
               cbind(zeros, sa, xa, xh2)) # away team offense, home team defense
    colnames(x)[ncol(x)] = "collapsed_def"
  }
  
  # Create response vector(s)
  yh <- play_by_play_data %>%
    select(home_goal, home_shot, home_xg) %>%
    as.matrix() # home team on offense
  
  ya <- play_by_play_data %>%
    select(away_goal, away_shot, away_xg) %>%
    as.matrix() # away team
  
  rownames(yh) <- play_by_play_data$key
  rownames(ya) <- play_by_play_data$key
  
  y <- rbind(yh, ya) # as with x, home on offense, then away on offense
  colnames(y) <- c("goal", "shot", "xg")
  
  # Create the weights vector
  w <- play_by_play_data %>% select(sec) %>% as.matrix()
  w <- rbind(w, w) # need two, one for home on offense, one for away on offense
  
  # Create the weighted versions of design matrix and response vector
  w.half <- Diagonal(n = length(as.numeric(w)), 
                     x = sqrt(as.numeric(w)))
  xw <- w.half %*% x 
  yw <- w.half %*% y
  yw <- as.matrix(yw)
  
  # Center y
  # y[, "goal"] <- y[, "goal"] - mean(y[, "goal"])
  # y[, "shot"] <- y[, "shot"] - mean(y[, "shot"])
  # y[, "xg"] <- y[, "xg"] - mean(y[, "xg"])
  # yw[, "goal"] <- yw[, "goal"] - mean(yw[, "goal"])
  # yw[, "shot"] <- yw[, "shot"] - mean(yw[, "shot"])
  # yw[, "xg"] <- yw[, "xg"] - mean(yw[, "xg"])
  
  # Return a list containing the design matrix, response vector, weights, 
  # and weighted versions of the design matrix and response vector
  return(list(x = x, y = y, w = w, xw = xw, yw = yw))
}