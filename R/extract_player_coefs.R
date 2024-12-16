extract.player.coefs <- function(data,
                                 ols_model,
                                 wols_model,
                                 ridge_model,
                                 lasso_model,
                                 pois_model,
                                 collapsed = FALSE,
                                 which_collapsed = "def"
                                 ) {

  which_player = which(grepl("_def", colnames(data$xw)) | grepl("_off", colnames(data$xw)))
  n_player = length(which_player)
  temp_names = unlist(str_split(colnames(data$xw)[which_player], "_"))
  player_name = temp_names[0:(n_player - 1) * 2 + 1]
  off_def = temp_names[(1:n_player) * 2]
  
  # Minutes
  mins = diag(t(data$xw) %*% data$xw) / 60
  mins = mins[grepl("off|def", colnames(data$xw))]

  coefs_rapm <- data.frame(player_name, mins, off_def) %>%
    mutate(ols = coef(ols_model)[which_player],
           wols = coef(wols_model)[which_player],
           ridge = coef(ridge_model, s = "lambda.min")[which_player],
           lasso = coef(lasso_model, s = "lambda.min")[which_player],
           pois = coef(pois_model, s = "lambda.min")[which_player])
  
  if(!"ols" %in% colnames(coefs_rapm)) {
    coefs_rapm$ols = NA
  }
  
  if(!"wols" %in% colnames(coefs_rapm)) {
    coefs_rapm$wols = NA
  }
  
  if(!"ridge" %in% colnames(coefs_rapm)) {
    coefs_rapm$ridge = NA
  }
  
  if(!"lasso" %in% colnames(coefs_rapm)) {
    coefs_rapm$lasso = NA
  }
  
  if(!"pois" %in% colnames(coefs_rapm)) {
    coefs_rapm$pois = NA
  }
  
  coefs_rapm <- coefs_rapm %>%
    pivot_wider(names_from = off_def,
                values_from = c("ols", "wols", "ridge", "lasso", "pois")) %>%
    mutate(ols_def = -1 * ols_def,
           wols_def = -1 * wols_def,
           ridge_def = -1 * ridge_def,
           lasso_def = -1 * lasso_def,
           pois_def = -1 * pois_def)

  # Center the coefficients so that the weighted mean is 0
  coefs_rapm = coefs_rapm %>%
    mutate(ols_off = ols_off - weighted.mean(x = ols_off, w = mins, na.rm = TRUE),
           ols_def = ols_def - weighted.mean(x = ols_def, w = mins, na.rm = TRUE),
           wols_off = wols_off - weighted.mean(x = wols_off, w = mins, na.rm = TRUE),
           wols_def = wols_def - weighted.mean(x = wols_def, w = mins, na.rm = TRUE),
           ridge_off = ridge_off - weighted.mean(x = ridge_off, w = mins, na.rm = TRUE),
           ridge_def = ridge_def - weighted.mean(x = ridge_def, w = mins, na.rm = TRUE),
           lasso_off = lasso_off - weighted.mean(x = lasso_off, w = mins, na.rm = TRUE),
           lasso_def = lasso_def - weighted.mean(x = lasso_def, w = mins, na.rm = TRUE),
           pois_off = pois_off - weighted.mean(x = pois_off, w = log(mins * 60), na.rm = TRUE),
           pois_def = pois_def - weighted.mean(x = pois_def, w = log(mins * 60), na.rm = TRUE))

  if (collapsed) {
    if (which_collapsed == "off") {
      coefs_rapm = coefs_rapm %>%
        select(player_name, mins, ols_def, wols_def, ridge_def, lasso_def, pois_def)
    } else if (which_collapsed == "def") {
      coefs_rapm = coefs_rapm %>%
        select(player_name, mins, ols_off, wols_off, ridge_off, lasso_off, pois_off)
    }
    
    coefs_rapm <- coefs_rapm %>% filter(player_name != "collapsed")
  }

  return(coefs_rapm)
}
