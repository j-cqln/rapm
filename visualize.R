# Goals, assists vs total xG, playmaking score histogram
dg <- ranked_players %>%
  rename(value = assists) # switch based on variable

title <- "Distribution of 5-on-5 assists" 
g <- ggplot(dg, 
            aes(x = value)) +
  geom_histogram(fill = pubblue, 
                 color = pubbackgray) +
  labs(title = title,
       subtitle = "2022-2023 season",
       x = "Assists",
       y = "Count")

g %>%
  pub(type = "hist") +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))

gg <- g %>% 
  pub(type = "hist", base_size = 36) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))

ggsave(filename = paste0("img/", gsub("%", " Perc", title), ".jpg"), 
       plot = gg,
       width = 20, # do not change
       height = 20, # can change from 20 if desired
       units = "in", # do not change
       dpi = 72) # do not change

# Visualize coefficients before/after collapse
all_data <- readRDS("data/apm_data_collapse_off_FALSE_collapse_def_FALSE_from_league_avg_TRUE.rds")
ols <- readRDS("models/collapse_off_FALSE_collapse_def_FALSE_from_league_avg_TRUE_ols.rds")
wols <- readRDS("models/collapse_off_FALSE_collapse_def_FALSE_from_league_avg_TRUE_wols.rds")
lasso <- readRDS("models/collapse_off_FALSE_collapse_def_FALSE_from_league_avg_TRUE_lasso.rds")
ridge <- readRDS("models/collapse_off_FALSE_collapse_def_FALSE_from_league_avg_TRUE_ridge.rds")

collapse_off_data <- readRDS("data/apm_data_collapse_off_TRUE_collapse_def_FALSE_from_league_avg_TRUE.rds")
ols_collapse_off <- readRDS("models/collapse_off_TRUE_collapse_def_FALSE_from_league_avg_TRUE_ols.rds")
wols_collapse_off <- readRDS("models/collapse_off_TRUE_collapse_def_FALSE_from_league_avg_TRUE_wols.rds")
lasso_collapse_off <- readRDS("models/collapse_off_TRUE_collapse_def_FALSE_from_league_avg_TRUE_lasso.rds")
ridge_collapse_off <- readRDS("models/collapse_off_TRUE_collapse_def_FALSE_from_league_avg_TRUE_ridge.rds")

collapse_def_data <- readRDS("data/apm_data_collapse_off_FALSE_collapse_def_TRUE_from_league_avg_TRUE.rds")
ols_collapse_def <- readRDS("models/collapse_off_FALSE_collapse_def_TRUE_from_league_avg_TRUE_ols.rds")
wols_collapse_def <- readRDS("models/collapse_off_FALSE_collapse_def_TRUE_from_league_avg_TRUE_wols.rds")
lasso_collapse_def <- readRDS("models/collapse_off_FALSE_collapse_def_TRUE_from_league_avg_TRUE_lasso.rds")
ridge_collapse_def <- readRDS("models/collapse_off_FALSE_collapse_def_TRUE_from_league_avg_TRUE_ridge.rds")

player_coefs <- extract.player.coefs(all_data, ols, wols, ridge, lasso, NULL)

player_coefs_collapse_off <- extract.player.coefs(collapse_off_data,
                                                  ols_collapse_off,
                                                  wols_collapse_off,
                                                  ridge_collapse_off,
                                                  lasso_collapse_off,
                                                  NULL,
                                                  collapsed = TRUE,
                                                  which_collapsed = "off")

player_coefs_collapse_def <- extract.player.coefs(collapse_def_data,
                                                  ols_collapse_def,
                                                  wols_collapse_def,
                                                  ridge_collapse_def,
                                                  lasso_collapse_def,
                                                  NULL,
                                                  collapsed = TRUE,
                                                  which_collapsed = "def")

# collapsed/not for all models
coefs_off <- data.frame(
  ols_off = player_coefs$ols_off,
  wols_off = player_coefs$wols_off,
  ridge_off = player_coefs$ridge_off,
  lasso_off = player_coefs$lasso_off,
  ols_off_collapse_def = player_coefs_collapse_def$ols_off,
  wols_off_collapse_def = player_coefs_collapse_def$wols_off,
  ridge_off_collapse_def = player_coefs_collapse_def$ridge_off,
  lasso_off_collapse_def = player_coefs_collapse_def$lasso_off
)

coefs_def <- data.frame(
  ols_def = player_coefs$ols_def,
  wols_def = player_coefs$wols_def,
  ridge_def = player_coefs$ridge_def,
  lasso_def = player_coefs$lasso_def,
  ols_def_collapse_off = player_coefs_collapse_off$ols_def,
  wols_def_collapse_off = player_coefs_collapse_off$wols_def,
  ridge_def_collapse_off = player_coefs_collapse_off$ridge_def,
  lasso_def_collapse_off = player_coefs_collapse_off$lasso_def
)

corr <- cor(coefs_off)
# corr <- cor(coefs_def)

dg <- corr %>%
  as.data.frame() %>%
  # longer format
  rownames_to_column(var = "x") %>%
  pivot_longer(cols = -x, 
               names_to = "y", 
               values_to = "value") %>%
  # order them however you"d like
  mutate(x = factor(x, levels = rev(sort(unique(x)))), 
         y = factor(y, levels = sort(unique(y))))

title <- "Correlation of offense coefficients from different models"
g <- ggplot(dg) + 
  geom_tile(aes(x = x, 
                y = y, 
                fill = value),
            linewidth = 0.4, 
            show.legend = T, 
            color = pubdarkgray) +
  scale_fill_gradientn(colors = c("red4", 
                                  pubred, 
                                  publightred, 
                                  "white", # or "pubbackgray"
                                  publightblue, 
                                  pubblue, 
                                  "navy"),
                       na.value = pubmediumgray, # same color as below
                       oob = squish,
                       breaks = c(-1, 0, 1),
                       limits = c(-1, 1)) +
  labs(title = title,
       subtitle = "Not collapsed vs. collapsed defense design matrix",
       x = "Models",
       y = NULL,
       fill = "Correlation")

g <- g %>% 
  pub(type = "grid") + 
  theme(axis.text.x.top = element_text(angle = 90, 
                                       hjust = 0, 
                                       vjust = 0.3),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))

gg <- g %>%
  pub(type = "grid", 
      base_size = 36) + 
  theme(axis.text.x.top = element_text(angle = 90, 
                                       hjust = 0, 
                                       vjust = 0.3),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))

ggsave(filename = paste0("img/", gsub("%", " Perc", title), ".jpg"), 
       plot = gg,
       width = 20,   # do not change
       height = 24,
       units = "in", # do not change
       dpi = 72)   # do not change

# Visualize a subset of coefficients
dg1 <- data.frame(model1 = player_coefs$ridge_off,
                 model2 = player_coefs_collapse_def$ridge_off) %>%
  mutate(name = "Uncollapsed vs. collapsed defense",
         type = "Offense")

dg2 <- data.frame(model1 = player_coefs$ridge_def,
                  model2 = player_coefs_collapse_off$ridge_def) %>%
  mutate(name = "Uncollapsed vs. collapsed offense",
         type = "Defense")

dg <- rbind(dg1, dg2)

title <- "Ridge coefficients before and after collapsing design matrix"
g <- ggplot(dg, aes(x = model1,
                    y = model2,
                    color = type)) +
  geom_point() +
  facet_wrap(~ name, nrow = 1) +
  labs(title = title,
       subtitle = "Compare offense before and after collapsing defense, vice versa for defense",
       color = "Coefficient type",
       x = "Uncollapsed",
       y = "Collapsed") +
  guides(color = guide_legend(nrow = 1))

g %>% 
  pub(xlim = c(-1, 1), 
      ylim = c(-2, 2),
      facet = TRUE) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))

# Save to a file using base_size = 36
gg <- g %>%
  pub(xlim = c(-1, 1), 
      ylim = c(-2, 2),
      facet = TRUE, 
      base_size = 36) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))

ggsave(filename = paste0("img/", gsub("%", " Perc", title), ".jpg"),
       plot = gg,
       width = 24, # do not change
       height = 16,
       units = "in", # do not change
       dpi = 72) # do not change

# Visualize coefficients ridge vs. boxscore vs. pseudo-bayes
coefs_off <- ranked_players %>%
  select(ridge_off,
         boxscore_based_off, pseudo_bayes_off,
         boxscore_based_off_alternate, pseudo_bayes_off_alternate)

coefs_def <- ranked_players %>%
  select(ridge_def,
         boxscore_based_def, pseudo_bayes_def,
         boxscore_based_def_alternate, pseudo_bayes_def_alternate)

corr <- cor(coefs_off)
# corr <- cor(coefs_def)

dg <- corr %>%
  as.data.frame() %>%
  # longer format
  rownames_to_column(var = "x") %>%
  pivot_longer(cols = -x, 
               names_to = "y", 
               values_to = "value") %>%
  # order them however you"d like
  mutate(x = factor(x, levels = rev(sort(unique(x)))), 
         y = factor(y, levels = sort(unique(y))))

title <- "Correlation of offense coefficients from different models"
g <- ggplot(dg) + 
  geom_tile(aes(x = x, 
                y = y, 
                fill = value),
            linewidth = 0.4, 
            show.legend = T, 
            color = pubdarkgray) +
  scale_fill_gradientn(colors = c("red4", 
                                  pubred, 
                                  publightred, 
                                  "white", # or "pubbackgray"
                                  publightblue, 
                                  pubblue, 
                                  "navy"),
                       na.value = pubmediumgray, # same color as below
                       oob = squish,
                       breaks = c(-1, 0, 1),
                       limits = c(-1, 1)) +
  labs(title = title,
       subtitle = "Ridge, box score, and pseudo-Bayes models",
       x = "Models",
       y = NULL,
       fill = "Correlation")

g <- g %>% 
  pub(type = "grid") + 
  theme(axis.text.x.top = element_text(angle = 90, 
                                       hjust = 0, 
                                       vjust = 0.3),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))

gg <- g %>%
  pub(type = "grid", 
      base_size = 36) + 
  theme(axis.text.x.top = element_text(angle = 90, 
                                       hjust = 0, 
                                       vjust = 0.3),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))

ggsave(filename = paste0("img/", gsub("%", " Perc", title), ".jpg"), 
       plot = gg,
       width = 20, # do not change
       height = 24,
       units = "in", # do not change
       dpi = 72) # do not change
