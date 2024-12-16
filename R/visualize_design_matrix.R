## visualize.design.matrix.r
library(pubtheme)

## Choose one game
df <- X[grepl("2022020001", rownames(X)),]

## Remove players not in that game
df <- df[, colSums(df) != 0]

dim(df)
head(df)

## How many shifts do we want to show? 
n.shifts <- 30

## Find the row where away starts
## Find the col where def starts
row2 <- nrow(df) / 2 + 1
col.off <- grep("off", colnames(df))[1]
col.def <- grep("def", colnames(df))[1]

## Take the first n.shift shifts
home <- df[1:n.shifts, ]
away <- df[row2:(row2 + n.shifts - 1), ]

## Find which players are the home players and away players
home_players <- pbp %>%
  filter(grepl("2022020001", key)) %>%
  select(home_on_1:home_on_6) %>%
  pivot_longer(cols = everything()) %>%
  pull(value) %>%
  unique()

home_players

away_players <- pbp %>%
  filter(grepl("2022020001", key)) %>%
  select(away_on_1:away_on_6) %>%
  pivot_longer(cols = everything()) %>%
  pull(value) %>%
  unique()

away_players

dg <- rbind(home, away) %>%
  as.matrix() %>%
  as.data.frame() %>%
  # rownames_to_column("shift.id") %>%
  mutate(shift.id = rep(1:n.shifts, 2), 
         ha = ifelse(home == 1, "Home", "Away")) %>%
  pivot_longer(cols = c(-shift.id, -ha)) %>%
  mutate(group = case_when(grepl("off", name) ~ "Offense", 
                           grepl("def", name) ~ "Defense",
                           TRUE ~ ""), 
         name = gsub("_.+", "", name), 
         name = factor(name, levels = c("home", "down3", "down2", "down1", 
                                        "tie", "up1", "up2", "up3", 
                                        home_players, 
                                        away_players)), 
         fill = case_when(name %in% home_players ~ "Home", 
                          name %in% away_players ~ "Away",
                          TRUE ~ "Indicator"),
         group = paste(fill, group),
         group = gsub("Indicator ", "Indicator", group),
         group = factor(group, levels = c("Indicator", 
                                          "Home Offense", 
                                          "Away Offense",
                                          "Home Defense",
                                          "Away Defense")),
         value = as.character(value),
         ha = factor(ha, levels = c("Home", "Away")), 
         shift.id = factor(shift.id, levels = rev(unique(shift.id))),
         fill = paste0(fill, " Player is 1"),
         fill = ifelse(fill == "Indicator Player is 1", "Indicator is 1", fill),
         fill = ifelse(value == 0, "0", fill), 
         fill = factor(fill, 
                       levels = c("Home Player is 1", 
                                  "Away Player is 1", 
                                  "Indicator is 1",
                                  "0"))) %>%
  select(shift.id, name, value, group, fill, ha)

head(dg)
tail(dg)

title <- "Snippet of a design matrix for one game" 
g <- ggplot(dg, 
           aes(x = name, 
               y = shift.id, 
               fill = fill)) +
  geom_tile(linewidth = 0.4, 
            show.legend = TRUE, 
            color = pubdarkgray) + 
  facet_grid(ha ~ group,        ## Now faceting by division
             scales = "free") + ## Show only the relevant teams in each facet
  scale_fill_manual(values = c(pubblue, 
                               pubred, 
                               pubmediumgray, 
                               pubbackgray)) +   
  labs(title = title,
       x = "Predictors", 
       y = NULL, 
       fill = NULL) 

gg <- g %>%
  pub(type = "grid", 
      base_size = 10, 
      ybreaks = seq(1, n.shifts, by = 2)) + ## Smaller text so names fit better
  theme(axis.text.x.top = element_text(angle = 90, 
                                       vjust = .5, 
                                       hjust = 0, 
                                       size  = 5),
        axis.text.y = element_text(size = 6),
        ## we can adjust the panel spacing a little bit, if desired, and 
        ## makes the legend look like squares instead of rectangles, if desired
        panel.spacing     = unit(2/72*10/4, "in"), 
        legend.key.width  = unit(1/72*36/4, "in"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))

print(gg)

## Save the image
gg <- g %>%
  pub(type = "grid",
      base_size = 10*3,
      ybreaks = seq(1, n.shifts, by = 2)) + ## Smaller text so names fit better
  theme(axis.text.x.top = element_text(angle = 90,
                                       vjust = .5,
                                       hjust = 0,
                                       size  = 5*3),
        axis.text.y = element_text(size = 6*3),
        ## we can adjust the panel spacing a little bit, if desired, and
        ## makes the legend look like squares instead of rectangles, if desired
        panel.spacing     = unit(2/72*10/4*3, "in"),
        legend.key.width  = unit(1/72*36/4*3, "in"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))

ggsave(filename = paste0("img/", gsub("%", " Perc", title), ".jpg"),
       plot   = gg,
       width  = 20,   ## do not change
       height = 20,   ## can change from 20 if desired
       units  = "in", ## do not change
       dpi    = 72)   ## do not change