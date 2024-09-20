library(nflreadr)
library(nflplotR)
library(tidyverse)
library(plotly)

pbp <- nflreadr::load_participation(include_pbp = TRUE)

pass_rush_epa <- pbp %>% filter(pass == 1) %>% group_by(defteam) %>% 
  summarize(plays = n(), epa = mean(epa, na.rm=T), 
            avg_pass_rushers = round(mean(number_of_pass_rushers, na.rm=T), 3))

#EPA plot
ggplot(pass_rush_epa, aes(x = avg_pass_rushers, y = epa)) +
  geom_mean_lines(aes(x0 = avg_pass_rushers, y0 = epa)) +
  geom_nfl_logos(aes(team_abbr = defteam), width = 0.05) + theme_classic() +
  labs(title = "EPA/play on Average Amount of Pass Rushers", subtitle = "Through 2023 Week 12", 
       caption = "Data Source: nflreadr", x = "Average Amount of Pass Rushers", 
       y = "EPA/play")

#Dropback pass plot
dropback_pass_yards <- pbp %>% filter(pass == 1, qb_dropback == 1) %>% group_by(passer_player_name, passer_id) %>% 
  summarize(avg_pass_yards_per_dropback = round(mean(passing_yards, na.rm=T), 3), dropbacks = n(), avg_dropbacks_per_game = round(dropbacks/12, 3)) %>% 
  filter(dropbacks > 100, !is.na(passer_player_name)) 

dropback_plot <- ggplot(dropback_pass_yards, aes(x = avg_dropbacks_per_game, y = avg_pass_yards_per_dropback, label = passer_player_name)) +
  geom_mean_lines(aes(x0 = avg_dropbacks_per_game, y0 = avg_pass_yards_per_dropback)) +
  geom_nfl_headshots(aes(player_gsis = passer_id), width = 0.075) + theme_classic() +
  labs(title = "Pass Yards/Dropback on Average Dropbacks per Game", subtitle = "Through 2023 Week 12", x = "Average Dropbacks/Game", y = "Average Passing Yards/Dropback", 
       caption = "Data Source: nflreadr  Creator: Parker Kuchulan")

print(dropback_plot)
ggplotly(dropback_plot, tooltip = c("passer_player_name"))



dropback_scatter <- ggplot(dropback_pass_yards, aes(x = avg_dropbacks_per_game, y = avg_pass_yards_per_dropback, label = passer_player_name)) + 
  geom_mean_lines(aes(x0 = avg_dropbacks_per_game, y0 = avg_pass_yards_per_dropback)) + geom_point() +
  theme_classic() +
  labs(title = "Pass Yards/Dropback on Average Dropbacks per Game", subtitle = "Through 2023 Week 12", x = "Average Dropbacks/Game", y = "Average Passing Yards/Dropback", 
       caption = "Data Source: nflreadr  Creator: Parker Kuchulan")

print(dropback_scatter)

ggplotly(dropback_scatter, tooltip = c("passer_player_name"))

dropback_df2 <- dropback_pass_yards %>% rename( "QB" = "passer_player_name")
  
dropback_scatter2 <- ggplot(dropback_df2, aes(x = avg_dropbacks_per_game, y = avg_pass_yards_per_dropback, label = QB)) + 
  geom_hline(yintercept = mean(dropback_pass_yards$avg_pass_yards_per_dropback), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(dropback_pass_yards$avg_dropbacks_per_game), linetype = "dashed", color = "red") +
  geom_point() +
  theme_classic() +
  labs(title = "Pass Yards/Dropback on Average Dropbacks per Game", subtitle = "Through 2023 Week 12", x = "Average Dropbacks/Game", y = "Average Passing Yards/Dropback", 
       caption = "Data Source: nflreadr  Creator: Parker Kuchulan")
 

print(dropback_scatter2)

interactive_dropback <- ggplotly(dropback_scatter2, tooltip = c("QB"))
print(interactive_dropback)

htmlwidgets::saveWidget(interactive_dropback, file = "interactive_plot.html")




