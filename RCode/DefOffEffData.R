#Exploring Def vs Off Effeciency of NFL Teams Through Week 15

#Libs
library(tidyverse)
library(nflplotR)

#Importing
pfrTeamDef <- read_csv("~/RStudio/NFL/Datafiles/pfrTeamDef15.csv")
pfrTeamOff <- read_csv("~/RStudio/NFL/Datafiles/nflTeamOff15.csv")

#Cleaning + Tidying
pfrTeamDefSel <- pfrTeamDef %>% select(Tm, G, PA)
pfrTeamOffSel <- pfrTeamOff %>% select(Tm, G, PF)
pfrTeamJoin <- pfrTeamDefSel %>% left_join(pfrTeamOffSel, by = c("Tm", "G")) %>% 
  mutate(pa_per_game = round(PA / G, 1)) %>% mutate(pf_per_game = round(PF / G, 1))

#Convert team names to Abbr
team_abbr <- data.frame(
  team = c("Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills", 
           "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", "Cleveland Browns",
           "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers",
           "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs",
           "Las Vegas Raiders", "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins",
           "Minnesota Vikings", "New England Patriots", "New Orleans Saints", "New York Giants",
           "New York Jets", "Philadelphia Eagles", "Pittsburgh Steelers", "San Francisco 49ers",
           "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Commanders"),

  abbr = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", 
           "HOU", "IND", "JAX", "KC", "LV", "LAC", "LAR", "MIA", "MIN", "NE", "NO", "NYG", "NYJ",
           "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS")
)

#Joining abbr with main
pfrAbbrJoin <- pfrTeamJoin %>% rename("team" = "Tm") %>% left_join(team_abbr, by = c("team"))

pfrAbbrJoin$pf_per_game <- as.numeric(pfrAbbrJoin$pf_per_game)
pfrAbbrJoin$pa_per_game <- as.numeric(pfrAbbrJoin$pa_per_game)

#removing NAs
pfrAbbrJoin2 <- pfrAbbrJoin %>% filter(!is.na(team))

#Viz
PFvsPA <- ggplot(pfrAbbrJoin2, aes(x = pa_per_game, y = pf_per_game)) +
  geom_mean_lines(aes(x0 = pa_per_game, y0 = pf_per_game)) +
  geom_nfl_logos(aes(team_abbr = abbr), width = 0.05) + theme_classic() +
  labs(title = "Points Scored vs Points Allowed", subtitle = "Through NFL Week 15 2023-2024 Season", x = "Points Allowed per Game", 
       y = "Points Scored per Game", 
       caption = "Source: ProFootballReference, Creator: Parker Kuchulan")
print(PFvsPA)
