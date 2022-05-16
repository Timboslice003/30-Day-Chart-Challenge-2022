#This code gathers all NFL playoff matchups since 2010 and plots them
#as a network graph illustrating the number of times teams have played
#each other in the playoffs

library(tidyverse)
library(ggplot2)
library(ggraph)
library(data.table)


#Load Schedule
schedules <- nflfastR::fast_scraper_schedules(2010:2021)

#Get Playoff Match ups and clean up team abbreviations for relocated teams
#SD -> LAC, STL -> LA, OAK -> LV
playoff_matchups <- schedules %>%
  filter(game_type != 'REG') %>%
  select(home_team, away_team) %>%
  mutate(away_team = ifelse(away_team == 'SD', 'LAC', away_team)) %>%
  mutate(away_team = ifelse(away_team == 'STL', 'LA', away_team)) %>%
  mutate(away_team = ifelse(away_team == 'OAK', 'LV', away_team)) %>%
  mutate(home_team = ifelse(home_team == 'SD', 'LAC', home_team)) %>%
  mutate(home_team = ifelse(home_team == 'STL', 'LA', home_team)) %>%
  mutate(home_team = ifelse(home_team == 'OAK', 'LV', home_team))

#Sort the matchups so home and away does not matter and
#store in a graph
teams_sorted <- t(apply(playoff_matchups,1,sort)) %>%
  data.table() %>%
  rename('Team1' = V1, 'Team2' = V2) %>%
  group_by(Team1, Team2) %>%
  summarise(Games = n())

#Plot as a network diagram
teams_sorted %>%
  ggraph::ggraph(layout = 'linear', circular = T) +
  geom_edge_arc(aes(edge_alpha = Games, edge_width = Games), color = "red") +
  nflplotR::geom_nfl_logos(aes(x = x, y = y, team_abbr = name, height = .1)) +
  labs(title = 'Every NFL Playoff Matchup',
       subtitle = 'Since the 2010 NFL Season',
       caption = 'Graph: @Timboslice003 | Data: NFLverse',
       edge_alpha = '# of Games\nBetween Teams',
       edge_width = '# of Games\nBetween Teams',) +
  theme(plot.background = element_rect(color = 'black', fill = 'black'),
        panel.background = element_rect(color = 'black', fill = 'black'),
        panel.grid.major.x = element_blank(),
        legend.title = element_text(colour="white", size=16, face="bold"),
        legend.text = element_text(colour="white", size=16, face="bold"),
        legend.background = element_rect(color = 'black', fill = 'black'),
        plot.title = element_text(color = 'white', size = 24, face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = 'white', size = 14, face = 'bold', hjust = .5),
        plot.caption = element_text(color = 'white', size = 18, face = 'bold'))

#Save
#ggsave('Day_4_Flora.png', width = 14, height = 10, dpi = 'retina')
