#This code plots a bar chart of the 3 different types of QBs to start the
#Superbowl since 2011: 1) QBs on their Rookie Deal 2) Tom Brady 3) Other
#Need 2 picture files in your directory

library(tidyverse)
library(ggplot2)
library(ggimage)

#Load teams, schedules, players, NFL Draft logo
team_info <- nflreadr::load_teams()
schedules <- nflreadr::load_schedules(2011:2021)
player_info <- nflreadr::load_ff_playerids()

#Get Superbowl Team
sbteams <- schedules %>%
  filter(game_type == 'SB') %>%
  select(season, home_team, away_team) %>%
  pivot_longer(c(home_team, away_team)) %>%
  select(season, value) %>%
  rename(tm = value)

#Load stats. This will give us QB stats with corresponding season
stats <-nflreadr::load_player_stats(seasons = 2011:2021 ,stat_type = 'offense')

#Join stats with player info. Use Season and Draft year to determine years
#of experience for each QB. We want to know those on rookie deals, which means
#they have less than 5 years of experience
qbstats <- stats %>%
  group_by(player_id, player_name, season, recent_team,) %>%
  summarise(yards = sum(passing_yards)) %>%
  filter(yards > 2000) %>% 
  left_join(player_info, by = c('player_id' = 'gsis_id')) %>%
  select(player_id, player_name, season, recent_team, draft_year) %>%
  mutate(exp = season - draft_year,
         rookie_deal = ifelse(exp < 5, 1, 0))

#Join SB QBs with the qbstats to get SB QBs who were on their rookie deal
qbs_in_sb <- sbteams %>%
  left_join(qbstats, by = c('season', 'tm' = 'recent_team')) %>%
  mutate(category = case_when(rookie_deal == 1 ~ 'On Rookie Deal',
                              player_name == 'T.Brady' ~ 'Tom Brady',
                              TRUE ~ 'Other')) %>%
  left_join(team_info, by = c('tm' = 'team_abbr'))

#Data should be ready to go. By looking at data, you can see there were 10 QBS
#on their rookie deals, 6 who were Tom Brady (lol) and 6 other QBS
#Colors chosen manually
#Tom Brady's ID number is 00-0019596
myplot <- qbs_in_sb %>% ggplot(aes(x = 1, fill = as.factor(category))) +
  geom_bar(width = 1, color = 'white') +
  coord_polar('y') +
  scale_fill_manual(values = c('blue','#322F2B','#A71930')) +
  theme_void() + 
  theme(plot.background = element_rect(color = 'midnightblue', fill = 'black'),
        legend.position = 'none',
        plot.title = element_text(color = 'black', size = 32,
                                  face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = 'black', size = 18,
                                     face = 'bold', hjust = .5),
        plot.caption = element_text(color = 'white', size = 20, 
                                    face = 'bold', hjust = .1, vjust = 10)) +
  ggimage::geom_image(aes(x = 1.2, y = 2.75, image = team_league_logo[1]), size = .15)+
  annotate(geom = 'text', x = 1.05, y = 4.25, label = 'Other (6/22)', size = 5, color = 'grey') +
  nflplotR::geom_nfl_headshots(aes(x = 1, y =8.5, player_gsis = '00-0019596'), width = .23) +
  annotate(geom = 'text', x = 1.23, y = 9.3, label = 'Tom Brady (6/22)', size = 5, color = 'grey') +
  annotate(geom = 'text', x = 1, y = 16.8, label = 'Rookie Deal QBs (10/22)', size = 5, color = 'grey') +
  ggimage::geom_image(aes(x = 1.1, y = 18.5, image = 'NFL_Draft_logo.png'), size = .2)+
  labs(title = 'Starting Quarterbacks in the Super Bowl',
       subtitle = 'From 2011 through 2021',
       caption = '@Timboslice003 | NFLfastR')

#Add Stadium Background
ggbackground(myplot ,background =  'SoFi.jpg',)

#Save
#ggsave('Day_1_Part_to_Whole.png', width = 14, height = 10, dpi = 'retina')
