library(tidyverse)
library(ggplot2)
library(ggthemes)

##Load Schedule, filter for playoffs, grab QBs I broke this into two 
#data frames, one for home qbs and one for away qbs, then joined together.
schedule <- nflreadr::load_schedules()
home_playoff_qbs <- schedule %>%
  filter(game_type != 'REG') %>%
  select(qb_id = home_qb_id, qb_name = home_qb_name)

playoff_qbs <- schedule %>%
  filter(game_type != 'REG') %>%
  select(qb_id = away_qb_id, qb_name = away_qb_name) %>%
  rbind(home_playoff_qbs) %>%
  group_by(qb_id, qb_name) %>%
  summarise(tot = n()) %>%
  filter(tot > 0) %>%
  mutate(tot = NULL)

#Load player stats
player_stats <- nflreadr::load_player_stats(1999:2021) 

#Get all QBs cumulative stats through their first 30 games
#to complare with Tua
qbs <- player_stats %>%
  filter(season_type == 'REG') %>%
  select(player_id, player_name, completions:sack_yards) %>%
  left_join(playoff_qbs, c('player_id' = 'qb_id')) %>%
  filter(!is.na(qb_name) | player_name == 'T.Tagovailoa') %>%
  group_by(player_id, player_name) %>%
  filter(attempts > 1) %>%
  mutate(game_num = 1) %>%
  mutate(across(c(completions:sack_yards,game_num), ~cumsum(.x))) %>%
  mutate(anya = (passing_yards + 20*passing_tds - 45*interceptions - sack_yards)/
                 (attempts + sacks)) %>%
  filter(game_num <31) %>%
  ungroup()

#Set my guy to Tua
#His ID number is '00-0036212'
my_guy <- 'T.Tagovailoa'

#Plot
qbs %>% ggplot(aes(x = game_num, y = anya)) +
  geom_point(color = 'grey', size = 2) +
  geom_point(data = qbs %>% filter(player_name == my_guy),
             color = 'orange', fill = '#008E97',
             size = 4, shape = 21) +
  scale_x_continuous(limits = c(.5,30.5),
                     expand = c(0,0),
                     breaks = c(1,10,20,30),
                     labels = c(1,10,20,30)) +
  labs(title = 'Tua Tagovailoa vs Playoff QBs',
       y = 'Cumulative ANY/A',
       x = 'Games Played',
       caption = 'Figure: @Timboslice003 | Data: NFLverse') + 
  theme_bw() +
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = .5),
        axis.title = element_text(size = 18, face = 'bold'),
        axis.text = element_text(size = 14),
        plot.background = element_rect(fill = 'gray93'),
        plot.margin = margin(t = 10, b = 5, l = 10, r = 30),
        plot.caption =  element_text(size = 15, face = 'bold')) +
  nflplotR::geom_nfl_logos(data = data.frame(x = 15, y = 20),
                           aes(x, y, team_abbr = 'MIA'), height = .7) +
  nflplotR::geom_nfl_headshots(data = data.frame(x = 23, y = 20),
                           aes(x, y,player_gsis = '00-0036212'), height = .5)

#Save
#ggsave('Day_24_Theme_Day.png', width = 17, height = 10, dpi = 'retina')
