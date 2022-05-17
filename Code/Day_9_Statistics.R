#This code plots a distribution of NFL player ages for the
#2021 NFL season

library(tidyverse)
library(ggplot2)

#Load Rosters, teams for NFL logo, and snapcounts
rosters <- nflreadr::load_rosters(2021)
tmcol <- nflreadr::load_teams()
snap_counts <- nflreadr::load_snap_counts()

#group by player id to get all players who played a snap
#merge with rosters to get age. Use firt NFL game as 
#starting date
players_list <- snap_counts %>%
  mutate(total = offense_snaps + defense_snaps + st_snaps) %>%
  group_by(player, pfr_player_id) %>%
  summarise(snaps = sum(total)) %>%
  left_join(rosters, by = c('pfr_player_id' = 'pfr_id')) %>%
  filter(!is.na(birth_date)) %>%
  mutate(age = round(as.numeric(((as.Date('2021-09-09')) - birth_date))/365,2)) %>%
  select(player, birth_date, age)

#Plot
players_list %>% ggplot(aes(x = age, ..count..)) +
  geom_density(fill = 'navy', color = 'red', size = 1) +
  scale_x_continuous(labels = 20:45, breaks = 20:45) +
  annotate(
    nflplotR::GeomFromPath,
    y = 180,
    x = 35,
    height = .75,
    path = tmcol$team_league_logo[1]) +
  annotate("segment", x = 41, xend = 44, y = 100, yend = 6,
           colour = "red", size = 2, arrow = arrow()) +
  annotate(geom="text", x=41.2, y=112, label="Tom Brady", color="red", size = 8) +
  labs(y = 'Number of Players',
       x = 'Age',
       title = 'Distribution of NFL Player Age',
       subtitle = 'Age as of 9/9/2021',
       caption = 'Graph: @Timboslice003 | Data: NFLverse') +
  theme(plot.background = element_rect('navy'),
        panel.grid.major=element_line(color="navy"),
        panel.grid.minor=element_line(color="navy"),
        panel.grid.minor.y =element_blank(),
        plot.title = element_text(color = 'grey', size = 24,
                                  face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = 'grey', size = 14,
                                     hjust = .5, face = 'bold'),
        plot.caption = element_text(color = 'grey', size = 14, face = 'bold'),
        axis.title = element_text(color = 'grey', size = 16),
        axis.text = element_text(color = 'grey', size = 12))

#Save
#ggsave('Day_9_Statistics.png', width = 14, height = 10, dpi = 'retina')
