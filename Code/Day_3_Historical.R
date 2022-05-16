#This code gathers NFL data since 2011 and plots the total final score by year
#as a visual aid to see points scored over the years

library(tidyverse)
library(ggplot2)

#Load play by play data
pbp <- nflreadr::load_pbp(2010:2021)

#Filter for only regular season games
games <- pbp %>%
  filter(season_type == 'REG') %>%
  group_by(game_id, season) %>%
  summarise(score = mean(total))

#Get average of each season
mean_games <- games %>%
  group_by(season) %>%
  summarise(avg_score = mean(score))

#Plot
games %>% ggplot(aes(x = season, y = score)) +
  geom_jitter(width = .2, aes(color = score)) +
  scale_color_gradient(low = c('red', 'black'), 
                       high = 'green', guide = 'none') +
  scale_x_continuous(labels = games$season, breaks = games$season) +
  geom_path(data = mean_games,aes(x = season, y = avg_score),
              size = 2.5) +
  theme(panel.background = element_rect('bisque'),
        plot.background = element_rect('darkblue'),
        panel.grid.major=element_line(colour="darkblue"),
        panel.grid.minor=element_line(colour="darkblue"),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(color = 'bisque', size = 28,
                                  face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = 'bisque', size = 16, 
                                     hjust = .5),
        plot.caption = element_text(color = 'bisque', size = 14,
                                    face = 'bold'),
        axis.title = element_text(color = 'bisque', size = 20, 
                                  face = 'bold'),
        axis.text = element_text(color = 'bisque', size = 12, 
                                 face = 'bold')) +
  labs(title = 'Points Scored Per Game by NFL Season',
       subtitle = 'Yearly Average Score Shown by Line\nOne Dot = One Game Played',
       x = "Season",
       y = "Total Points Scored",
       caption = 'Graph: @Timboslice003 | Data: NFLverse')

#Save
ggsave('Day_3_Historical.png', width = 17, height = 10, dpi = 'retina')
