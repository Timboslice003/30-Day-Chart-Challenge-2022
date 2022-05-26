library(tidyverse)
library(ggplot2)
library(ggthemes)

#Load PBP
pbp_loaded <- nflreadr::load_pbp(1999:2021)

#Clean to just get run and pass plays then percent pass plays
pbp <- pbp_loaded %>%
  filter(season_type == 'REG') %>%
  group_by(game_id, season) %>%
  summarise(pass_plays = sum(pass),
            rush_plays = sum(rush),
            percent_pass = pass_plays/(pass_plays + rush_plays)) %>%
  mutate(percent_pass = round(percent_pass*100)) %>%
  ungroup()

#Get yearly means
mean_pass <- pbp %>%
  group_by(season) %>%
  summarise(mean_pass_plays = mean(percent_pass))

#Plot
pbp %>% ggplot(aes(x = season, y = percent_pass)) +
  geom_boxplot(aes(group = season), outlier.alpha = 0,
               size = 1, color = 'black') +
  geom_jitter(width = .1, color = 'black', alpha = .2) +
  theme_economist_white() +
  geom_smooth(data = mean_pass, aes(x = season, y = mean_pass_plays),
              method = 'lm', se = F, size = 2, color = 'blue') +
  scale_x_continuous(breaks = 1999:2021,
                     limits = c(1998.5,2021.5),
                     expand = c(0,0)) +
  labs(title = 'NFL Passing Tendencies Through The Years',
       subtitle = 'One Dot = One Game',
       x = 'Season',
       y = 'Passing Percentage of Plays Ran',
       caption = 'Figure: @Timboslice003 | Data: NFLverse') +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(color = 'grey'),
        plot.caption = element_text(size = 14, face = 'bold',
                                    hjust = 1),
        axis.ticks.y = element_line(color = 'black'),
        axis.title.y = element_text(size = 14, face = 'bold',
                                  vjust = 2),
        axis.title.x = element_text(size = 14, face = 'bold',
                                    vjust = -1),
        plot.title = element_text(size = 24, face = 'bold',
                                  hjust = .5),
        plot.subtitle = element_text(size = 14, hjust = .5))

#Save
#ggsave('Day_25_Trend.png', width = 14, height = 10, dpi = 'retina')
