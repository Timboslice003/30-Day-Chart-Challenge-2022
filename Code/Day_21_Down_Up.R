library(tidyverse)
library(ggplot2)

#Load Team Info
team_info <- nflreadr::load_teams()

#Load pbp and get KC-BUF playoff game
#Game ID: '2021_20_BUF_KC'
pbp <- nflreadr::load_pbp(2021)
kcb <- pbp %>%
  filter(game_id == '2021_20_BUF_KC') %>%
  mutate(play_number = row_number(game_id),
         home_wp = round(home_wp*100,2)) %>%
  select(play_number, home_wp, game_seconds_remaining,
         quarter_end)

kcb %>% ggplot(aes(x = play_number, y = home_wp)) +
  annotate('rect',
           fill = 'red',
           xmin = 1, xmax = max(kcb$play_number),
           ymin = 50, ymax = 100) +
  annotate('rect',
           fill = 'blue',
           xmin = 1, xmax = max(kcb$play_number),
           ymin = 0, ymax = 50) +
  geom_ribbon(aes(xmin = lag(play_number), ymin = 50,
                  xmax = play_number, ymax = ifelse(home_wp > 50, home_wp, 50)),
              fill = 'yellow', color = 'yellow',alpha = .3) +
  geom_ribbon(aes(xmin = lag(play_number), ymin = 50,
                  xmax = play_number, ymax = ifelse(home_wp < 50, home_wp, 50)),
              fill = 'white', color = 'red',alpha = .5) +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(1,35,86,120,181,max(kcb$play_number)),
                     labels = c('Kickoff',1,2,3,4,'Final')) +
  scale_y_continuous(expand = c(0,0)) +
  geom_vline(xintercept = c(1,35,86,120,181,max(kcb$play_number))) +
  geom_hline(yintercept = 50) +
  theme(plot.background = element_rect(color = 'black', fill = 'black'),
        plot.title = element_text(color = 'white', size = 24, face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = 'white', size = 14, hjust = .5),
        plot.caption = element_text(color = 'white', size = 14, face = 'bold'),
        axis.title = element_text(color = 'white', size = 18),
        axis.text = element_text(color = 'white', size = 14),
        plot.margin = margin(t = 10, b = 5, l = 10, r = 30),
        panel.border = element_rect(color = 'black', fill = 'transparent')) +
  labs(title = 'Chiefs vs Bills 2021 AFC Divisional Round Playoff Game',
       subtitle = 'Win Probabilty Throughout Game',
       x = 'Quarter',
       y = 'Chiefs Win Probabilty',
       caption = 'Figure: @Timboslice003 | Data: NFLverse') +
  nflplotR::geom_nfl_logos(aes(x = 26, y = 80, team_abbr = 'KC'), height = .4) +
  nflplotR::geom_nfl_logos(aes(x = 26, y = 20, team_abbr = 'BUF'), height = .4)

#Save
#ggsave('Day_21_Down_Up.png', width = 14, height = 10, dpi = 'retina')
