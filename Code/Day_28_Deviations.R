library(tidyverse)
library(ggplot2)

#Read in CSV
wins_loaded <- read.csv('historical_win_totals.csv')

#Fix data with NAs
wins_acc <- wins_loaded %>%
  mutate(true_wins = ifelse(is.na(true_wins),win_total,true_wins),
         true_wins = round(true_wins,1)) %>%
  filter(season > 2010,
         !is.na(true_wins)) %>%
  select(true_wins, actual_wins, win_total) %>%
  group_by(win_total) %>%
  mutate(total = n(),
         ave_wins = mean(actual_wins))

#Create a tibble with data to create perfect 1:1 line for
#predicted vs actual
perfect_line <- tibble(xp = c(3,13),
                       yp = c(3,13))

#Plot
wins_acc %>% ggplot(aes(x = win_total, y = ave_wins)) +
  geom_jitter(aes(y = actual_wins), 
              width = .1, height = 0, color = 'grey60') +
  geom_point(size = 3,
             color = ifelse(wins_acc$ave_wins > wins_acc$win_total,
                            'darkgreen','red')) +
  geom_line(aes(y = win_total), size = 1.5) +
  geom_errorbar(aes(ymin = win_total, ymax = ave_wins), width = 0,
                color = ifelse(wins_acc$ave_wins > wins_acc$win_total,
                               'darkgreen','red'),
                size = 1) +
  theme_light() +
  labs(x = 'Preseason Win Totals',
       y = 'Actual Wins',
       title = 'Accuracy of NFL Preseason Win Totals',
       subtitle = 'From 2011 to 2020',
       caption = 'Figure: @Timboslice003 | Data: @mrcaseb') +
  scale_x_continuous(breaks = 2.5:12.5) +
  scale_y_continuous(breaks = 0:15) +
  theme(panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = 'bold', size = 20, hjust = .5),
        plot.caption = element_text(face = 'bold', size = 14),
        plot.subtitle = element_text(size = 12, hjust = .5),
        axis.title = element_text(face = 'bold', size = 14, hjust = .5),
        axis.text = element_text(face = 'bold', size = 10, 
                                 hjust = .5, color = 'black'),
        axis.ticks = element_line('black'),
        plot.background = element_rect('grey'),
        plot.margin = margin(r = 20, l = 5, t = 5, b = 5))

#Save
#ggsave('Day_28_Deviations.png', width = 14, height = 10, dpi = 'retina')  
