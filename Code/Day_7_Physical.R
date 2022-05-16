#This code plots two different metrics of evaluating an offense's
#success when running the ball: Expected Points Added per Play (EPA/Rush) 
#and PFF rushing grades, which requires a subscription.

library(nflfastR)
library(tidyverse)
library(ggplot2)
library(nflplotR)

#Get rid of scientific notation. Load PFF data, which requires
#a subscription.
options(scipen = 99999)
team_grades <- read.csv('teamgrades.csv') %>%
  filter(!is.na(Wins))

#Load play by play data and filter for non-blowouts
pbp <- load_pbp(2021)
pbp_run <- pbp %>%
  filter(play_type == 'run',
         as.double(wp) >= 0.1 & as.double(wp) < 0.9) %>%
  select(posteam, epa) %>%
  group_by(posteam) %>%
  summarise(EPA_play = mean(epa)) %>%
  left_join(team_grades, by = c('posteam' = 'team_abbr'))

#Plot data
pbp_run %>% ggplot(aes(x = Rush.Grade, y = EPA_play)) +
  geom_smooth(aes(x = Rush.Grade, y = EPA_play), method = 'lm', se = F) +
  geom_nfl_logos(aes(team_abbr = posteam, height = .1)) +
  geom_median_lines(v_var = pbp_run$Rush.Grade, h_var = pbp_run$EPA_play) %>%
  labs(title = 'Expected Points Added Per Run vs Offensive Rushing Grade',
       caption = 'Data: PFF & NFLverse | Plot: @Timboslice003',
       y = 'EPA/Rush',
       x = 'PFF Rushing Grade') +
  theme(plot.title = element_text(color = 'black', size = 24,
                                  face = 'bold', hjust = .5),
        plot.caption = element_text(color = 'black', size = 16, face = 'bold'),
        axis.title = element_text(size = 16, face = 'bold', color = 'black'),
        axis.text = element_text(size = 16, face = 'bold',color = 'black'),
        panel.background = element_rect('black'),
        plot.background = element_rect('darkgrey'))

#Save
#ggsave('Day_7_Physical.png', width = 14, height = 10, dpi = 'retina')
