#This code plots a pictogram using number of NFL Team Logos
#to represent the number of Superbowls every franchise has won

library(nflfastR)
library(tidyverse)
library(nflplotR)
library(data.table)

#I just manually entered SB wins for each Team
SBdt <- data.table(c('PIT','NE','SF','DAL','NYG','GB','DEN','WAS',
              'LV','KC','BAL','LA','IND','MIA','TB','NYJ','NO',
              'CHI','SEA','PHI'),
              c(6,6,5,5,4,4,3,3,3,2,2,2,2,2,2,1,1,1,1,1)) %>%
  as.tibble() %>%
  rename('team_abbr' = 'V1',
         'SBwins' = 'V2')

#Expand to help with plotting
SBdt <- SBdt %>%  expand_grid(totalW = 1:max(SBdt$SBwins)) %>%
  filter(totalW <= SBwins)

#Plot using geon_nfl_logos from NFLfastR
SBdt %>% ggplot(aes(x = fct_reorder(team_abbr, -SBwins), y = totalW)) +
  geom_nfl_logos(aes(team_abbr = team_abbr, height = .1)) +
  labs(title = 'Super Bowl Wins by NFL Franchises',
       y = 'Total SB Wins',
       x = NULL,
       subtitle = 'No SB Wins: AZ, BUF, MIN, CIN\n
                          ATL, CAR, TEN, LAC\n
                          CLE, DET, JAX, HOU',
       caption = 'Figure: @Timboslice003 | NFLverse') +
  theme(plot.background = element_rect(color = 'grey', fill = 'grey'),
        panel.background = element_rect(color = 'grey', fill = 'grey'),
        panel.grid.major.x = element_blank(),
        legend.position = 'none',
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = 'black', size = 18, face = 'bold'),
        plot.title = element_text(color = 'black', size = 24, 
                                  face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = 'black', size = 18, 
                                     face = 'bold', hjust = .5),
        plot.caption = element_text(color = 'black', size = 18,
                                    face = 'bold'),
        axis.title.y = element_text(size = 22, face = 'bold'),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(breaks = scales::pretty_breaks())

#Save
#ggsave('Day_2_Pictogram.png', width = 14, height = 10, dpi = 'retina')
