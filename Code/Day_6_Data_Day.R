#This code plots a comparison between the number of undefeated
#seasons by the Miami Dolphins (1) vs all of the other NFL franchises
#combined (0)

library(ggplot2)
library(tidyverse)
library(ggimage)

#Load Team logos and colors
team_info <- nflreadr::load_teams()

#Create dummy data set. Using TB just to get NFL logo
Miami <- data.table(team_abbr = c('MIA', 'TB'),
                    undefeated = c(1,0)) %>%
  left_join(team_info, by = 'team_abbr') %>%
  mutate(team_logo_wikipedia = ifelse(team_abbr == 'MIA', team_logo_wikipedia,
                                      team_league_logo))

#Plot
Miami %>% ggplot(aes(x = team_abbr, y = undefeated)) +
  geom_col(fill = Miami$team_color, color = Miami$team_color2) +
  scale_y_continuous(limits = c(-.2,1.2), breaks = c(0,1)) +
  scale_x_discrete(labels = c('Miami','Every Other Team')) +
  labs(title = 'Undefeated Seasons in NFL History',
       subtitle = 'Regular and Postseason',
       caption = '#NFLfastR| Plot: @Timboslice003',
       y = 'Undefeated Seasons') +
  theme(plot.title = element_text(color = 'orange', size = 24,
                                  face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = '#008e97', face = 'bold',
                                     size = 16, hjust = .5),
        plot.caption = element_text(color = '#f58220', size = 14, face = 'bold'),
        axis.title.y = element_text(size = 18, face = 'bold', color = 'white'),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 18, colour = 'white'),
        plot.background = element_rect(color = 'navy', fill = 'darkblue')) +
  geom_image(aes(image = team_logo_wikipedia), size = .1, by = 'width', asp = 16/9)

#Save
#ggsave('Day_6_Data_Day.png', width = 14, height = 10, dpi = 'retina')  

