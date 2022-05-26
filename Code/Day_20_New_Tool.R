library(tidyverse)
library(gganimate)
library(ggplot2)

#Load Draft Picks
draft <- cfbfastR::cfbd_draft_picks(2022)

#Select wanted data and clean conferences
draft_clean <- draft %>%
  select(overall, college_conference) %>%
  mutate(college_conference = ifelse(!college_conference %in% c('Pac-12', 'ACC', 'SEC', 'Big Ten', 'Big 12'),
                'Other', college_conference))

#Get list of conferences and loop through data frame to get clean table
conf_list <- unique(draft_clean$college_conference)
for (i in conf_list) {
  draft_clean[i] <- ifelse(draft_clean$college_conference == i, 1, 0)
  draft_clean[i] <- cumsum(draft_clean[i])
}

#Change to long data format
ready <- draft_clean %>%
  pivot_longer(cols = 3:8, names_to = 'conf',
               values_to = 'picks')

#Plot and animate
myanim <- ready %>% ggplot(aes(x = factor(conf, c('Other', 'SEC', 'Big Ten','Pac-12', 'Big 12', 'ACC')),
                                  y = picks)) +
  geom_col(color = 'black', fill = 'black') +
  transition_manual(frames = overall)+
  labs(title = 'NFL Draft by NCAA Conference (2022)',
       subtitle = 'Pick Number: {frame - 1}',
       x = '',
       y = 'Total Picks',
       caption = 'Animation: @Timboslice003 | Data: CFBfastR') +
  theme(panel.background = element_rect('bisque'),
        plot.background = element_rect(fill = 'black', color = 'black'),
        panel.grid.major=element_line(colour="black"),
        panel.grid.minor.y = element_line(colour="black"),
        panel.grid.minor.x = element_line(colour="bisque"),
        plot.title = element_text(color = 'bisque', size = 24, face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = 'bisque', size = 16, hjust = .5),
        plot.caption = element_text(color = 'bisque', size = 16,
                                    face = 'bold', hjust = .5),
        axis.title = element_text(color = 'bisque', size = 18),
        axis.text = element_text(color = 'bisque', size = 16),
        strip.text.x = element_text(size = 12, color = 'black', face = "bold.italic"),
        plot.margin = margin(t = 10, b = 5, l = 10, r = 30))

#Create Animation
mygif <- animate(myanim,
                 nframes = length(unique(ready$overall)),
                 fps = 12,
                 end_pause = 60)

#Save gif
#anim_save('Day_20_New_Tool.gif', mygif)
