#This code plot the expected points added per play (epa/play) for 
#Oregon State from 2015 to 2018

library(ggplot2)

#Load play by play, team info, and sort for Pac-12 schools
pbp <- cfbfastR::load_cfb_pbp(2015:2021)
team_in1 <- cfbfastR::cfbd_team_info()

#Fix logo column
team_in <- team_in1 %>%
  unnest_wider(logos) %>%
  filter(conference == 'Pac-12') %>%
  rename(logo1 = ...1,
         logo2 = ...2)
  
#Get OSU data and logo/colors
pbp_osu <- pbp %>%
  filter(pos_team == 'Oregon State',
         !is.na(EPA),
         rush == 1 | pass == 1) %>%
  group_by(year, pos_team) %>%
  summarise(EPApp = mean(EPA)) %>%
  left_join(team_in, by = c('pos_team' = 'school'))

#Get all other pac-12 schools
pbp_all <- pbp %>%
  filter(!is.na(EPA),
         rush == 1 | pass == 1,
         offense_conference == 'Pac-12') %>%
  group_by(year, pos_team) %>%
  summarise(EPApp = mean(EPA)) %>%
  left_join(team_in, by = c('pos_team' = 'school'))

#Plot
ggplot() +
  geom_point(data = pbp_all, aes(x = year, y = EPApp, fill = color, color = alt_color),
             size = 6, shape = 21, stroke = 2) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_line(data = pbp_osu, aes(x = year, y = EPApp, color = color), size = 2) +
  ggimage::geom_image(data = pbp_osu, aes(image = logo1,
                                          x = year, y = EPApp), size = .075, asp =16/9) +
  scale_x_continuous(labels = 2015:2021, breaks = 2015:2021) +
  labs(title = 'Oregon State Offensive Performance',
       subtitle = 'Since 2015 using Expected Points Added per Play',
       y = 'EPA/Play',
       x = 'Season',
       caption = 'Graph: @Timboslice003 | Data: CFBfastR') +
  theme(plot.background = element_rect('black'),
        panel.background = element_rect('#dea076'),
        panel.grid.major=element_line(color="black"),
        panel.grid.minor=element_line(color="black"),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(color = '#c34500', size = 24,
                                  face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = 'grey', size = 14,
                                     hjust = .5, face = 'bold'),
        plot.caption = element_text(color = '#c34500', size = 14, 
                                    face = 'bold'),
        axis.title = element_text(color = 'grey', size = 16),
        axis.text = element_text(color = 'grey', size = 12))

#Save
#ggsave('Day_10_Experimental.png', width = 14, height = 10, dpi = 'retina')

