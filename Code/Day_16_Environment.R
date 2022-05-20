library(tidyverse)
library(usmap)

#Get Coordinates and fix team names
team_stuff <- nflreadr::load_teams()
team_info <- team_stuff %>%
  filter(!team_abbr %in% c('OAK', 'STL', 'LAR', 'SD'))
nflcoords <- read.csv('NFLcoords.csv') 
coords_clean <- nflcoords %>%
  mutate(pic = NULL,
         X = NULL,
         Team = case_when(Team == 'Forty-Niners' ~ '49ers',
                          Team == 'Redskins' ~ 'Commanders', 
                          TRUE ~ Team)) %>%
  left_join(team_info %>% select(team_abbr, Team = team_nick, team_logo_espn, team_color, team_color2),
            by = 'Team')

#Transform data to use with usmap library
cities <- usmap_transform(data =  coords_clean,
                          input_names = c('longitude', 'latitude'),
                          output_names = c('x','y'))

#Plot Map
plot_usmap(size = 1) +
  nflplotR::geom_nfl_logos(data = cities,
                           aes(x = x, y = y, team_abbr = team_abbr), height = .1) +
  theme(panel.background = element_rect('navy'),
        plot.background = element_rect('black'),
        plot.caption = element_text(color = 'white', size = 14, face = 'bold'),
        plot.title = element_text(color = 'white', size = 24, face = 'bold', hjust = .5)) +
  labs(title = 'Locations of all NFL Teams',
       caption = 'Map: @Timboslice003')
  
#Save
#ggsave('Day_16_Environment.png', width = 14, height = 10, dpi = 'retina')
