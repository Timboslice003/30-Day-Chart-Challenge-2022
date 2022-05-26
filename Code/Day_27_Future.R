library(tidyverse)
library(ggplot2)
library(data.table)
library(usmap)

#Load Info
team_info <- cfbfastR::cfbd_team_info()

#Get Corvallis Lat and Long ans OSU colors
lat <- team_info[team_info$school == 'Oregon State', "latitude"]
long <- team_info[team_info$school == 'Oregon State', "longitude"]
color1 <- team_info[team_info$school == 'Oregon State', "color"]
color2 <- team_info[team_info$school == 'Oregon State', "alt_color"]
beavs <- team_info %>%
  unnest_wider(logos, names_sep = "_") %>%
  filter(school == 'Oregon State') %>%
  select(logos_1)

#Load Schedule, Clean, replace home games lat long
#Add Montana State Logo
schedule_loaded <- read.csv('osu22.csv') 
schedule <- schedule_loaded %>%
  left_join(team_info, by = c('Opponent' = 'school')) %>%
  select(Game:Opponent, latitude, longitude, logos) %>%
  mutate(latitude = ifelse(H.A == 'H', lat, latitude),
         longitude = ifelse(H.A == 'H', long, longitude)) %>%
  unnest_wider(logos, names_sep = "_") %>%
  mutate(logos_1 = ifelse(Opponent == 'Montana State',
                          'https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/147.png',
                          logos_1),
         logos_2 = NULL)

#Change data to use with usmap()
schedule_ready <- usmap_transform(data = schedule,
                                  input_names = c('longitude', 'latitude'),
                                  output_names = c('x','y'))

#Coordinates for Schedule Labels
schedule_ready$x_label = seq(700000,-1400000,length.out = 12)

#Plot
myplot <- plot_usmap(size = 1, color = color1, fill = color2,
                     include = c('WA','OR','CA','AZ',
                                 'ID','UT','CO','NM',
                                 'MT','WY','NV')) +
  theme_void() +
  theme(plot.background = element_rect('black')) +
  geom_path(data = schedule_ready, aes(x = x, y = y), linetype = 'dashed')+
  nflplotR::geom_from_path(data = schedule_ready,
                           aes(x = x, y = y, 
                               path = logos_1, height = .1)) +
  nflplotR::geom_from_path(data = schedule_ready,
                           aes(x = 1000000, y = -799749.0, 
                               path = beavs$logos_1, height = .9)) +
  annotate('text', x = 1000000, y = 226454.6, label = 'Oregon State Beavers \n2022 Schedule',
           color = color1, size = 13) +
  geom_label(data = schedule_ready,
             aes(x = -3154389.5, y = x_label,
                 label = paste0('Game ', Game, ': ', Opponent)),
             color = ifelse(schedule_ready$H.A == 'H', 'white', color1),
             fill = ifelse(schedule_ready$H.A == 'H', color1, 'white'),
             size = 7) +
  geom_label(data = schedule_ready,
             aes(x = -2084389.5, y = -1240000.00, label = 'Home Game'),
             color = 'white', fill = color1, size = 5) +
  geom_label(data = schedule_ready,
             aes(x = -2084389.5, y = -1400000.00, label = 'Away Game'),
             fill = 'white', color = color1, size = 5) +
  scale_x_continuous(breaks = c(), expand = c(.3,0))

#Save
#ggsave('Day_27_Future.png', width = 14, height = 10, dpi = 'retina')

