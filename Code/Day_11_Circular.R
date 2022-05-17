#This code gathers PFF team data from the last 3 years and puts teams into
#tiers based upon their grades. Then it creates a plot to show which tiers
#of teams are making the playoffs the most.

library(tidyverse)
library(ggplot2)

#Load Schedule, set color, and get NFL logo url
schedule <- nflreadr::load_schedules(2019:2021)
myurl <- "https://raw.githubusercontent.com/nflverse/nflfastR-data/master/NFL.png"
mycolor = 'white'

#Gather Playoff Teams
pteams <- schedule %>%
  filter(game_type != 'REG') %>%
  pivot_longer(cols = c(away_team,home_team)) %>%
  select(game_id, season, game_type, team_abbr = value) %>%
  group_by(season, team_abbr) %>%
  summarise(playoffs = ifelse(n() > 0, 1, 0))

#Load PFF Team Grades
team_grades3 <- read.csv('grades3.csv')

#Lengthen Data to have one row per year/team/side of ball
team_grades_long <- pivot_longer(team_grades3, off21:def19, names_to = 'year', values_to = 'grades') %>%
  mutate(season = as.integer(paste0(20,substring(year,4))),
         od = substring(year,1,3),
         year = NULL)

#Gather Defensive grades and set Tiers
def_grades <- team_grades_long %>%
  filter(od == "def") %>%
  left_join(pteams, by = c('team_abbr','season')) %>%
  mutate(playoffs = ifelse(is.na(playoffs), 0, 1),
         grade_group = case_when(grades <= 56 ~ 'Tier 4',
                                 grades > 56 & grades < 69.5 ~ 'Tier 3',
                                 grades >= 69.5 & grades < 80.5 ~ 'Tier 2',
                                 grades >= 80.5 ~ 'Tier 1'),
         percentile = (grades - min(grades))/(max(grades)-min(grades)))

#Group Defensive Grades by Tier
def_grades_grouped <- def_grades %>%
  group_by(grade_group) %>%
  summarise(chance = mean(playoffs),
            teams = n())

#Gather Offensive grades and set Tiers
off_grades <- team_grades_long %>%
  filter(od == "off") %>%
  left_join(pteams, by = c('team_abbr','season'))  %>%
  mutate(playoffs = ifelse(is.na(playoffs), 0, 1),
         grade_group = case_when(grades <= 65.5 ~ 'Tier 4',
                                 grades > 65.5 & grades < 74.2 ~ 'Tier 3',
                                 grades >= 74.2 & grades < 85.5 ~ 'Tier 2',
                                 grades >= 85.5 ~ 'Tier 1'),
         percentile = (grades - min(grades))/(max(grades)-min(grades)))

#Group Offensive Grades by Tier
off_grades_grouped <-off_grades %>%
  group_by(grade_group) %>%
  summarise(chance = mean(playoffs),
            teams = n())

#Plot
def_grades_grouped %>% ggplot(aes(color = as.factor(grade_group))) +
  geom_curve(aes(x = rep(-.5, length(chance)),
                 y = rep(-.5, length(chance)),
                 xend = chance,
                 yend = rep(0, length(chance))),
             size = 3,
             curvature = .5) +
  geom_curve(data = off_grades_grouped,
             aes(color = as.factor(grade_group),
                 x = rep(-.5, length(chance)),
                 y = rep(.5, length(chance)),
                 xend = chance,
                 yend = rep(0, length(chance))),
             size = 3,
             curvature = -.5) +
  scale_color_manual(values = c('green', 'blue', 'yellow', 'red')) +
  ylim(-5,5) +
  theme_void() +
  geom_segment(x = 0, xend = 1,
               y = 0, yend = 0,
               color = mycolor,
               size = 2) + 
  geom_segment(x = 0, xend = 0,
               y = -.3, yend = .3,
               color = mycolor,
               size = 2) +
  geom_segment(x = .2, xend = .2,
               y = -.3, yend = .3,
               color = mycolor,
               size = 2) +
  geom_segment(x = .4, xend = .4,
               y = -.3, yend = .3,
               color = mycolor,
               size = 2) +
  geom_segment(x = .6, xend = .6,
               y = -.3, yend = .3,
               color = mycolor,
               size = 2) +
  geom_segment(x = .8, xend = .8,
             y = -.3, yend = .3,
             color = mycolor,
             size = 2) +
  geom_segment(x = 1, xend = 1,
               y = -.3, yend = .3,
               color = mycolor,
               size = 2) +
  annotate('text', x = -.15, y = 0,
           label = 'Percentage To\n Make Playoffs',
           size = 6,
           color = mycolor) + 
  annotate('text', x = .005, y = .5,
           label = '0 %',
           size = 4,
           color = mycolor) +
  annotate('text', x = .205, y = .5,
           label = '20 %',
           size = 4,
           color = mycolor) +
  annotate('text', x = .405, y = .5,
           label = '40 %',
           size = 4,
           color = mycolor) +
  annotate('text', x = .605, y = .5,
           label = '60 %',
           size = 4,
           color = mycolor) +
  annotate('text', x = .805, y = .5,
           label = '80 %',
           size = 4,
           color = mycolor) +
  annotate('text', x = 1.005, y = .5,
           label = '100 %',
           size = 4,
           color = mycolor) +
  theme(plot.background = element_rect(color = 'black', fill = 'black'),
        panel.background = element_rect(color = 'black', fill = 'black'),
        legend.title = element_text(colour="white", size=16, face="bold"),
        legend.text = element_text(colour="white", size=16, face="bold"),
        legend.background = element_rect(color = 'black', fill = 'black'),
        plot.title = element_text(color = 'white', size = 24, face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = 'white', size = 14, face = 'bold', hjust = .5),
        plot.caption = element_text(color = 'white', size = 16,
                                    face = 'bold', hjust = .8, vjust = 3),
        legend.position = c(.8,.3)) +
  labs(title = 'PFF Team Grades and Playoff Appearances',
       subtitle = '2019 - 2021',
       caption = 'Graph: @Timboslice003 | Data: PFF',
       color = 'Grade Tier') +
  annotate(
    nflplotR::GeomFromPath,
    y = 0,
    x = -.5,
    height = .16,
    path = myurl) +
  annotate('text', x = -.45, y = 3,
           label = 'Offensive\n Grades',
           size = 8,
           color = mycolor) +
  annotate('text', x = -.45, y = -3,
           label = 'Defensive\n Grades',
           size = 8,
           color = mycolor)

#Save
#ggsave('Day_11_Circular.png', width = 14, height = 10, dpi = 'retina')






  