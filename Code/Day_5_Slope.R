#This code plots an NFL team's wins vs their passing grade and 
#rushin grade. Grades courtesy of PFF, which requires a subscription.

library(tidyverse)
library(ggplot2)
library(data.table)

#Load PFF Data
team_grades <- read.csv('teamgrades.csv')

#Fix csv file and lengthen data set
team_grades <- team_grades[complete.cases(team_grades),] %>%
  pivot_longer(cols = contains('Grade'),values_to = 'Grades', names_to = 'Type')

#Get slopes for both grades
pass_slope <- team_grades %>%
  filter(Type == 'Pass.Grade') %>%
  lm(data = ., Wins ~ Grades)

rush_slope <- team_grades %>%
  filter(Type == 'Rush.Grade') %>%
  lm(data = ., Wins ~ Grades)

#Plot Data
team_grades %>% ggplot(aes(y = Wins)) +
  nflplotR::geom_nfl_logos(aes(x = Grades, team_abbr = team_abbr, height = .1)) +
  facet_wrap(~ Type, labeller = labeller(Type = c('Pass.Grade' = 'Pass Grade',
                                                  'Rush.Grade' = 'Rush Grade'))) +
  geom_smooth(aes(x = Grades), method = 'lm', se = F) +
  scale_y_continuous(limits = c(0,17),
                     breaks = seq(0,16,4)) +
  labs(title = 'Wins vs Offensive Pass & Rush Grades',
       subtitle = paste0('Pass Grade Slope: ', round(pass_slope$coefficients[2],3), '\n',
                         'Rush Grade Slope: ', round(rush_slope$coefficients[2],3)),
       caption = 'Data: PFF | Plot: @Timboslice003') +
  theme_bw() +
  theme(plot.background = element_rect('grey'),
        plot.title = element_text(color = 'black', size = 24,
                                  face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = 'black', size = 18, hjust = .5),
        plot.caption = element_text(color = 'black', size = 18, face = 'bold'),
        axis.title = element_text(size = 18, face = 'bold', color = 'black'),
        axis.text  = element_text(size = 18, face = 'bold', color = 'black'),
        strip.background = element_rect('black'),
        strip.text = element_text(color = 'white', face = 'bold',
                                  size = 16))
#Save
#ggsave('Day_5_Slope.png', width = 14, height = 10, dpi = 'retina')
