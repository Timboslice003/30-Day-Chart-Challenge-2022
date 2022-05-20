library(ggplot2)
library(tidyverse)
library(countrycode)
library(ggimage)

#Load Data and clean, add colors for countries
mydata <- read.csv('foreignborn.csv')
players <- mydata %>%
  mutate(Birthplace = substring(Birthplace,2)) %>%
  group_by(Birthplace) %>%
  summarise(Total = n()) %>%
  arrange(-Total) %>%
  filter(Birthplace != 'United States',
         Total > 1) %>%
  mutate(mycolor = c('red', 'darkgreen', 'darkblue', 
                     'yellow', 'red', 'darkgreen',
                     'red', 'blue', 'red'))

#Plot
players %>% ggplot(aes(x = reorder(Birthplace, Total), y = Total)) +
  geom_col(fill = players$mycolor, color = 'black') +
  scale_y_continuous(breaks = 0:22, labels = 0:22) +
  ggthemes::theme_wsj() +
  coord_flip() +
  labs(x = 'Birthplace',
       y = 'Active Players',
       title = 'Foreign Born NFL Players',
       caption = 'Figure: @Timboslice003 | Data: Wikipedia') +
  theme(plot.title = element_text(hjust = .5, size = 24),
        axis.title = element_text(color = 'black', size = 18),
        plot.caption = element_text(color = 'black', size = 16, face = 'bold'))
#Save
#ggsave('Day_19_Global_Change.png', width = 14, height = 10, dpi = 'retina')
