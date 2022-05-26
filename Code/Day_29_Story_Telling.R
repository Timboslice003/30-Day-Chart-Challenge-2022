library(rvest)
library(tidyverse)

#Load URL
url_reg <- "https://www.sportsoddshistory.com/nfl-reg/?y=2021&sa=nfl&a=sb&p=reg&o=r"
url_play <- 'https://www.sportsoddshistory.com/nfl-post/?y=2021&sa=nfl&a=sb&p=reg&o=r'

#Webscrape the table from the url for regular season
content_reg <- read_html(url_reg)
my_table_loaded_reg <- content_reg %>% html_table()
my_table_reg <- my_table_loaded_reg[[2]]

#Clean table, give column names, remove +- for the odds
colnames(my_table_reg)[2:19] <- 1:18
my_table_reg[1,] <- NA
my_table_reg$Result <- NULL
my_table_reg <- my_table_reg[!is.na(my_table_reg$Team),]
my_table_reg[my_table_reg == ""] <- '+0'
reg_final <- tibble(Team = rep('any',32))
reg_final$Team <- my_table_reg$Team
for (i in 1:32) {
  for (j in 2:19) {
    my_table_reg[i,j] <- substring(my_table_reg[i,j],2)
    reg_final[i,j] <- as.double(my_table_reg[i,j],2)
    reg_final[i,j] <- ifelse(reg_final[i,j] < 1, 0,
                              round((100/(reg_final[i,j] + 100)*100),1))
  }
}
colnames(reg_final)[2:19] <- 1:18


#Webscrape the table from the url for playoffs
content_play <- read_html(url_play)
my_table_loaded_play <- content_play %>% html_table()
my_table_play <- my_table_loaded_play[[2]]
colnames(my_table_play)[2:5] <- 19:22
my_table_play[1,] <- NA
my_table_play$Result <- NULL
my_table_play <- my_table_play[!is.na(my_table_play$Team),]
my_table_play[my_table_play == ''] <- '+0'
play_final <- tibble(Team = rep('any',32))
play_final$Team <- my_table_play$Team
for (i in 1:32) {
  for (j in 2:5) {
    my_table_play[i,j] <- substring(my_table_play[i,j],2)
    play_final[i,j] <- as.double(my_table_play[i,j],2)
    play_final[i,j] <- ifelse(play_final[i,j] < 1, 0,
                              round((100/(play_final[i,j] + 100)*100),1))
  }
}
play_final$last <- c(100,rep(0,31))
play_final$...5 <- c(66.6,33.3,rep(0,30))
colnames(play_final)[2:6] <- 19:23

#Make clean colors and logos table
team_info <- nflfastR::teams_colors_logos
team_info <- team_info %>%
  filter(!team_abbr %in% c('LAR','SD','OAK','STL')) %>%
  select(team_abbr,team_name,team_color,team_color2,team_logo_wikipedia)

#Combine 2 tables and add team colors logos
sb_odds <- reg_final %>%
  left_join(play_final, by = 'Team') %>%
  left_join(team_info, by = c('Team' = 'team_name')) %>%
  pivot_longer(cols = `1`:`23`) %>%
  mutate(name = as.integer(name),
         value = ifelse(value < .1, NA, value)) %>%
  filter(name < 23) %>%
  mutate(type = ifelse(name > 18, 'Playoffs','Regular Season'))

#Seperate Rams and Bengals
la <- sb_odds %>%
  filter(team_abbr %in% c('LA','CIN'))

#Plot
sb_odds %>% ggplot(aes(x = name, y = value)) +
  geom_point(shape = 21, size = 3,
             color = sb_odds$team_color2, fill = sb_odds$team_color) +
  nflplotR::geom_nfl_logos(data = la,
                           aes(x = name, y = value, team_abbr = team_abbr),
                           height = .075) +
  scale_x_continuous(breaks = 1:22,
                     labels = c(1:18,'Wild\nCard','Divisional',
                                'Conf\nChamp','Super\nBowl')) +
  scale_y_continuous(breaks = seq(0,70,10),
                     limits = c(0,70)) +
  labs(title = 'Rams 2021 Season',
       x = 'Week',
       y = 'Probability of Winning Superbowl',
       caption = 'Figure: @Timboslice003 | Data: sportsoddshistory.com') +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line('blue'),
        plot.background = element_rect('blue'),
        panel.background = element_rect('lightyellow'),
        plot.title = element_text(face = 'bold', size = 22,
                                  hjust = .5, color = 'yellow'),
        plot.caption = element_text(face = 'bold', size = 14,
                                    color = 'yellow'),
        plot.margin = margin(r = 20, l = 5, t = 5, b = 5),
        axis.title = element_text(face = 'bold', size = 14,
                                  hjust = .5, color = 'yellow'),
        axis.text = element_text(face = 'bold', size = 10,
                                 hjust = .5, color = 'white'),
        axis.ticks = element_line('white'))

#Save
#ggsave('Day_29_Story_telling.png', width = 14, height = 10, dpi = 'retina') 
