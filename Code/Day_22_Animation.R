library(tidyverse)
library(ggplot2)
library(gganimate)

#Load Team Info store abbreviations in variable
team_info <- nflreadr::load_teams() %>%
  filter(!team_abbr %in% c('OAK','STL','LAR','SD'))
team_ab <- team_info$team_abbr

#Load Schedules
schedule_loaded <- nflreadr::load_schedules()
#Get playoff games after 2002, select info, fix old abbreviations, add winning team
schedule <- schedule_loaded %>%
  filter(game_type != 'REG',
         season > 2001) %>%
  select(season, away_team:home_score) %>%
  mutate(away_team = case_when(away_team == 'OAK' ~ 'LV',
                               away_team == 'SD' ~ 'LAC',
                               away_team == 'STL' ~ 'LA',
                               TRUE ~ away_team),
         home_team = case_when(home_team == 'OAK' ~ 'LV',
                               home_team == 'SD' ~ 'LAC',
                               home_team == 'STL' ~ 'LA',
                               TRUE ~ home_team),
         winner = ifelse(home_score > away_score, home_team, away_team),
         game = row_number(season))

#Add Column for each team to track wins
for (i in team_ab) {
  schedule[i] <- ifelse(schedule$winner == i, 1, 0)
  schedule[i] <- cumsum(schedule[i])
}

#Change to long
ready_to_plot <- schedule %>%
  pivot_longer(cols = ARI:WAS, values_to = 'wins') %>%
  group_by(game) %>%
  mutate(ordering = row_number(wins)) %>%
  ungroup() %>%
  left_join(team_info %>% select(team_abbr, team_color, team_color2, team_logo_espn),
            by = c('name' = 'team_abbr'))

#Create Animation
myanim <- ready_to_plot %>% ggplot(aes(x = ordering, group = name,
                                       label = paste0(season))) +
  geom_tile(aes(y = wins/2, height = wins, width = .8),
            color = ready_to_plot$team_color2, fill = ready_to_plot$team_color) +
  nflplotR::geom_nfl_logos(aes(y = wins, team_abbr = name), hjust = -.15, width = .03) +
  transition_manual(frames = ready_to_plot$game) +
  ease_aes('cubic-in-out') +
  labs(title = 'Playoff Performance by Each NFL Franchise (Since 2002)',
       x = '',
       y = 'Playoff Games Won',
       caption = 'Figure: @Timboslice003 | Data: NFLverse') +
  geom_text(aes(x = 10, y = 19.6), color = 'grey', size = 50) +
  geom_text(aes(label = name, y = -.5), color = 'white', size = 7) +
  scale_y_continuous(limits = c(-1,30),
                     expand = c(0,0),
                     breaks = seq(0,30,3),
                     labels = seq(0,30,3),
                     position = c('right')) +
  scale_x_continuous(limits = c(0,32.5),
                     expand = c(0,0)) +
  theme(plot.background = element_rect(fill = 'black', color = 'black'),
        plot.title = element_text(color = 'white', size = 48, face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = 'white', size = 14, hjust = .5),
        plot.caption = element_text(color = 'white', size = 32, face = 'bold'),
        axis.title = element_text(color = 'white', size = 30, face = 'bold'),
        axis.text = element_text(color = 'white', size = 24, face = 'bold'),
        plot.margin = margin(t = 10, b = 5, l = 10, r = 30),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = 'white', size = 24),
        axis.text.x.top = element_text(color = 'white', size = 24, angle = 0),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = 'white'),
        panel.background = element_rect(color = 'black', fill = 'black')) +
  coord_flip()

#Animate
playoffwins_gif <- animate(plot =  myanim,
                           fps = 8,
                           nframes = length(unique(ready_to_plot$game)),
                           width = 1600,
                           height = 1200,
                           end_pause = 32)

#Save
#anim_save('Day_22_Animation.gif', playoffwins_gif)


