#This code plots a WR's Average depth of target (ADOT) vs a WR's
#average yards after reception (YAC)

library(tidyverse)
library(ggplot2)
library(ggrepel)

#Load PFF Data and team colors. Fix for abbreviations for Team colors 
#and merge together
newstats <- read.csv('receiving_summary.csv')
team_info <- nflreadr::load_teams()
clean_stats <- newstats %>%
  select(player_id, player,
         ypr = yards_per_reception, adot = avg_depth_of_target,
         targets, team_name, receptions,
         yac = yards_after_catch_per_reception) %>%
  filter(targets > 79) %>%
  mutate(team_abbr = case_when(team_name == 'ARZ' ~ 'ARI',
                               team_name == 'BLT' ~ 'BAL',
                               team_name == 'HST' ~ 'HOU',
                               TRUE ~ team_name),
         player_id = as.character(player_id)) %>%
  left_join(team_info %>% select(team_abbr, team_color, team_color2),
            by = 'team_abbr')

#Plot
clean_stats %>% ggplot(aes(x = adot, y = ypr,
                           label = ifelse(player %in% c('Deebo Samuel',
                                                        'Ja\'Marr Chase'),
                                          player, ''))) +
  geom_point(color =clean_stats$team_color2,
             fill = clean_stats$team_color, shape = 23, size = 6) +
  geom_smooth(method = 'lm', se = F, color = 'black', size = 1.5) +
  geom_label_repel(box.padding = 1, size = 5, color = clean_stats$team_color,
                   fill = clean_stats$team_color2) +
  theme_bw() +
  labs(title = 'Best players with football in their hands in 2021',
       x = 'Average Depth of Target',
       y = 'Yards Per Reception',
       subtitle = 'Players with at least 80 targets\nColors represent players\' teams',
       caption = 'Graph: @Timboslice003 | Data: Pro Football Focus') +
  theme(plot.title = element_text(color = 'black', size = 24, face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = 'black', size = 14, 
                                     face = 'bold', hjust = .5),
        plot.caption = element_text(color = 'black', size = 14, 
                                    face = 'bold'),
        legend.position = 'none',
        axis.title.x = element_text(color = 'black', size = 16),
        axis.title.y = element_text(color = 'black', size = 16),
        axis.text = element_text(color = 'black', size = 16),
        plot.margin = margin(t = 0, b = 0, l = 10, r = 30)) +
  scale_y_continuous(limits = c(6,19),
                     breaks = seq(6,18,2))

#Save
#ggsave('Day_13_Correlation.png', width = 14, height = 10, dpi = 'retina')
