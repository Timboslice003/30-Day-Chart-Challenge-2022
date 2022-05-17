library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggpubr)

#Load PBP since Adams was a Rookie
pbp <- nflreadr::load_pbp(2014:2021)

#Group by player and filter for players with 300+ targets
wrs <- pbp %>%
  filter(!is.na(epa),
         !is.na(receiver_player_name),
         season_type == 'REG',
         qb_dropback == 1) %>%
  group_by(receiver_player_id, receiver_player_name) %>%
  mutate(targets = n()) %>%
  ungroup() %>%
  filter(targets > 300)

#Cumulative points added by target number
wrs$target_num <- ave(wrs$epa, wrs$receiver_player_name, FUN = seq_along)
wrs$epa_sum <- ave(wrs$epa, wrs$receiver_player_name, FUN = cumsum)

#Split data into Adams and other
#ID number '00-0031381'
adams <- wrs %>%
  filter(receiver_player_id == '00-0031381')

#Plot EPA Sum
#Green Bay color #203731
plot_epa <- wrs %>% ggplot(aes(x = target_num, y = epa_sum)) +
  geom_smooth(aes(group = receiver_player_id), color = 'darkgray',
              alpha = 1, size = 1) +
  geom_smooth(data = adams, aes(group = receiver_player_id),
              color = '#203731', alpha = 1, size = 1.5) +
  labs(x = 'Target Number',
       y = 'Expeceted Points Added',
       title = 'Points Added Through Career',
       subtitle = 'Includes all players with 300 Targets since 2014') +
  theme_economist() +
  theme(plot.title = element_text(size = 18, face = 'bold', hjust = .5),
        plot.subtitle = element_text(size = 13, hjust = .5),
        axis.title = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 13, face = 'bold'))

#Load PFR player stats
newstats <- nflreadr::load_player_stats(2014:2021, stat_type = 'offense')

#Getting PFR Data and adding PFF Grades
adamsStats <- newstats %>% 
  filter(player_id == '00-0031381',
         season_type == 'REG') %>%
  group_by(player_id, player_name, recent_team, season) %>%
  summarise(Games = n(),
            Rec = sum(receptions),
            Yards = sum(receiving_yards),
            TDs = sum(receiving_tds)) %>%
  ungroup() %>%
  mutate(FPTs = Rec + Yards/10 + TDs*6,
         FPG = FPTs/Games,
         PFFgrade = c(63.2, 59.1, 72.6, 80.5, 87.8, 88, 92, 92.7))

#Plot FPTs
plot_fpts <- adamsStats %>% ggplot(aes(x = season, y = FPTs)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = recent_team), height = .2) +
  ylim(0,400) +
  scale_x_continuous(breaks = 2014:2021) +
  geom_smooth(method = 'lm', se = F, color = '#203731', size = 1.5) +
  labs(x = 'Season',
       y = 'Fantasy Points Scored',
       title = 'Fantasy Points by Season') +
  theme_economist() +
  theme(plot.title = element_text(size = 18, face = 'bold', hjust = .5),
        plot.subtitle = element_text(size = 13, hjust = .5),
        axis.title = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 13, face = 'bold'))

#Plot PFF Grades
plot_pff <- adamsStats %>% ggplot(aes(x = season, y = PFFgrade)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = recent_team), height = .2) +
  ylim(50,100) +
  scale_x_continuous(breaks = 2014:2021) +
  geom_smooth(method = 'lm', se = F, color = '#203731', size = 1.75) +
  labs(x = 'Season',
       y = 'PFF Grade',
       title = 'Pro Football Focus Grade by Season') +
  theme_economist() +
  theme(plot.title = element_text(size = 18, face = 'bold', hjust = .5),
        plot.subtitle = element_text(size = 13, hjust = .5),
        axis.title = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 13, face = 'bold'))

#Plot Face and Raiders Logo with colors to match economist theme
plot_face <- ggplot(data = data.frame(x1 = 2, y1 = 1),
       aes(x = x1, y = y1)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = 'LV'), height = .7) +
  nflplotR::geom_nfl_headshots(data = data.frame(x1 = .7, y1 = 1),
                               aes(player_gsis = '00-0031381'), height = .7) +
  xlim(0,2.5) +
  theme_void() +
  theme(plot.background = element_rect(fill = '#d5e4eb', color = '#d5e4eb'))
  

#Arrange plots
da_plot <- ggarrange(plot_epa, plot_pff, plot_fpts, plot_face, nrow = 2, ncol = 2)

cowplot::ggdraw(da_plot) +
  theme(plot.background = element_rect(fill = '#d5e4eb', color = '#d5e4eb'),
        plot.title = element_text(size = 24, face = 'bold', hjust = .5),
        plot.caption = element_text(size = 24, face = 'bold', hjust = .5)) +
  labs(title = 'Davante Adams\' Career by the Numbers',
       caption = 'Figure: @Timboslice003 | Data: NFLverse, PFF, Pro Football Reference')

#Save
#ggsave('Day_12_Theme_Day.png', width = 14, height = 10, dpi = 'retina')
