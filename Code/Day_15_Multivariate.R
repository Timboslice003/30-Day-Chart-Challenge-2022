library(tidyverse)
library(ggplot2)

#Load Team Info
team_info <- nflreadr::load_teams()

#Load Rosters and filter for fantasy players
rosters <- nflreadr::load_rosters(2014:2021)
rosters3 <- rosters %>%
  filter(position %in% c('QB','RB','WR','TE'))

#Load Draft Data and filter for fantasy players
draft_data <- nflreadr::load_draft_picks(2014:2021)
draft_clean <- draft_data %>%
  mutate(draft_season = season,
         season = NULL) %>%
  filter(position %in% c('QB','WR','RB','TE'))

#Join the rosters, select relevant, filter for first 3 seasons
nopts <- rosters3 %>%
  left_join(draft_clean, by = 'pfr_id') %>%
  filter(!is.na(pick)) %>%
  select(position = position.x, name = full_name, round,
         pick, gsis_id, draft_season, current_season = season, draft_team = team.y) %>%
  group_by(position, round, pick, gsis_id, draft_season, draft_team) %>%
  mutate(name = first(name),
         years_exp = 1:n()) %>%
  filter(years_exp < 4) %>%
  ungroup()

#Load All Stats
newstats <- nflreadr::load_player_stats(2014:2021)

#Join with data set to remove players with > 3 years Exp.
players3 <- newstats %>%
  left_join(nopts, by = c('player_id' = 'gsis_id', 'season' = 'current_season')) %>%
  filter(!is.na(pick))

#Get Player Fantasy Points for first 3 seasons
fpts <- players3 %>%
  filter(season_type == 'REG') %>%
  group_by(player_id, name, round, pick, position, draft_team) %>%
  summarise(Games = n(),
            PassYds = sum(passing_yards),
            PassTds = sum(passing_tds),
            Ints = sum(interceptions),
            RushYards = sum(rushing_yards),
            RushTds = sum(rushing_tds),
            Fumb = sum(receiving_fumbles_lost + rushing_fumbles_lost),
            Rec = sum(receptions),
            RecYards = sum(receiving_yards),
            RecTds = sum(receiving_tds),
            TwoPt = sum(receiving_2pt_conversions + passing_2pt_conversions, + rushing_2pt_conversions))%>%
  filter(Games > 6) %>%
  mutate(FPTs = PassYds/25 + 4*PassTds 
                - 2*Ints - 2*Fumb +
                (RushYards + RecYards)/10 +
                (RushTds + RecTds)*6 +
                Rec + TwoPt,
         FPG = FPTs/Games) %>%
  left_join(team_info %>% select(team_abbr, team_color, team_color2),
            by = c('draft_team' = 'team_abbr')) %>%
  mutate(position = case_when(position == 'QB' ~ 'Quarterbacks',
                              position == 'RB' ~ 'Running Backs',
                              position == 'TE' ~ 'Tight Ends',
                              position == 'WR' ~ 'Wide Receivers'))

#Plot FPTs
fpts %>% ggplot(aes(x = round, y = FPG)) +
  geom_jitter(width = .1,
              color = fpts$team_color2,
              fill = fpts$team_color, shape = 21, size = 4) +
  geom_smooth(se = F, method = 'lm', color = 'red', size = 1.5) +
  facet_wrap(~ position, nrow = 2, ncol = 2) +
  scale_x_continuous(breaks = 1:7, labels = 1:7) +
  theme(panel.background = element_rect('white'),
        plot.background = element_rect('black'),
        panel.grid.major=element_line(colour="black"),
        panel.grid.minor.y = element_line(colour="black"),
        panel.grid.minor.x = element_line(colour="white"),
        plot.title = element_text(color = 'bisque', size = 24, face = 'bold', hjust = .5),
        plot.subtitle = element_text(color = 'bisque', size = 14, hjust = .5),
        plot.caption = element_text(color = 'bisque', size = 12, face = 'bold'),
        axis.title = element_text(color = 'bisque', size = 17),
        axis.text = element_text(color = 'bisque', size = 12),
        strip.text.x = element_text(size = 14, color = 'black', face = "bold.italic"),
        plot.margin = margin(t = 10, b = 5, l = 10, r = 30)) +
  labs(title = "Importance of Draft Capital for Fantasy Football",
       x = 'Round Drafted',
       y = 'Fantasy Points Per Game',
       subtitle = 'FPTs per Game for the First 3 Seasons of a Player\'s Career (Since the 2014 Draft Class)
                  **Dot Colors Denote Draft Team**',
       caption = 'Graph: @Timboslice003 | Data: NFLfastR & Pro Football Reference')

#Save
#ggsave('Day_15_Multivariate.png', width = 14, height = 10, dpi = 'retina')

