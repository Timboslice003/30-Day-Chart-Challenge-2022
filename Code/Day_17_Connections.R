library(tidyverse)
library(nflplotR)
library(gt)
library(gtExtras)

#Load PBP and Team logos
pbp <- nflreadr::load_pbp(TRUE)
team_info <- nflreadr::load_teams()
team_logo <- team_info %>% select(team_abbr, team_wordmark)

#Clean up, group by QB and WR
#Summarize stats
pbp_clean <- pbp %>%
  filter(season_type == 'REG') %>%
  group_by(passer_player_id, passer_player_name,
           receiver_player_id, receiver_player_name, posteam) %>%
  summarise(Completions = sum(complete_pass),
            TDs = sum(touchdown),
            Yards = sum(yards_gained)) %>%
  filter(Completions > 500) %>%
  ungroup() %>%
  left_join(team_logo, by = c('posteam' = 'team_abbr')) %>%
  select(Quarterback = passer_player_name,
         Receiver = receiver_player_name,
         Team = team_wordmark, Completions, TDs, Yards) %>%
  arrange(-Yards)

#Create Table
pbp_clean %>% head(10) %>% gt() %>%
  gt_img_rows(columns = Team) %>%
  cols_align(align = 'center') %>%
  tab_source_note(source_note = 'Data: NFLverse | Table: @Timboslice003') %>%
  tab_header(title = md('Best QB-WR Duos since 2000'),
             subtitle = md('Sorted by Yards')) %>%
  gt_theme_guardian() %>%
  tab_options(table.border.top.color = 'white',
              heading.align = 'center') %>%
  tab_style(style = cell_text(color = 'black'),
            locations = list(cells_title('subtitle'),
                             cells_title('title'))) %>%
  tab_style(style = cell_text(size = 'large'),
            locations = cells_source_notes())
  gtsave_extra('QBsWRs.png')  
