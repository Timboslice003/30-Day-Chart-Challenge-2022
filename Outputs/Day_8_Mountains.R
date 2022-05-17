#This code plots the biggest upsets in the NFL from 2012 to 2021

library(tidyverse)
library(gt)
library(gtExtras)

#Load Schedule. This has pregame spreads in the data
schedule <- nflreadr::load_schedules(2012:2021)

#Get Team logos
team_info <- nflreadr::load_teams()
team_info <- team_info %>%
  select(team_abbr, logo = team_logo_espn)

#Manipulate data to show which games the underdog won
#Also add in ectra columns to show who the favorite was,
#who the underdog was, and the final score. Finally,
#join with logos and get largest spreads
results <- schedule %>%
  select(season, spread_line, result, home_team, 
         away_team, home_score, away_score) %>%
  mutate(upset = case_when(spread_line > 0 & result < 0 ~ 1,
                           spread_line < 0 & result > 0 ~ 1,
                           TRUE ~ 0)) %>%
  filter(upset == 1,
         abs(spread_line) > 10) %>%
  arrange(-abs(spread_line)) %>%
  mutate(underdog = case_when(result > 1 ~ home_team,
                              result < 1 ~ away_team,
                              TRUE ~ 'other'),
         favorite = case_when(result > 1 ~ away_team,
                              result < 1 ~ home_team,
                              TRUE ~ 'other'),
         'Final Score' = case_when(home_score > away_score ~ paste0(home_score,' - ', away_score),
                                   home_score < away_score ~ paste0(away_score,' - ', home_score),
                                   TRUE ~ 'other'),
         Spread = abs(spread_line)) %>%
  left_join(team_info, by = c('favorite' = 'team_abbr')) %>%
  left_join(team_info, by = c('underdog' = 'team_abbr')) %>%
  select(Season = season, Underdog = logo.y, Spread, Favorite = logo.x, 'Final Score') %>%
  filter(Spread > 13.5)

#Create Table
results %>% gt() %>%
  gt_img_rows(columns = Favorite) %>%
  gt_img_rows(columns = Underdog) %>%
  gt_theme_guardian() %>%
  cols_align(align = 'center') %>%
  tab_source_note(source_note = 'Data: NFLverse | Table: @Timboslice003') %>%
  tab_header(title = md('Biggest Upsets According to Spread'),
             subtitle = md('Last 10 NFL Seasons')) #%>% delete 'comment'#' to save
  gtsave('Day_8_Mountains.png')
