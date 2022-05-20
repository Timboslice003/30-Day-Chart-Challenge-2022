library(gt)
library(gtExtras)
library(tidyverse)

#Load PFF Data and Team Info
mydata <- read.csv('rushing_summary.csv')
team_info <- nflreadr::load_teams()
team_logo <- team_info %>% select(team_abbr, logo = team_logo_espn)

#Select wanted data and get top 10
data_clean <- mydata %>%
  select(player,team_name, breakaway_yards, breakaway_attempts, yards, attempts) %>%
  arrange(-breakaway_yards) %>%
  head(10) %>%
  mutate(team_name = ifelse(team_name == 'CLV', 'CLE', team_name),
         apb =round(attempts/breakaway_attempts,1)) %>%
  left_join(team_logo, by = c('team_name' = 'team_abbr')) %>%
  arrange(apb) %>%
  select(Player = player,
         Team = logo, 
         'Yards from Breakaways' = breakaway_yards, 
         'Number of Breakaways' = breakaway_attempts, 
         'Total Rushing Yards' =  yards,
         'Attempts per Breakaway' = apb) 

#Create Table
data_clean %>% gt() %>%
  gt_img_rows(columns = Team) %>%
  gt_highlight_rows(rows = c(2,4,6,8,10),
                    fill = 'darkgray') %>%
  gt_highlight_rows(rows = c(1,3,5,7,9),
                    fill = 'lightgray') %>%
  data_color(columns = 'Attempts per Breakaway',
             colors = scales::col_numeric(palette = c("darkgreen", "white"), 
             domain = c(5,30))) %>%
  cols_align(align = 'center') %>%
  gt_add_divider(color = 'gray',
                 columns = Team) %>%
  tab_source_note(source_note = md('**Data: Pro Football Focus | Table: @Timboslice003**')) %>%
  tab_header(title = md('**Best Breakaway Runners in NFL**'),
             subtitle = md('Top 10 RBs in Yards from Breakaways (Rushes of 15+ Yards)')) %>%
  tab_style(style = cell_text(size = 'large',
                              align = 'center'),
            locations = cells_source_notes()) %>%
  tab_style(style = cell_text(color = 'black'),
            locations = list(cells_title('subtitle'),
                             cells_title('title'))) #%>%
  gtsave('Day_18_Data_Day.png') 

