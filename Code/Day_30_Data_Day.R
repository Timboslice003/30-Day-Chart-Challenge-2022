library(tidyverse)
library(ggplot2)
library(gt)
library(gtExtras)

#Load PFF Data
df <- read.csv('receiving_summary.csv')
df2 <- read.csv('receiving_scheme.csv')

#Get logos and faces
logos <- nflreadr::load_teams() %>%
  select(team_abbr, logo = team_logo_wikipedia)

faces <- nflreadr::load_rosters()
faces <- faces %>% select(pff_id, shot = headshot_url)

#Get WR Data table set up and add logos
wrs_loaded <- df %>%
  left_join(df2, by = c('player_id','player','team_name','position')) %>%
  select(player:team_name, grades_pass_route, yprr,
         man_grades_pass_route, zone_grades_pass_route, receptions) %>%
  filter(receptions > 40,
         position == 'WR') %>%
  left_join(logos, by = c('team_name' = 'team_abbr')) %>%
  left_join(faces, by = c('player_id' = 'pff_id')) %>%
  select(shot,logo, grades_pass_route, yprr, 
         man_grades_pass_route, zone_grades_pass_route) %>%
  arrange(-grades_pass_route) %>%
  head(10)

#Create Table
wrs_loaded %>% gt() %>%
  gt_img_rows(columns = shot, height = 42) %>%
  gt_img_rows(columns = logo, height = 42) %>%
  cols_align(align = 'center') %>%
  cols_label(shot = '', logo = '',
             grades_pass_route = 'Receiving Grade',
             man_grades_pass_route = 'Grade vs Man',
             zone_grades_pass_route = 'Grade vs Zone',
             yprr = 'Yards Per Route Ran') %>%
  data_color(columns = c('yprr'),
             colors = scales::col_numeric(palette = c("white", "darkgreen"), 
                                          domain = c(2,3.2))) %>%
  data_color(columns = c('grades_pass_route',
                         'man_grades_pass_route',
                         'zone_grades_pass_route'),
             colors = scales::col_numeric(palette = c('darkred',"white", "darkgreen"), 
                                          domain = c(61,100))) %>%
  tab_style(
    style = cell_borders(sides = 'all', color = "white",
                         weight = px(2), style = "solid"),
    locations = cells_body(columns = 3:6, rows = everything())) %>%
  gt_theme_espn() %>%
  tab_header(title = md('Best WRs of the 2021 NFL Season')) %>%
  tab_source_note(source_note = 'Data: Pro Football Focus | Table: @Timboslice003') %>%
  tab_options(heading.background.color = 'grey94',
              heading.align = 'center',
              table.background.color = 'grey94',
              column_labels.background.color = 'grey98',
              column_labels.border.bottom.color = 'white',
              table_body.border.bottom.color = 'white',
              source_notes.font.size = 20,
              heading.title.font.size = 30,
              table.font.size = 20,
              table.font.weight = 'bold') #%>%
  gtsave_extra('Day_30_Data_Day.png')

    