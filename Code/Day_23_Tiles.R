library(tidyverse)
library(ggplot2)
library(data.table)
library(ggpubr)
library(cowplot)

#Load pbp and team info
pbp <- cfbfastR::load_cfb_pbp(2021)

#Get OSU plays only, remove FCS (Idaho) game
#Select needed data columns
#Organize data frame as necessary
osu21 <- pbp %>%
  filter(home == 'Oregon State' | away == 'Oregon State',
         away != 'Idaho',
         !is.na(home_wp_before),
         !is.na(away_wp_after)) %>%
  select(home, away, home_wp_before, away_wp_before, week) %>%
  mutate(beavs_wp = ifelse(home == 'Oregon State', home_wp_before, away_wp_before),
         beavs = ifelse(home == 'Oregon State', home, away),
         opponent = ifelse(home == 'Oregon State', away, home),
         beavs_wp = round(beavs_wp,4)*100) %>%
  select(week, beavs, opponent, beavs_wp) %>%
  mutate(week = ifelse(opponent == 'Utah State', 14, week)) %>%
  group_by(opponent) %>%
  mutate(play = row_number(opponent),
         play_scale = play/max(play)) %>% #Normalize plays so all plots are equal
  ungroup() %>%
  arrange(week)

#Store List of opponents
opp_list <- unique(osu21$opponent)

#Get team colors logos of OSU and opponents
#Change colors around for visualization
team_info <- cfbfastR::cfbd_team_info()
teams_need <- team_info %>%
  filter(school %in% c(opp_list,'Oregon State')) %>%
  unnest_wider(col = 'logos', names_sep = '_') %>%
  mutate(opp_alt_color = case_when(school %in% c('USC',
                                                 'Purdue',
                                                 'Washington State', 
                                                 'Utah State') ~ alt_color,
                                   TRUE ~ color),
         opp_color = case_when(school %in% c('USC', 
                                             'Washington State', 
                                             'Utah State') ~ color,
                               school == 'Colorado' ~ 'black',
                               school == 'Purdue' ~ 'black',
                               school == 'Utah' ~ 'white',
                               TRUE ~ alt_color)) %>%
  select(school, opp_color, opp_alt_color, opp_logo = logos_1)


#Merge colors, logo
ready_to_plot <- osu21 %>%
  left_join(teams_need, by = c('opponent'='school')) %>%
  mutate(beavs_color = teams_need[teams_need$school == 'Oregon State',]$opp_alt_color,
         beavs_alt_color = 'black',
         beavs_logo = teams_need[teams_need$school == 'Oregon State',]$opp_logo)

#Create plots storing them as a variable with 
#variable name the same as the school
opp_plots <- list()
for (i in opp_list) {
opp <- i
temp_df <- ready_to_plot %>% filter(opponent == opp)
temp_plot <- temp_df %>% ggplot(aes(x = play_scale)) +
  annotate('rect',
           fill = temp_df$beavs_color,
           xmin = 0, xmax = max(temp_df$play_scale),
           ymin = 50, ymax = 100) +
  annotate('rect',
           fill = temp_df$opp_alt_color,
           xmin = 0, xmax = max(temp_df$play_scale),
           ymin = 0, ymax = 50) +
  geom_ribbon(aes(xmin = lag(play_scale), ymin = 50,
                  xmax = play_scale, ymax = ifelse(beavs_wp > 50, beavs_wp, 50)),
              fill = temp_df$beavs_alt_color, color = temp_df$beavs_alt_color,alpha = .5) +
  geom_ribbon(aes(xmin = lag(play_scale), ymin = 50,
                  xmax = play_scale, ymax = ifelse(beavs_wp < 50, beavs_wp, 50)),
              fill = temp_df$opp_color, color = temp_df$opp_color, alpha = .5) +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(0,.5,1),
                     labels = c('Kickoff','Time in Game','Final')) +
  scale_y_continuous(expand = c(0,0)) +
  theme(plot.background = element_rect(fill = 'black', color = 'grey'),
        plot.title = element_text(color = temp_df$opp_alt_color, size = 24, face = 'bold', hjust = .5),
        axis.title = element_text(color = temp_df$beavs_color, size = 14, face = 'bold'),
        axis.text = element_text(color = 'white', size = 10),
        axis.title.x = element_blank(),
        plot.margin = margin(t = 10, b = 5, l = 10, r = 30),
        panel.border = element_rect(color = 'white', fill = 'transparent')) +
  labs(title = as.character(opp),
       y = 'Beavers WP') +
  ggimage::geom_image(aes(image = beavs_logo, x = .07, y = 89), size = .15, asp = 16/6) +
  ggimage::geom_image(aes(image = opp_logo, x = .075, y = 18.5), size = .12, asp = 16/6)
assign(opp, temp_plot) 
opp_plots[[i]] <- temp_plot
}

#Combine Plots
full_plot <- ggarrange(plotlist = opp_plots,
                  ncol = 4, nrow = 3)

ggdraw(full_plot) +
  theme(plot.title = element_text(color = unique(ready_to_plot$beavs_color), 
                                  face = 'bold', size = 24, vjust = 2),
        plot.background = element_rect(unique(ready_to_plot$beavs_alt_color)),
        plot.caption = element_text(color = unique(ready_to_plot$beavs_color), 
                                    face = 'bold', size = 16, vjust = -1),
        plot.margin = margin(t = 15, b = 15, l = 10, r = 17)) +
  labs(title = 'Oregon State Beavers 2021 In-Game Win Probability Charts',
       caption = 'Figure: @Timboslice003 | Data: CFBfastR')

#Save
#ggsave('Day_23_Tiles.png', width = 17, height = 10, dpi = 'retina')
