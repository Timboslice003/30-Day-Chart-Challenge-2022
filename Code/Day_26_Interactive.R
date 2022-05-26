library(shiny)
library(tidyverse)
library(ggplot2)
library(data.table)
library(ggpubr)
library(cowplot)

schedule <- read.csv('schedule21.csv')
team_info <- read.csv('team_colors.csv')

# Define UI ----
ui <- fluidPage(
  titlePanel("NCAA Football Bowl Subdivision (FBS) 2021 Teams"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var",
                  label = 'Select Team',
                  choices = team_info$school,
                  selected = 'Air Force'),
      width = 2
    ),
    mainPanel(
      plotOutput("selected_var")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$selected_var <- renderPlot(width = 1000, height = 600, {
    my_team = input$var
    team_df <- schedule %>%
      filter(home == my_team | away == my_team) %>%
      select(home, away, home_wp_before, away_wp_before, game_id) %>%
      mutate(team_wp = ifelse(home == my_team, home_wp_before, away_wp_before),
             team = ifelse(home == my_team, home, away),
             opponent = ifelse(home == my_team, away, home),
             team_wp = round(team_wp,4)*100) %>%
      select(game_id, team, opponent, team_wp) %>%
      group_by(opponent, game_id) %>%
      mutate(play = row_number(opponent),
             play_scale = play/max(play)) %>% #Normalize plays so all plots are equal
      ungroup()
    
    #Get Team and Opponents colors and logos
    my_opps <- unique(team_df$opponent)
    team_info <- read.csv('team_colors.csv')
    opp_info <- team_info %>%
      filter(school %in% my_opps)
    
    team_color = team_info[team_info$school == my_team,]$color
    team_color2 = team_info[team_info$school == my_team,]$alt_color
    team_logo = team_info[team_info$school == my_team,]$logo
    
    #Merge Team data frame and colors and logos
    ready_to_plot <- team_df %>%
      left_join(opp_info, by = c('opponent'='school'))
    
    #Plots
    opp_plots <- list()
    for (i in my_opps) {
      opp <- i
      temp_df <- ready_to_plot %>% filter(opponent == opp)
      temp_plot <- temp_df %>% ggplot(aes(x = play_scale)) +
        annotate('rect',
                 fill = team_color,
                 xmin = 0, xmax = max(temp_df$play_scale),
                 ymin = 50, ymax = 100) +
        annotate('rect',
                 fill = temp_df$color,
                 xmin = 0, xmax = max(temp_df$play_scale),
                 ymin = 0, ymax = 50) +
        geom_ribbon(aes(xmin = lag(play_scale), ymin = 50,
                        xmax = play_scale, ymax = ifelse(team_wp > 50, team_wp, 50)),
                    fill = team_color2, color = team_color2,alpha = .5) +
        geom_ribbon(aes(xmin = lag(play_scale), ymin = 50,
                        xmax = play_scale, ymax = ifelse(team_wp < 50, team_wp, 50)),
                    fill = temp_df$alt_color, color = temp_df$alt_color, alpha = .5) +
        scale_x_continuous(expand = c(0,0),
                           breaks = c(0,.5,1),
                           labels = c('Kickoff','Time in Game','Final')) +
        scale_y_continuous(expand = c(0,0)) +
        theme(plot.background = element_rect(fill = 'black', color = 'grey'),
              plot.title = element_text(color = temp_df$color, size = 24, face = 'bold', hjust = .5),
              axis.title = element_text(color = team_color, size = 12, face = 'bold'),
              axis.text = element_text(color = 'white', size = 10),
              axis.title.x = element_blank(),
              plot.margin = margin(t = 10, b = 5, l = 10, r = 30),
              panel.border = element_rect(color = 'white', fill = 'transparent')) +
        labs(title = as.character(opp),
             y = paste0('Win Probability')) +
        ggimage::geom_image(aes(image = team_logo, x = .075, y = 81.5), size = .15, asp = 16/6) +
        ggimage::geom_image(aes(image = logo, x = .075, y = 18.5), size = .12, asp = 16/6)
      assign(opp, temp_plot) 
      opp_plots[[i]] <- temp_plot
    }
    
    #Combine Plots
    num_cols <- 4
    num_rows <- ifelse(length(opp_plots) > 12, 4, 3)
    full_plot <- ggarrange(plotlist = opp_plots,
                           ncol = num_cols, nrow = num_rows)
    
    
    ggdraw(full_plot) +
      theme(plot.title = element_text(color = team_color, 
                                      face = 'bold', size = 24, vjust = 2),
            plot.background = element_rect('white'),
            plot.caption = element_text(color = team_color, 
                                        face = 'bold', size = 16, vjust = -1),
            plot.margin = margin(t = 15, b = 15, l = 10, r = 17)) +
      labs(title = paste0(my_team, ' In-Game Win Probability Charts'),
           caption = 'Figure: @Timboslice003 | Data: #CFBfastR')
    #####
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
