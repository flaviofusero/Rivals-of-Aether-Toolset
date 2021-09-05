library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(data.table)
library(stringr)
library(openxlsx)
library(glue)
library(shinyXYpad)

server = function(input, output) {
  output$image <- renderUI({
    tags$img(src = "zet_fair.png", width = '200px', height = '170px', style = 'text-align:middle;')
  })
  
  stage_canvas <- draw_stage('The endless abyss')
  
  move <- reactive({
    move <- data.table()
    print(input$tabs)
    if (grepl('jab$', input$tabs) | 
        grepl('tilt$', input$tabs) | 
        grepl('strong$', input$tabs) |
        grepl('air$', input$tabs) | 
        grepl('special$', input$tabs)) {
      move <- get_move_data(paste0(input$char, '_', input$tabs))
    }
    return(move)
  })
  
  angles <- reactive({
    
    angles = NULL
    
    if (move()[,.N] > 0) {
      angles = (pi / 180) * (ifelse(move()[,Angle] == 361, 45, move()[,Angle]) + 
                               c(-18, 
                                 18 * (sin( (pi / 180) * (input$DI - ifelse(move()[,Angle] == 361, 45, move()[,Angle])))),
                                 18))
    }
    return(angles)
  })
  
  p_traj_in_out <- reactive({
    
    p_traj_in_out <- stage_canvas
    
    if (move()[,.N] > 0) {
      for (theta in angles()[c(1,3)]) {
        p_traj_in_out <- p_traj_in_out %>% 
          add_quadratic(x0 = canvas_w / 2 + input$xy[['x']] * canvas_w / 2,
                        y0 = canvas_h / 2 + input$xy[['y']] * canvas_h / 2,
                        v0 = scaling_factor * (move()$Base.Knockback + move()$Knockback.Scaling * 
                                                 char_stats[Character == input$char_victim, Knockback.Adjustment] * (input$damage + move()$Damage) * 0.12),
                        theta = theta,
                        t_max = move()$Base.Knockback * 4 * ((char_stats[Character == input$char_victim, Knockback.Adjustment] - 1) * 0.6 + 1) +
                          (input$damage + move()$Damage) * 0.12 * move()$Knockback.Scaling * 4 * 0.65 * 
                          char_stats[Character == input$char_victim, Knockback.Adjustment], 
                        g = scaling_factor * char_stats[Character == input$char_victim, Hitstun.Gravity.Accel],
                        air_friction = scaling_factor * char_stats[Character == input$char_victim, Air.Friction],
                        drift = as.numeric(input$drift))
        
        p_traj_in_out <- p_traj_in_out[['plot']]
      }
      
      return(p_traj_in_out)
    }
  }) %>% bindCache(input$char, input$tabs, input$damage, input$drift, input$char_victim, input$xy)
  
  p_traj <- reactive({
    if (move()[,.N] > 0) {
      # print((1/2) * (move()$Base.Knockback + move()$Knockback.Scaling * 
      #                  char_stats[Character == input$char_victim, Knockback.Adjustment] * (input$damage + move()$Damage) * 0.12))
      p_traj <- p_traj_in_out() %>% 
        add_quadratic(x0 = canvas_w / 2 + input$xy[['x']] * canvas_w / 2,
                      y0 = canvas_h / 2 + input$xy[['y']] * canvas_h / 2,
                      v0 = scaling_factor * (move()$Base.Knockback + move()$Knockback.Scaling * 
                                               char_stats[Character == input$char_victim, Knockback.Adjustment] * 
                                               (input$damage + move()$Damage) * 0.12),
                      theta = angles()[2],
                      t_max = move()$Base.Knockback * 4 * 
                        ((char_stats[Character == input$char_victim, Knockback.Adjustment] - 1) * 0.6 + 1) +
                        (input$damage + move()$Damage) * 0.12 * move()$Knockback.Scaling * 4 * 0.65 * 
                        char_stats[Character == input$char_victim, Knockback.Adjustment],
                      g = scaling_factor * char_stats[Character == input$char_victim, Hitstun.Gravity.Accel],
                      air_friction = scaling_factor * char_stats[Character == input$char_victim, Air.Friction],
                      drift = as.numeric(input$drift))
      return(p_traj)
    }
  })
  
  output$move_kills <- renderText({
    if (p_traj()[['xmin']] < canvas_w / 2 - stages[[input$stages]][['side']] |
        p_traj()[['xmax']] > canvas_w / 2 + stages[[input$stages]][['ground']] + stages[[input$stages]][['side']] |
        p_traj()[['ymin']] < canvas_h / 2 - stages[[input$stages]][['bottom']] |
        p_traj()[['ymax']] > canvas_h / 2 + stages[[input$stages]][['top']]) {
      '<font color="Tomato">Kills</font>'
    } else { 
      'Does not kill'
    }
  })
  
  output$plot <- renderPlotly(p_traj()[['plot']])
  
  output$angle_text <- renderText({
    paste0('Launch angle: ', move()[, Angle], "°")
  })
}