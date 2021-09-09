library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(data.table)
library(stringr)
library(openxlsx)
library(glue)

server = function(input, output) {
  
  stage_canvas <- reactive({
    draw_stage(input$stage)
  })
  
  observe({
    updateSelectizeInput(
      inputId = 'hitbox',
      choices = get_move_data(paste0(input$char, '_', input$tabs))[order(-Base.Knockback, -Knockback.Scaling), Ground.Moves]
    )
  })
  
  observeEvent(input$stage, {
    updateNumericInput(
      inputId = 'x0',
      value = 0
    )
    updateNumericInput(
      inputId = 'y0',
      value = 0
    )
  })
  
  observe({
    toggle('DI', condition = !input$No_DI)
  })
  
  # observeEvent(input$reset_xy, {
  #   updateNoUiSliderInput(
  #     session = getDefaultReactiveDomain(),
  #     inputId = 'x0',
  #     value = 0)
  #   updateNoUiSliderInput(
  #     session = getDefaultReactiveDomain(),
  #     inputId = 'y0',
  #     value = 0)
  # })
  # 
  # observe({
  #   updateKnobInput(
  #     session = getDefaultReactiveDomain(),
  #     inputId = 'DI',
  #     value = ifelse(move()[,Angle] == 361, 45, move()[,Angle])
  #   )
  # })
  
  move <- reactive({ get(input$char)[Ground.Moves == input$hitbox] })
  
  angles <- reactive({
    
    angles = ifelse(move()[,Angle] == 361, 45, move()[,Angle])
    if (between(angles, 90, 270)) {angles <- 180 - angles}
    
    DI_offsets = 18 * c('DI out' = -1, 
                        'Custom DI' = ifelse(input$No_DI, 0, sin((pi / 180) * (input$DI - angles))), 
                        'DI in' = 1)
                  
    angles = angles + DI_offsets  %>% 
      setNames(c('DI out', 'Custom DI', 'DI in'))
    
    if (input$reverse_hit) {angles <- 180 - angles}
  
    return((pi / 180) * angles)
  })
  
  x0 <- reactive({ canvas_w / 2 + input$x0 / 100 * canvas_w / 2 })
  y0 <- reactive({ canvas_h / 2 + input$y0 / 100 * canvas_h / 2 })
  v0 <- reactive({
    armor_multiplier <- 1 - 0.3 * (tolower(input$char_victim) == 'etalus (armor)')
    
    v0 <- scaling_factor * armor_multiplier *
      (move()$Base.Knockback + move()$Knockback.Scaling * 
         char_stats[Character == input$char_victim, Knockback.Adjustment] * 
         (input$damage + move()$Damage) * 0.12)
    
    return(v0)
  })
  
  t_max <- reactive({
    move()$Base.Knockback * 4 * ((char_stats[Character == input$char_victim, Knockback.Adjustment] - 1) * 0.6 + 1) +
      (input$damage + move()$Damage) * 0.12 * move()$Knockback.Scaling * 4 * 0.65 * 
      char_stats[Character == input$char_victim, Knockback.Adjustment]
  })
  
  g <- reactive({ scaling_factor * char_stats[Character == input$char_victim, Hitstun.Gravity.Accel] })
  air_friction <- reactive({ scaling_factor * char_stats[Character == input$char_victim, Air.Friction] })
  drift <- reactive({ as.numeric(input$drift) * ifelse((input$reverse_hit), -1, 1) })
  
  p_traj_in_out <- reactive({
    
    p_traj_in_out <- stage_canvas()
    
    for (DI_type in c('DI out','DI in')) {
      p_traj_in_out <- p_traj_in_out %>% 
        add_quadratic(x0 = x0(),
                      y0 = y0(),
                      v0 = v0(),
                      theta = angles()[DI_type],
                      t_max = t_max(), 
                      g = g(),
                      air_friction = air_friction(),
                      drift = drift(),
                      name = DI_type)
      
      p_traj_in_out <- p_traj_in_out[['plot']]
    }
    
    return(p_traj_in_out)
  }) %>% bindCache(input$char, 
                   input$tabs, 
                   input$hitbox, 
                   input$damage, 
                   input$drift, 
                   input$char_victim, 
                   input$stage,
                   input$reverse_hit,
                   input$x0,
                   input$y0
  )
  
  p_traj <- reactive({
    
    p_traj <- p_traj_in_out() %>% 
      add_quadratic(x0 = x0(),
                    y0 = y0(),
                    v0 = v0(),
                    theta = angles()[2],
                    t_max = t_max(),
                    g = g(),
                    air_friction = air_friction(),
                    drift = drift(),
                    name = names(angles())[2]
      )
    
    return(p_traj)
  })
  
  output$plot <- renderPlotly(p_traj()[['plot']])
  
  # Right side outputs --------------------
  
  output$image <- renderUI({
    tags$img(src = glue("{input$char}/{input$char}_{input$tabs}.png"), width = '50%', height = '50%', style = 'text-align:middle;')
  })
  
  
  output$move_kills <- renderText({
    if (p_traj()[['xmin']] < canvas_w / 2 - stages[[input$stage]][['side']] |
        p_traj()[['xmax']] > canvas_w / 2 + stages[[input$stage]][['ground']] + stages[[input$stage]][['side']] |
        p_traj()[['ymin']] < canvas_h / 2 - stages[[input$stage]][['bottom']] |
        p_traj()[['ymax']] > canvas_h / 2 + stages[[input$stage]][['top']]) {
      '<font color="Tomato">Kills</font>'
    } else { 
      'Does not kill'
    }
  })
  
  output$angle_text <- renderText({
    paste0('Launch angle: ', move()[, Angle], "°")
  })
  
  output$velocity_text <- renderText({
    paste0('Launch velocity: ', round(v0() / scaling_factor, digits = 1), " pixel / frame")
  })
  
  output$DI_in_text <- renderText({
    paste0('Maximum DI in angle: ', round((ifelse(move()[,Angle] == 361, 45, move()[,Angle]) + 90) %% 360), "°")
  })
  
  output$DI_out_text <- renderText({
    paste0('Maximum DI out angle: ', round((ifelse(move()[,Angle] == 361, 45, move()[,Angle]) - 90) %% 360), "°")
  })
  
  
}