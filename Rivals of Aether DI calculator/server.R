library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(data.table)
library(stringr)
library(openxlsx)
library(glue)
library(Rcpp)
library(htmlwidgets)
library(DT)
source('js/click_anywhere.js')
sourceCpp('cpp/utils.cpp')

server = function(input, output, session) {
  
  # Observes --------------------
  observe({
    updateSelectizeInput(
      inputId = 'hitbox',
      choices = paste0(input$char,
                       '_',
                       get_move_data(paste0(input$char, '_', input$tabs))[order(-Base.Knockback, -Knockback.Scaling), Ground.Moves]) %>% 
        setNames(get_move_data(paste0(input$char, '_', input$tabs))[order(-Base.Knockback, -Knockback.Scaling), Ground.Moves])
    )
  })
  
  observe({
    toggle('DI', condition = !input$No_DI)
  })
  
  # Reactive values --------------------
  
  stage_elements <- reactive({ make_stage_elements(input$stage) }) %>% bindCache(input$stage)
  stage_canvas <- reactive({ draw_stage(stage_elements()) }) %>% bindCache(input$stage)
  
  snap_to_element <- reactive({ snap_to(elements = stage_elements()[-1], 
                                        x = input$clickposition[1],
                                        y = input$clickposition[2],
                                        snap_tol = snap_tol)
  }) %>% bindCache(input$stage, input$clickposition)
  
  is_grounded <- reactive({ if (!is.na(snap_to_element()) & input$autosnap == TRUE) {
    TRUE
  } else { FALSE }
  }) %>% 
    bindCache(input$stage,
              input$clickposition,
              input$autosnap)
  
  move <- reactive({ 
    get(strsplit(input$hitbox, '_', fixed = TRUE)[[1]][1])[Ground.Moves == strsplit(input$hitbox, '_', fixed = TRUE)[[1]][2]]
  }) %>% 
    bindCache(input$hitbox)
  # If we don't add the bindEvent, then when the char changes, there is a split moment when the hitbox is not updated
  # This is bad because then e.g. going from Zetter > fair (sweetpot) to Ori > fair (sweetspot) causes an error as there is no such move
  # We only care about the hitbox anyway so we can let the move update according to that
  
  parsed_angles <- reactive ({ 
    parse_angle(move(), is_grounded()) 
  }) %>% bindCache(input$hitbox,
                   input$autosnap,
                   input$clickposition,
                   input$stage)
  
  angles <- reactive({
    angles = parsed_angles()
    if (between(angles, 90, 270)) {angles <- 180 - angles}
    
    DI_offsets = 18 * c('DI out' = -1, 
                        'Custom DI' = ifelse(input$No_DI, 0, sin((pi / 180) * (input$DI - angles))), 
                        'DI in' = 1)
    
    angles = angles + DI_offsets  %>% 
      setNames(c('DI out', 'Custom DI', 'DI in'))
    
    if (input$reverse_hit) {angles <- 180 - angles}
    
    return((pi / 180) * angles)
  }) %>%
    bindCache(input$autosnap,
              input$clickposition,
              input$stage,
              input$char,
              input$hitbox,
              input$No_DI,
              input$DI,
              input$reverse_hit)
  
  v0 <- reactive({
    armor_multiplier <- 1 - 0.3 * (tolower(input$char_victim) == 'etalus (armor)')
    
    v0 <- scaling_factor * armor_multiplier *
      (move()$Base.Knockback + move()$Knockback.Scaling * 
         char_stats[Character == input$char_victim, Knockback.Adjustment] * 
         (input$damage + move()$Damage) * 0.12)
    
    return(v0)
  }) %>% 
    bindCache(input$hitbox,
              input$char_victim,
              input$damage)
  
  t_max <- reactive({
    move()$Hitstun.Modifier * 
      (move()$Base.Knockback * 4 * ((char_stats[Character == input$char_victim, Knockback.Adjustment] - 1) * 0.6 + 1) +
         (input$damage + move()$Damage) * 0.12 * move()$Knockback.Scaling * 4 * 0.65 * 
         char_stats[Character == input$char_victim, Knockback.Adjustment])
  }) %>% 
    bindCache(input$hitbox,
              input$char_victim,
              input$damage)
  
  g <- reactive({ scaling_factor * char_stats[Character == input$char_victim, Hitstun.Gravity.Accel] }) %>% bindCache(input$char_victim)
  air_friction <- reactive({ scaling_factor * char_stats[Character == input$char_victim, Air.Friction] }) %>% bindCache(input$char_victim)
  drift <- reactive({ as.numeric(input$drift) * ifelse((input$reverse_hit), -1, 1) }) %>% bindCache(input$drift, input$reverse_hit)
  
  output$plot <- renderPlotly(stage_canvas())
  stage_traj_proxy <- plotlyProxy("plot", session)
  
  x0 <- reactive({ 
    nvl(input$clickposition[1], center_w) 
  }) %>% bindCache(input$clickposition)
  
  y0 <- reactive({ 
    if (input$autosnap == TRUE) {
      if (!is.na(snap_to_element())) { 
        make_stage_elements(input$stage)[[snap_to_element() + 1]]$y1 
      } else {
        nvl(input$clickposition[2], center_h) 
      }
    } else {
      nvl(input$clickposition[2], center_h) 
    }
    
  }) %>% bindCache(input$clickposition,
                   input$stage,
                   input$autosnap)
  
  x <- reactive({
    make_x(t_max = t_max(),
           scaling_factor = scaling_factor,
           v0x = v0() * cos(angles()[2]),
           drift = drift(),
           air_friction = air_friction())
  }) %>% 
    bindCache(input$hitbox,
              input$char_victim,
              input$damage,
              input$No_DI,
              input$DI,
              input$reverse_hit,
              input$drift)
  
  y <- reactive({
    make_y(t_max = t_max(),
           scaling_factor = scaling_factor,
           v0y = v0() * sin(angles()[2]),
           g = g())
  }) %>% 
    bindCache(input$hitbox,
              input$char_victim,
              input$damage,
              input$No_DI,
              input$DI,
              input$reverse_hit,
              input$drift)
  observe({
    input$stage
    plotlyProxyInvoke(
      stage_traj_proxy,
      "restyle",
      list(x = list(x0() + x()), y = list(y0() + y()))
      # marger.color = list('rgb(231, 99, 250)'),
      # text = 'test',
      # traceIndices = list(0, 1)
    )
  })
  
  # Right side outputs --------------------
  
  output$image <- renderUI({
    tags$img(src = glue("{input$char}/{input$char}_{input$tabs}.png"), width = '80%', height = '80%', style = 'text-align:middle;')
  })
  
  output$infocircle <- renderUI({
    tags$img(src = "infocircle_question_mark.png", width = 20, height = 20)
  })
  
  output$move_kills <- renderText({
    
    if (min(x0() + x()) < center_w - stages[[input$stage]][['ground']] - stages[[input$stage]][['side']] |
        max(x0() + x()) > center_w + stages[[input$stage]][['ground']] + stages[[input$stage]][['side']] |
        min(y0() + y()) < center_h - stages[[input$stage]][['bottom']]  |
        max(y0() + y()) > center_h + stages[[input$stage]][['top']] ) {
      '<font color="Tomato">Kills</font>'
    } else {
      'Does not kill'
    }
  })
  
  output$angle_text <- renderText({
    paste0('Launch angle: ', 
           ifelse(between(round(parsed_angles() %% 360), 
                          90,
                          270
           ), 
           180 - round(parsed_angles()),
           round(parsed_angles())
           )
           %% 360, "°")
  }) 
  
  output$velocity_text <- renderText({
    paste0('Launch velocity: ', round(v0() / scaling_factor, digits = 1), " pixel / frame")
  })
  
  output$hitstun_text <- renderText({
    paste0('Frames in hitstun: ', floor(t_max()))
  })
  
  output$DI_in_text <- renderText({
    paste0('Maximum DI in angle: ', 
           ifelse(between(round(parsed_angles() %% 360), 
                          90,
                          270
           ), 
           180 - round(parsed_angles()),
           round(parsed_angles())
           ) + 90,
           "°")
  })
  
  output$DI_out_text <- renderText({
    paste0('Maximum DI in angle: ', 
           (ifelse(between(round(parsed_angles() %% 360), 
                           90,
                           270
           ), 
           180 - round(parsed_angles()),
           round(parsed_angles())
           ) - 90) %% 360,
           "°")
  })
  
  output$grounded_text <- renderText({
    if (is_grounded()) 'Grounded hit' else 'Mid-air hit'
  })
  
  output$table <- renderDT(
    datatable(get(input$char)[, -'Moves'],
              filter = 'top', extensions = c('Buttons', 'Scroller', 'FixedColumns'),
              options = list(scrollY = 650,
                             scrollX = 500,
                             deferRender = TRUE,
                             scroller = TRUE,
                             # paging = TRUE,
                             # pageLength = 25,
                             buttons = list('excel',
                                            list(extend = 'colvis', targets = 0, visible = FALSE)),
                             dom = 'lBfrtip',
                             fixedColumns = list(leftColumns = 2),
                             autoWidth = TRUE
              )
    )
  )
  
}