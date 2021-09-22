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

server = function(input, output, session) {
  
  source('js/click_anywhere.js')
  sourceCpp('cpp/utils.cpp')
  source('R/utils.R')
  source('R/make_stage_elements.R')
  source('R/assist_di.R')
  source('R/constants.R')
  source('server.R')
  source('ui.R')
  
  # Observes --------------------
  
  # Prevents Heroku's greying up after 55 seconds of inactivity
  observe({     
    invalidateLater(10000)     
    cat(".")   
  })
  
  observe({
    updateSelectInput(
      inputId = 'hitbox',
      choices = paste0(input$char,
                       '_',
                       selectable_hitboxes()) %>% 
        setNames(get_move_data(paste0(input$char, '_', input$tabs))[order(-Base.Knockback, -Knockback.Scaling), Ground.Moves])
    )
  })
  
  observe({
    toggle('DI', condition = !input$No_DI)
  })
  
  observe({
    toggle('omni_angle', condition = (paste0(input$char, '_', input$tabs) %in% omni_moves))
  })
  
  # Reactive values --------------------
  
  stage_elements <- reactive({ make_stage_elements(input$stage) }) %>% bindCache(input$stage)
  
  # We bind the canvas to the input$stage, since we only want to redraw a new canvas when the stage changes
  stage_canvas <- reactive({ draw_stage(stage_elements(),
                                        traj = list(x0 = x0(), 
                                                    x_in = x[['DI in']](),
                                                    x_custom = x[['Custom DI']](),
                                                    x_out =  x[['DI out']](),
                                                    y0 = y0(), 
                                                    y_in = y[['DI in']](),
                                                    y_custom = y[['Custom DI']](),
                                                    y_out =  y[['DI out']]())
  )
  }) %>% bindEvent(input$stage)
  
  snap_to_element <- reactive({ snap_to(elements = stage_elements()[-1], 
                                        x = input$clickposition[1],
                                        y = input$clickposition[2],
                                        snap_tol = snap_tol)
  }) %>% bindCache(input$stage, input$clickposition)
  
  is_grounded <- reactive({ 
    if (!is.na(snap_to_element()) & input$autosnap == TRUE) {
      TRUE
    } else { FALSE }
  }) %>% 
    bindCache(input$stage,
              input$clickposition,
              input$autosnap)
  
  char_moves <- reactive({
    get_move_data(paste0(input$char, '_', input$tabs))
  }) %>% bindCache(input$char, input$tabs)
  
  selectable_hitboxes <- reactive({
    char_moves()[order(-Base.Knockback, -Knockback.Scaling), Ground.Moves]
  }) %>% bindCache(input$char, input$tabs)
  
  selected_hitbox <- reactive({ 
    get(strsplit(input$hitbox, '_', fixed = TRUE)[[1]][1])[Ground.Moves == strsplit(input$hitbox, '_', fixed = TRUE)[[1]][2]]
  }) %>% 
    bindCache(input$hitbox)
  # When the char changes, there is a split moment when the hitbox is not updated
  # This is bad because then e.g. going from Zetter > fair (sweetpot) to Ori > fair (sweetspot) causes an error as there is no such move
  # We only care about the hitbox anyway so we can let the move update according to that
  
  BKB <- reactive({ nvl(selected_hitbox()$Base.Knockback, 0) }) 
  KBS <- reactive({ nvl(selected_hitbox()$Knockback.Scaling, 0) }) 
  hitbox_damage <- reactive({ nvl(selected_hitbox()$Damage, 0) }) 
  
  parsed_angle <- reactive ({ 
    parse_angle(selected_hitbox(), is_grounded()) 
  }) %>% bindCache(input$hitbox,
                   input$autosnap,
                   input$clickposition,
                   input$stage)
  
  angles <- reactive({
    angle <- parsed_angle()
    if (between(angle, 90, 270)) {angle <- (180 - angle) %% 360}
    if (paste0(input$char, '_', input$tabs) %in% omni_moves) {angle <- nvl(input$omni_angle, 0)}
    
    DI_offsets = 18 * c('DI out' = -1, 
                        'Custom DI' = ifelse(input$No_DI, 0, assist_di(angle, input$DI, v0())), 
                        'DI in' = 1)
    
    angles = angle + DI_offsets %>% 
      setNames(c('DI out', 'Custom DI', 'DI in'))
    
    if (input$reverse_hit) {angles <- (180 - angles) %% 360}
    return((pi / 180) * angles)
  }) %>%
    bindCache(input$autosnap,
              input$clickposition,
              input$stage,
              input$char,
              input$omni_angle,
              input$hitbox,
              input$No_DI,
              input$DI,
              input$reverse_hit)
  
  v0 <- reactive({
    armor_multiplier <- 1 - 0.3 * (tolower(input$char_victim) == 'etalus (armor)')
    
    v0 <- scaling_factor * armor_multiplier *
      (BKB() + KBS() * 
         char_stats[Character == input$char_victim, Knockback.Adjustment] * 
         (input$damage + hitbox_damage()) * 0.12)
    
    return(v0)
  }) %>% 
    bindCache(input$hitbox,
              input$char_victim,
              input$damage)
  
  t_max <- reactive({
    nvl(selected_hitbox()$Hitstun.Modifier, 0) * 
      (BKB() * 4 * ((char_stats[Character == input$char_victim, Knockback.Adjustment] - 1) * 0.6 + 1) +
         (input$damage + hitbox_damage()) * 0.12 * KBS() * 4 * 0.65 * 
         char_stats[Character == input$char_victim, Knockback.Adjustment]) %>% 
      floor
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
    # autosnap to (0,0)
    if (input$autosnap == TRUE &
        between(nvl(input$clickposition[1], center_w), -(center_w  + snap_tol), center_w + snap_tol) & 
        between(nvl(input$clickposition[2], center_w), center_h, center_h + snap_tol)) {
      0
    } else {
      nvl(input$clickposition[1], center_w)
    }
  }) %>% bindCache(input$clickposition,
                   input$autosnap)
  
  y0 <- reactive({ 
    if (input$autosnap == TRUE) {
      
      if (between(nvl(input$clickposition[1], center_w), -(center_w  + snap_tol), center_w + snap_tol) & 
          between(nvl(input$clickposition[2], center_w), center_h, center_h + snap_tol)) {
        0
      } else if (!is.na(snap_to_element())) { 
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
  
  # x, y and plot --------------------
  x <- reactiveValues()
  x[['DI out']] = reactive({
    nvl(make_x(t_max = t_max(),
               scaling_factor = scaling_factor,
               v0x = v0() * cos(angles()['DI out']),
               drift = drift(),
               air_friction = air_friction()), rep(center_w, t_max()))
  }) %>% 
    bindCache(input$hitbox,
              input$char_victim,
              input$damage,
              input$No_DI,
              input$DI,
              input$omni_angle,
              input$reverse_hit,
              input$drift)
  
  x[['Custom DI']] = reactive({
    nvl(make_x(t_max = t_max(),
               scaling_factor = scaling_factor,
               v0x = v0() * cos(angles()['Custom DI']),
               drift = drift(),
               air_friction = air_friction()), rep(center_w, t_max()))
  }) %>% 
    bindCache(input$hitbox,
              input$char_victim,
              input$damage,
              input$No_DI,
              input$DI,
              input$omni_angle,
              input$reverse_hit,
              input$drift)
  
  x[['DI in']] = reactive({
    nvl(make_x(t_max = t_max(),
               scaling_factor = scaling_factor,
               v0x = v0() * cos(angles()['DI in']),
               drift = drift(),
               air_friction = air_friction()), rep(center_w, t_max()))
  }) %>% 
    bindCache(input$hitbox,
              input$char_victim,
              input$damage,
              input$No_DI,
              input$DI,
              input$omni_angle,
              input$reverse_hit,
              input$drift)
  
  
  y <- reactiveValues()
  
  y[['DI out']] = reactive({
    nvl(make_y(t_max = t_max(),
               scaling_factor = scaling_factor,
               v0y = v0() * sin(angles()['DI out']),
               g = g()), rep(center_h, t_max()))
  }) %>% 
    bindCache(input$hitbox,
              input$char_victim,
              input$damage,
              input$No_DI,
              input$DI,
              input$omni_angle,
              input$reverse_hit,
              input$drift)
  
  y[['Custom DI']] = reactive({
    nvl(make_y(t_max = t_max(),
               scaling_factor = scaling_factor,
               v0y = v0() * sin(angles()['Custom DI']),
               g = g()), rep(center_h, t_max()))
  }) %>% 
    bindCache(input$hitbox,
              input$char_victim,
              input$damage,
              input$No_DI,
              input$DI,
              input$omni_angle,
              input$reverse_hit,
              input$drift)
  
  y[['DI in']] = reactive({
    nvl(make_y(t_max = t_max(),
               scaling_factor = scaling_factor,
               v0y = v0() * sin(angles()['DI in']),
               g = g()), rep(center_h, t_max()))
  }) %>% 
    bindCache(input$hitbox,
              input$char_victim,
              input$damage,
              input$No_DI,
              input$DI,
              input$omni_angle,
              input$reverse_hit,
              input$drift)
  
  observe({
    input$stage
    plotlyProxyInvoke(
      stage_traj_proxy,
      "restyle",
      list(
        x = list(
          x0() + x[['DI in']](),
          x0() + x[['Custom DI']](),
          x0() + x[['DI out']]()
        ), 
        y = list(
          y0() + y[['DI in']](),
          y0() + y[['Custom DI']](),
          y0() + y[['DI out']]()
        ),
        customdata = list(as.list(0:t_max())),
        hovertemplate = list(paste0('(%{x:.0f}, %{y:.0f})<br>Frame: %{customdata}'))
      ),
      c(1, 2, 3)
    )
  })
  
  # Right side outputs --------------------
  
  output$image <- renderUI({
    tags$img(src = glue("{input$char}/{input$char}_{input$tabs}.png"), width = '80%', height = '80%', style = 'text-align:middle;')
  })
  
  output$infocircle <- renderUI({
    tags$img(src = "infocircle_question_mark.png", width = 20, height = 20)
  })
  
  output$selected_hitbox_kills <- renderText({
    if (min(x0() + x[['Custom DI']]()) < center_w - stages[[input$stage]][['ground']] - stages[[input$stage]][['side']] |
        max(x0() + x[['Custom DI']]()) > center_w + stages[[input$stage]][['ground']] + stages[[input$stage]][['side']] |
        min(y0() + y[['Custom DI']]()) < center_h - stages[[input$stage]][['bottom']]  |
        max(y0() + y[['Custom DI']]()) > center_h + stages[[input$stage]][['top']] ) {
      '<font color="Tomato">Kills</font>'
    } else {
      'Does not kill'
    }
  })
  
  output$angle_text <- renderText({
    validate(need(BKB() != 0, ''))
    paste0('Launch angle: ', 
           ifelse(between(round(parsed_angle() %% 360), 
                          90,
                          270
           ), 
           180 - round(parsed_angle()),
           round(parsed_angle())
           )
           %% 360, "°")
  }) 
  
  output$velocity_text <- renderText({
    validate(
      need(BKB() != 0, '') 
    )
    paste0('Launch velocity: ', round(v0() / scaling_factor, digits = 1), " pixel / frame")
  })
  
  output$hitstun_text <- renderText({
    validate(need(BKB() != 0, ''))
    paste0('Frames in hitstun: ', floor(t_max()))
  })
  
  output$DI_in_text <- renderText({
    validate(need(BKB() != 0, ''))
    paste0('Maximum DI in angle: ', 
           ifelse(between(round(parsed_angle() %% 360), 
                          90,
                          270
           ), 
           180 - round(parsed_angle()),
           round(parsed_angle())
           ) + 90,
           "°")
  })
  
  output$DI_out_text <- renderText({
    validate(need(BKB() != 0, ''))
    paste0('Maximum DI out angle: ', 
           (ifelse(between(round(parsed_angle() %% 360), 90, 270), 180 - round(parsed_angle()), round(parsed_angle())
           ) - 90) %% 360,
           "°")
  })
  
  output$grounded_text <- renderText({
    validate(need(BKB() != 0, ''))
    if (is_grounded()) 'Grounded hit' else 'Mid-air hit'
  })
  
  output$armor <- renderText({
    validate(need(hitbox_damage() != 0, ''))
    
    paste0("Breaks Eta's fair armor at (No Ice Armor / Ice Armor): ",
           round((12 - BKB()) / 
                   (KBS() * 0.12 * 0.9) - 
                   hitbox_damage()),
           '% / ',
           round((12 - 0.7 * BKB()) / 
                   (KBS() * 0.7 * 0.12 * 0.9) -
                   hitbox_damage()),
           '%'
    )
  })
  
  output$armor <- renderText({
    validate(need(hitbox_damage() != 0, ''))
    
    paste0("Breaks Eta's fair armor at (No Ice Armor / Ice Armor): ",
           round((12 - BKB()) / 
                   (KBS() * 0.12 * 0.9) - 
                   hitbox_damage()),
           '% / ',
           round((12 - 0.7 * BKB()) / 
                   (KBS() * 0.7 * 0.12 * 0.9) -
                   hitbox_damage()),
           '%'
    )
  })
  
  output$notes <- renderText({
    validate(need(!is.na(selected_hitbox()$Notes), ''))
    selected_hitbox()$Notes
  })
  
  output$move_data <- renderDT({
    cols <- c('Ground.Moves', 'Startup', 'Active.Frames', 'Endlag.(Hit)', 'Endlag.(Whiff)', 'FAF', 'Damage', 'Landing.Lag.(Hit)',
              'Landing.Lag.(Whiff)', 'Cooldown')
    
    move_data <- char_moves()[Ground.Moves %in% selectable_hitboxes(), intersect(cols, colnames(char_moves())), with = F]
    move_data <- move_data[, colSums(is.na(move_data)) < nrow(move_data), with = FALSE] # Returns only column with at least one non-NA value
    untidy_cols <- colnames(move_data)
    tidy_cols <- gsub('.', ' ', gsub('Ground.Moves', 'Move', untidy_cols,  fixed = T), fixed = T)
    setnames(move_data, old = untidy_cols, new = tidy_cols)
    datatable(move_data, options = list(dom = 't', paging = FALSE, ordering = FALSE))
  })
  
  
  # Characters stats -------------------
  
  output$table <- renderDT(
    datatable(get(input$char)[, -'Moves'],
              filter = 'top', extensions = c('Buttons', 'Scroller', 'FixedColumns'),
              options = list(scrollY = 650,
                             scrollX = 500,
                             deferRender = TRUE,
                             scroller = TRUE,
                             # paging = TRUE,
                             # pageLength = 25,
                             buttons = list(list(extend = 'colvis', targets = 0, visible = FALSE)),
                             dom = 'lBfrtip',
                             fixedColumns = list(leftColumns = 2),
                             autoWidth = TRUE
              )
    )
  )
  
  # Credits -------------------
  
  output$credits <- renderUI({
    fd <- 'https://docs.google.com/spreadsheets/d/19UtK7xG2c-ehxdlhCFKMpM4_IHSG-EXFgXLJaunE79I/edit?usp=sharing'
    stats <- 'https://docs.google.com/spreadsheets/d/14JIjL_5t81JHqnJmU6BSsRosTe2JO8sFGUysM_9tDoU/edit#gid=1576686769'
    igl_tool <- 'https://jsfiddle.net/IGLima/5sh0pudr/show/'
    
    x <- glue({'
    <p>Tool by Vincent46</p>
    <p>Kill % may be +/- 1% off due to rounding</p>
    <br>
    <p>Input data taken from the following resources (not by me):</p>
    <p><a href="{fd}">Rivals of Aether Academy Frame Data</a> - Data extracted manually in-game and from dev-mode files by SNC. Extra information provided by Menace13 and Youngblood. General Stats created by Kisuno. Collated Patch Notes created by SNC</p>
    <p><a href="{fd}">Rivals of Aether General Stats</a> - Data extracted from devmode files and formatted by Kisuno. Info provided by Menace13, Youngblood and SNC</p>
    <p>Thanks to IGL for answering my questions about knockback formulas, directly on Sector 7-G\'s discord and indirectly via his 
    <a href="{igl_tool}">Knockback Visualizer tool</a></p>
    '})
    
    HTML(x)
  })
  
}