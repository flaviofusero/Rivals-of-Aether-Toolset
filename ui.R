# Header --------------------

header <- dashboardHeader(title = #'RoA Toolset', 
                            shinyDashboardLogoDIY(
                              boldText = "RoA",
                              mainText = "toolset",
                              badgeTextSize = 0,
                              badgeText = NULL,
                              badgeBackColor = NULL,
                              badgeTextColor = NULL
                            ),
                          titleWidth = 350)

# Sidebar --------------------

sidebar <- dashboardSidebar(
  width = 350,
  minified = FALSE,
  
  useShinyjs(),
  
  pickerInput('char',
              label = 'Attacker',
              choices = chars,
              selected = 'Zetterburn',
              choicesOpt = list(content = icons_chars,
                                style = rep(("padding-left: 10px; color: black; background: lightgrey;"), length(chars)))
  ),
  
  #   fluidRow(
  #   img(id="Zetterburn_test",src="icons/Zetterburn.png",width="25px",style="cursor:pointer; margin-left: 25px;"),
  #   img(id="Forsburn_test",src="icons/Forsburn.png",width="25px",style="cursor:pointer;"),
  #   img(id="Clairen_test",src="icons/Clairen.png",width="25px",style="cursor:pointer;")
  # ),
  
  div(style = "margin-top:-15px"),
  
  sidebarMenu(
    id = "tabs",
    menuItem("Jab", tabName = glue("jab")),
    menuItem("Dash Attack", tabName = glue("dashattack")),
    menuItem("Tilts", tabName = glue("Tilts"),
             menuSubItem("Ftilt", tabName = glue("ftilt")),
             menuSubItem("Utilt", tabName = glue("utilt")),
             menuSubItem("Dtilt", tabName = glue("dtilt"))
    ),
    menuItem("Strongs", tabName = glue("strongs"),
             menuSubItem("Fstrong", tabName = glue("fstrong")),
             menuSubItem("Ustrong", tabName = glue("ustrong")),
             menuSubItem("Dstrong", tabName = glue("dstrong"))
    ),
    menuItem("Aerials", tabName = glue("aerials"), startExpanded = TRUE,
             menuSubItem("Nair", tabName = glue("nair")),
             menuSubItem("Fair", tabName = glue("fair"), selected = TRUE),
             menuSubItem("Bair", tabName = glue("bair")),
             menuSubItem("Uair", tabName = glue("uair")),
             menuSubItem("Dair", tabName = glue("dair"))
    ),
    menuItem("Specials", tabName = glue("Specials"),
             menuSubItem("Nspecial", tabName = glue("nspecial")),
             menuSubItem("Fspecial", tabName = glue("fspecial")),
             menuSubItem("Uspecial", tabName = glue("uspecial")),
             menuSubItem("Dspecial", tabName = glue("dspecial"))
    )
  ),
  
  selectInput('hitbox',
              label = 'Hitbox',
              choices = c('Fair (Sweetspot)' = 'Zetterburn_Fair (Sweetspot)', 
                          'Fair' = 'Zetterburn_Fair', 
                          'Fair (Sour)' = 'Zetterburn_Fair (Sour)'),
              multiple = FALSE),
  
  div(style = "margin-top:-15px"),
  
  pickerInput('char_victim',
              label = 'Victim',
              choices = chars_victim,
              selected = 'Zetterburn',
              choicesOpt = list(content = icons_chars_victim,
                                style = rep(("padding-left: 10px; color: black; background: lightgrey;"), length(chars_victim)))),
  
  div(style = "margin-top:-15px"),
  
  # fluidRow(
  #   algin = 'center',
  #   column(12,
  #          selectInput('drift',
  #                      label = 'Drift DI',
  #                      choices = c(No = 0, 
  #                                  In = -1,
  #                                  Out = 1))
  #   )
  # ),
  # 
  # div(style = "margin-top:-15px"),
  
  radioGroupButtons(
    inputId = "drift",
    label = "Drift DI", 
    choices = c("In" = -1, "None" = 0, "Out" = 1),
    status = "primary"
  ),
  
  # div(style = "margin-top:-15px"),
  
  fluidRow( 
    align = 'center',
    
    column(4, align = 'center',
           style = 'margin-right:0px;',
           prettyCheckbox(
             inputId = "autosnap",
             label = "Snap to ground", 
             value = TRUE,
             status = "primary")
    ),
    
    column(4, align = 'center',
           style = 'margin-right:-2px; margin-left: 7px;',
           prettyCheckbox(
             inputId = "reverse_hit",
             label = "Reverse hit", 
             value = FALSE,
             status = "primary")
    ),
    
    column(4, align = 'center',
           style = 'padding-right:0px; margin-left:-5px;',
           prettyCheckbox(
             inputId = "No_DI",
             label = "No DI",
             value = FALSE,
             status = "primary")
    )
  ),
  
  div(style = "margin-top:-10px"),
  
  fluidRow(
    column(6,
           align = 'center',
           style='padding-left:20px; padding-right: 0px;',
           knobInput('damage',
                     label = '% (pre-hit)',
                     value = 100,
                     max = 999,
                     step = 1,
                     angleOffset = 90,
                     rotation = 'anticlockwise',
                     post = '%',
                     width = '100%')
    ),
    column(6,
           align = 'center',
           style='padding-right:20px; padding-left: 0px;',
           knobInput('DI',
                     label = 'DI angle',
                     value = 40,
                     max = 360,
                     step = 1,
                     angleOffset = 90,
                     rotation = 'anticlockwise',
                     width = '100%')
    )
    
  ),
  fluidRow(
    column(12,
           style = 'margin-top: -15px;',
           hidden(numericInput('omni_angle',
                               label = 'Override attack angle (0 <= x < 360)',
                               value = 45,
                               min = 0,
                               max = 360,
                               step = 1))
    )
  )
)

# Body  --------------------

body <- dashboardBody(
  
  tags$head(tags$style(HTML('.box{box-shadow: 0 5px 10px rgba(154,160,185,.05), 0 15px 40px rgba(166,173,201,.2)};'))),
  
  mainPanel(
    width = 12,
    tabsetPanel(
      tabPanel('Knockback',
               br(),
               fluidRow(
                 column(
                   width = 9,
                   style='margin-left: -14px;', #-28px;',
                   align = 'left',
                   box(
                     width = 12,
                     collapsible = TRUE,
                     headerBorder = F,
                     status = 'primary',
                     style = 'margin-top:-32px;',
                     align = 'center',
                     fluidRow(
                       column(3,
                              align='left',
                              selectizeInput('stage',
                                             label = NULL,
                                             choices = names(stages) %>% sort)
                       ),
                       column(8, 
                              offset = 1,
                              align = 'left',
                              h4('Click anywhere on the chart to move the trajectory')
                       )
                     ),
                     fluidRow(
                       column(8,
                              offset = 4,
                              align = 'left',
                              style = 'margin-top: -20px; padding-left: 35px',
                              h5('DI and % can be input via mouse, keyboard or mouse wheel')
                       )
                     ),
                     div(style = "margin-top:-11px"), # any more than this and the "y" above is cut off
                     plotlyOutput(outputId = "plot" ,
                                  width = '100%',
                                  height = 600
                     )
                   ),
                   box(width = 12,
                       style = 'margin-top:-32px;',
                       status = 'primary',
                       headerBorder = F,
                       DTOutput('move_data')
                   )
                 ),
                 
                 column(width = 3,
                        # align = 'center',
                        box(width = 12,
                            style='padding-right:30px; padding-left:30px; margin-top:-32px;',
                            status = 'primary',
                            headerBorder = F,
                            fluidRow(
                              uiOutput(outputId = "image", style = 'text-align: center;')
                            ),
                            fluidRow(
                              h3(htmlOutput('selected_hitbox_kills', style = 'text-align: center;')),
                              br(),
                              htmlOutput('angle_text'),
                              bsTooltip('angle_text', 
                                        title = 'Knockback angle of the move relative to the x axis.<br><br>The Sakurai Angle is expressed as 361°. A move with this property will send opponents 40 if they are grounded and 45 otherwise.', 
                                        placement = 'left'),
                              
                              h4(textOutput('velocity_text')),
                              bsTooltip('velocity_text', 
                                        title = 'Launch velocity is given by the knockback formula:<br><br>BKB + damage (post-hit) * KBS * 0.12 * Knockback Adjustment<br><br>Etalus ice armor decreases the result by 30%.',
                                        placement = 'left'),
                              
                              h4(textOutput('hitstun_text')),
                              bsTooltip('hitstun_text', 
                                        title = 'Total hitstun frames are given by the hitstun formula:<br><br>Histun modifier * (BKB * 4 * ((Knockback Adjustment - 1) * 0.6 + 1) + damage (post-hit) * 0.12 * KBS * 4 * 0.65 * Knockback Adjustment)<br><br>Result is rounded down to the nearest integer.',
                                        placement = 'left'),
                              
                              
                              h4(textOutput('DI_in_text')),
                              bsTooltip('DI_in_text', 
                                        title = 'DI can alter the launch angle by up to 18°. Maximum effect is given when DI is perpendicular to launch angle.<br><br>RoA has a DI assist future: angles within 22° (incl.) from perpendicular still count as full DI in/out.',
                                        placement = 'left'),
                              
                              h4(textOutput('DI_out_text')),
                              bsTooltip('DI_out_text', 
                                        title = 'DI can alter the launch angle by up to 18°. Maximum effect is given when DI is perpendicular to launch angle.<br><br>RoA has a DI assist future: angles within 22° (incl.) from perpendicular still count as full DI in/out.',
                                        placement = 'left'),
                              
                              h4(textOutput('grounded_text')),
                              bsTooltip('grounded_text', 
                                        title = 'If the "Snap to ground" option is active, clicking slightly above the ground will autosnap the trajectory to the below platform. Also relevant for moves with Sakurai angle.',
                                        placement = 'left'),
                              
                              h4(textOutput('armor')),
                              bsTooltip('armor', 
                                        title = 'No armor: breaks at (12 - BKB) / (KBS * 0.12 * 0.9)<br><br>Ice armor: breaks at (12 - 0.7 * BKB) / (KBS * 0.7 * 0.12 * 0.9)<br><br>Damage is post-hit and the final result is rounded to the nearest integer.',
                                        placement = 'left'),
                              
                              div(style = 'padding-top: 5px;'),
                              
                              h5(htmlOutput('notes'))
                            )
                        )
                 )
               )
      ),
      
      tabPanel('Character stats',
               br(),
               box(width = 12,
                   style='margin-top:-32px;',
                   status = 'primary',
                   headerBorder = F,
                   p('Hover over a cell to see its full content'),
                   DTOutput('table')
               )
      ),
      
      tabPanel('About & Feedback',             
               br(),
               box(
                 width = 6,
                 style='margin-top:-32px;',
                 status = 'primary',
                 headerBorder = F,
                 fluidRow(htmlOutput('credits'),
                          style = 'margin-left: 0px;')
               ),
               box(6,
                   align= 'center',
                   style='margin-top:-32px;',
                   status = 'primary',
                   headerBorder = F,
                   tags$iframe(src = 'https://forms.gle/6mRDH1QQyTEYwUxV7',
                               width = '100%',
                               height = 600,
                               frameborder = 0,
                               marginheight = 0)
               )
      )
    )
  )
)

ui = dashboardPage(header, sidebar, body)