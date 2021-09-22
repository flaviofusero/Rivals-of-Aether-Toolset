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

header <- dashboardHeader(title = 'RoA Toolset', 
                          titleWidth = 350)

sidebar <- dashboardSidebar(
  width = 350,
  
  useShinyjs(),
  
  selectInput('char',
              label = 'Attacker',
              choices = chars,
              selected = 'Zetterburn'),
  
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
  
  selectizeInput('char_victim',
                 label = 'Victim',
                 choices = chars_victim,
                 selected = 'Zetterburn'),
  
  div(style = "margin-top:-15px"),
  
  fluidRow(
    algin = 'center',
    column(12,
           selectInput('drift',
                       label = 'Drift DI',
                       choices = c(No = 0, 
                                   In = -1,
                                   Out = 1))
    )
  ),
  
  div(style = "margin-top:-15px"),
  
  fluidRow(
  ),
  
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
           style = 'margin-right:-2px;',
           prettyCheckbox(
             inputId = "reverse_hit",
             label = "Reverse hit", 
             value = FALSE,
             status = "primary")
    ),
    
    column(4, align = 'center',
           style = 'padding-right:0px; margin-left:-2px;',
           prettyCheckbox(
             inputId = "No_DI",
             label = "No DI",
             value = FALSE,
             status = "primary")
    )
  ),
  
  div(style = "margin-top:-15px"),
  div(style = "margin-top:-15px"),
  
  fluidRow(
    column(6,
           align = 'center',
           style='padding-left:15px;',
           knobInput('damage',
                     label = '% (pre-hit)',
                     value = 100,
                     max = 999,
                     step = 1,
                     angleOffset = 90,
                     rotation = 'anticlockwise',
                     post = '%',
                     width = '100%',
                     height = '100%')
    ),
    column(6,
           align = 'center',
           style='padding-right:15px;',
           knobInput('DI',
                     label = 'DI angle',
                     value = 40,
                     max = 360,
                     step = 1,
                     angleOffset = 90,
                     rotation = 'anticlockwise',
                     width = '100%',
                     height = '100%')
    )
    
  )
)

body <- dashboardBody(
  navbarPage( 
    title = 'Rivals of Aether Toolset',
    tabPanel('Knockback',
             
             # tags$head(tags$style(HTML('
             #  .box-body {
             #      padding-top: 0px;
             #      margin-top: 0px;
             #      padding-left: 0px;
             #      margin-left: 0px;
             #      padding-bottom: 0px;
             #      margin-bottom: 0px;
             #  }
             #                           
             #                           '))),
             
             fluidRow(selectizeInput('stage',
                                     label = NULL,
                                     choices = names(stages) %>% sort)
             ),
             
             fluidRow(
               column(width = 9,
                      align = 'left',
                      box(width = 12,
                          align = 'center',
                          h4('Click anywhere on the chart to move the trajectory'),
                          h5('DI and % can be input via mouse, keyboard or mouse wheel'),
                          div(style = "margin-top:-10px"),
                          plotlyOutput(outputId = "plot" ,
                                       width = '100%',
                                       height = 600
                          )
                      ),
                      box(width = 12,
                          DTOutput('move_data')
                      )
               ),
               
               column(width = 3,
                      # align = 'center',
                      box(width = 12,
                          style='padding-right:30px; padding-left:30px;',
                          fluidRow(
                            uiOutput(outputId = "image", style = 'text-align: center;')
                          ),
                          fluidRow(
                            h3(htmlOutput('selected_hitbox_kills', style = 'text-align: center;')),
                            br(),
                            h4(textOutput('angle_text')),
                            h4(textOutput('velocity_text')),
                            h4(textOutput('hitstun_text')),
                            h4(textOutput('DI_in_text')),
                            h4(textOutput('DI_out_text')),
                            h4(textOutput('grounded_text')),
                            h4(textOutput('armor')),
                            h5(textOutput('notes'))
                          )
                      )
               )
             )
    ),
    
    tabPanel('Character stats',
             DTOutput('table')
    ),
    
    tabPanel('About & Feedback',
             fluidRow(htmlOutput('credits')),
             fluidRow(column(6,
                             offset = 3,
                             align= 'center',
                             br(),
                             tags$iframe(src = 'https://forms.gle/6mRDH1QQyTEYwUxV7',
                                         width = '100%',
                                         height = 600,
                                         frameborder = 0,
                                         marginheight = 0)
             ))
    )
  )
)

ui = dashboardPage(header, sidebar, body)