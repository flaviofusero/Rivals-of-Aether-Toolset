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

header <- dashboardHeader(title = 'RoA KB Calculator')

sidebar <- dashboardSidebar(
  
  useShinyjs(),
  
  selectizeInput('char',
                 label = 'Attacker',
                 choices = chars,
                 selected = 'Zetterburn'),
  
  div(style = "margin-top:-15px"),
  
  sidebarMenu(
    id = "tabs",
    menuItem("Jab", tabName = glue("jab")),
    menuItem("Dash Attack", tabName = glue("da")),
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
  
  selectizeInput('hitbox',
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
  
  fluidRow(column(6,
                  numericInput('damage',
                               label = '% (pre-hit)',
                               min = 0,
                               max = 999,
                               value = 100,
                               step = 1)
  ),
  column(6,
         selectInput('drift',
                     label = 'Drift DI',
                     choices = c(No = 0, 
                                 In = -1,
                                 Out = 1)
         )
  )
  ),
  
  div(style = "margin-top:-15px"),
  
  fluidRow( 
    align = 'center',
    
    column(6, align = 'center',
           prettyCheckbox(
             inputId = "reverse_hit",
             label = "Reverse hit", 
             value = FALSE,
             status = "primary")
    ),
    
    column(6, align = 'center',
           prettyCheckbox(
             inputId = "No_DI",
             label = "No DI", 
             value = FALSE,
             status = "primary")
    )
  ),
  
  div(style = "margin-top:-15px"),
  
  fluidRow( 
    align = 'center',
    
    column(12, align = 'left',
           prettyCheckbox(
             inputId = "autosnap",
             label = "Autosnap to ground", 
             value = TRUE,
             status = "primary")
    )
  ),
  
  fluidRow(
    align = 'center',
    knobInput('DI',
              label = 'DI angle',
              value = 40,
              max = 360,
              step = 5,
              angleOffset = 90,
              rotation = 'anticlockwise',
              width = '80%',
              height = '80%')
  ),
  
  fluidRow(
    align = 'center',
    knobInput('DIaaaa',
              label = 'Damage (%)',
              value = 100,
              max = 999,
              step = 1,
              angleOffset = 90,
              rotation = 'anticlockwise',
              width = '80%',
              height = '80%')
  )
)

body <- dashboardBody(
  navbarPage( 
    title = 'Rivals of Aether Knockback Calculator',
    tabPanel('Calculator',
             
             tags$head(tags$style(HTML('
              .box-body {
                  padding-top: 0px;
                  margin-top: 0px;
                  padding-left: 0px;
                  margin-left: 0px;
                  padding-bottom: 0px;
                  margin-bottom: 0px;
              }'))),
             
             # fluidRow(titlePanel('Rivals of Aether Knockback Calculator')),
             # 
             # br(),
             
             fluidRow(selectizeInput('stage',
                                     label = NULL,
                                     choices = names(stages) %>% sort)
             ),
             
             fluidRow(
               box(width = 12,
                   align = 'center',
                   column(width = 8,
                          align = 'left',
                          plotlyOutput(outputId = "plot", 
                                       width = canvas_w, 
                                       height = canvas_h)
                   ),
                   
                   column(width = 4,
                          align = 'center',
                          br(),
                          fluidRow(
                            uiOutput(outputId = "image")
                          ),
                          fluidRow(
                            h3(htmlOutput('move_kills')),
                            br(),
                            h4(textOutput('angle_text')),
                            h4(textOutput('velocity_text')),
                            h4(textOutput('hitstun_text')),
                            h4(textOutput('DI_in_text')),
                            h4(textOutput('DI_out_text')),
                            h4(textOutput('grounded_text'))
                          )
                   )
               )
             )
    ),
    tabPanel('Character stats',
             DTOutput('table')
    )
  )
)

ui = dashboardPage(header, sidebar, body)