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

header <- dashboardHeader(title = 'RoA KB Calculator')

sidebar <- dashboardSidebar(
  
  # fluidRow(column(12, offset = 1, h4('Attacker inputs'))),
  
  useShinyjs(),
  
  selectizeInput('char',
                 label = 'Attacker',
                 choices = chars,
                 selected = 'Zetterburn'),
  
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
                 choices = c('Fair (Sweetspot)', 'Fair', 'Fair (Sour)'),
                 multiple = FALSE),
  
  selectizeInput('char_victim',
                 label = 'Victim',
                 choices = chars_victim,
                 selected = 'Zetterburn'),
  
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
  )
)

body <- dashboardBody(
  fluidPage( 
    title = 'Rivals of Aether Knockback Calculator',
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
                 align = 'center',
                 plotlyOutput(outputId = "plot", width = canvas_w, height = canvas_h)
          ),
          
          column(width = 4,
                 align = 'center',
                 fluidRow(
                   uiOutput(outputId = "image")
                 ),
                 fluidRow(
                   h3(htmlOutput('move_kills')),
                   br(),
                   h4(textOutput('angle_text')),
                   h4(textOutput('velocity_text')),
                   h4(textOutput('DI_in_text')),
                   h4(textOutput('DI_out_text'))
                 )
          )
      )
    ),
    
    fluidRow(
      column(width = 8,
             align = 'center',
             # fluidRow(
             #   XYpadInput("xy", label = "Control hit location", pointRadius = 5, 
             #              x = "X", y = "Y",
             #              value = list(x = 0, y = 0),
             #              xmin = -1, xmax = 1,
             #              ymin = -1, ymax = 1,
             #              width = 150,
             #              height = 150,
             #              coordsColor = "orange", 
             #              xyColor = "red", xySize = 14, xyStyle = "oblique",
             #              onMove = TRUE)
             # ),
             
             fluidRow(
               column(6,
                      align = 'right',
                      numericInput('x0',
                                   label = 'Control hit location - X',
                                   min = -100,
                                   max = 100,
                                   value = 0,
                                   step = 1)
               ),
               # column(2,
               #        align = 'center',
               #        actionBttn('reset_xy',
               #                   label = 'Reset to (0, 0)',
               #                   style = 'simple',
               #                   color = 'success')
               # ),
               column(6,
                      align = 'left',
                      numericInput('y0',
                                   label = 'Control hit location - Y',
                                   min = -100,
                                   max = 100,
                                   value = 0,
                                   step = 1)
               )
             )
      )
    )
  )
)

ui = dashboardPage(header, sidebar, body)