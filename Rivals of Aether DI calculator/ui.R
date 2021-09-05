library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(data.table)
library(stringr)
library(openxlsx)
library(glue)
library(shinyXYpad)

header <- dashboardHeader()

sidebar <- dashboardSidebar(
  
  # fluidRow(column(12, offset = 1, h4('Attacker inputs'))),
  
  selectizeInput('char',
                 label = 'Attacker',
                 choices = chars,
                 selected = 'Zetterburn'),
  
  sidebarMenu(
    id = "tabs",
    menuItem("Jab", tabName = glue("jab")),
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
    menuItem("Aerials", tabName = glue("aerials"),
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
  
  selectizeInput('char_victim',
                 label = 'Victim',
                 choices = chars,
                 selected = 'Zetterburn'),
  
  fluidRow(column(12,
                  numericInput('damage',
                               label = '% (pre-hit)',
                               min = 0,
                               max = 999,
                               value = 100,
                               step = 1))
  ),
  
  knobInput('DI',
            label = 'DI angle',
            value = 0,
            max = 360,
            step = 5,
            angleOffset = 90,
            rotation = 'anticlockwise',
            width = '80%',
            height = '80%'),
  
  selectInput('drift',
              label = 'Drift DI',
              choices = c(None = 0, 
                          In = -1,
                          Out = 1)
  )
)

body <- dashboardBody(
  fluidPage(
    fluidRow(titlePanel('Rivals of Aether knockback calculator')),
    
    br(),
    
    fluidRow(selectizeInput('stages',
                            label = NULL,
                            choices = names(stages))
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
                   h4(textOutput('angle_text'))
                 )
          )
      )
    ),
    
    fluidRow(
      column(width = 8,
             align = 'center',
             fluidRow(
               XYpadInput("xy", label = "Control hit location", pointRadius = 5, 
                          x = "X", y = "Y",
                          value = list(x = 0, y = 0),
                          xmin = -1, xmax = 1,
                          ymin = -1, ymax = 1,
                          width = 150,
                          height = 150,
                          coordsColor = "orange", 
                          xyColor = "red", xySize = 14, xyStyle = "oblique")
             )
      )
    )
  )
)

ui = dashboardPage(header, sidebar, body)