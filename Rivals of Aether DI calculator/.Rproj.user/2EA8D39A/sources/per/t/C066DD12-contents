library(shiny)
library(shinydashboard)
library(plotly)

header <- dashboardHeader()

sidebar <- dashboardSidebar(
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Zetterburn", icon = icon("bar-chart-o"),
             menuItem("Jab", tabName = "subitem1"),
             menuItem("Tilts", tabName = "subitem2"),
             menuItem("Strong_attacks", tabName = "subitem3"),
             menuItem("Aerials", tabName = "subitem4",
                      menuItem("Nair", tabName = "zet_nair"),
                      menuItem("Fair", tabName = "zet_fair"
                               # ,
                               # uiOutput(outputId = "image")   
                      ),
                      menuItem("Bair", tabName = "zet_bair"),
                      menuItem("Uair", tabName = "zet_uair"),
                      menuItem("Dair", tabName = "zet_dair")
             ),
             menuItem("Specials", tabName = "subitem5")
    ),
    uiOutput(outputId = "image")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("zet_fair",
            plotlyOutput(outputId = "plot")
    )
  )
)

ui = dashboardPage(header, sidebar, body)