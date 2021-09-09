library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(data.table)
library(stringr)
library(openxlsx)
library(glue)

source('R/utils.R')
source('R/constants.R')
source('server.R')
source('ui.R')

runApp()

# rsconnect::deployApp()