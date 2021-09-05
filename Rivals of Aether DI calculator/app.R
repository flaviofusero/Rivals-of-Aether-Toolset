library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(data.table)
library(stringr)
library(openxlsx)
library(glue)
library(shinyXYpad)

source('R/utils.R')
source('R/constants.R')
source('server.R')
source('ui.R')

runApp()

# rsconnect::deployApp()