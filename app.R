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

source('R/utils.R')
source('R/make_stage_elements.R')
source('R/constants.R')
source('js/click_anywhere.js')
sourceCpp('cpp/utils.cpp')
source('server.R')
source('ui.R')

runApp() 

# rsconnect::deployApp()


