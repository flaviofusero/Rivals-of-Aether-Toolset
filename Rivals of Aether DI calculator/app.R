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

source('R/utils.R')
source('R/constants.R')
source('server.R')
source('ui.R')
sourceCpp('cpp/utils.cpp')

runApp() 
# rsconnect::deployApp()


