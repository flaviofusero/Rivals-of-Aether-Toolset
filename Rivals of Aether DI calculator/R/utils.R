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

nvl <- function(x, y) {
  if (isTRUE(is.na(x)) | isTRUE(is.null(x))) {
    y 
  } else { x }
}

parse_char_moves_data <- function(char) {
  char_moves_data <- as.data.table(readWorkbook('input/Rivals of Aether Academy Frame Data - Updated for 2.0.7.0.xlsx',
                                                sheet = char,
                                                rows = 2:100))
  
  char_moves_data[, Moves := str_replace_all(tolower(Ground.Moves), '-', ' ') %>% 
                    str_replace_all(' ', '') %>% 
                    str_replace_all('down', 'd') %>% 
                    str_replace_all('up', 'u') %>% 
                    str_replace_all('forward', 'f') %>% 
                    str_replace_all('back', 'b')
                  ]
  
  cols_to_num <- c('Base.Knockback', 'Knockback.Scaling', 'Angle', 'Damage')
  char_moves_data[ , (cols_to_num) := lapply(.SD, function(x) {
    as.numeric(nvl(x, -1))
  }), .SDcols = cols_to_num] 
  char_moves_data[, Knockback.Scaling := Knockback.Scaling / 100]
  
  return(char_moves_data)
}

parse_char_stats <- function() {
  char_stats <- as.data.table(readWorkbook('input/RoA General Stats.xlsx',
                                           sheet = 'All Stats',
                                           rows = 3:(3+length(chars_victim))))
  
  cols_to_num <- setdiff(colnames(char_stats), 'Character')
  char_stats[ , (cols_to_num) := lapply(.SD, function(x) {
    as.numeric(x)
  }), .SDcols = cols_to_num] 
  
  return(char_stats)
}

get_move_data <- function(char_move) {
  move_data <- get(unlist(strsplit(char_move, '_'))[1])[Moves %like% paste0('.*', unlist(strsplit(char_move, '_'))[2], '.*')]
}
