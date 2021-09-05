library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(data.table)
library(stringr)
library(openxlsx)
library(glue)
library(shinyXYpad)

nvl <- function(x, y) {
  nvl_to_vectorize <- function(x, y) {
  if (isTRUE(is.na(x)) | isTRUE(is.null(x))) {
    y 
  } else { x }
  }
  
  Vectorize(nvl_to_vectorize)(x,y)
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
                                           rows = 3:(3+length(chars))))
  
  cols_to_num <- setdiff(colnames(char_stats), 'Character')
  char_stats[ , (cols_to_num) := lapply(.SD, function(x) {
    as.numeric(nvl(x, -1))
  }), .SDcols = cols_to_num] 
  
  return(char_stats)
}

get_move_data <- function(char_move) {
  move_data <- get(unlist(strsplit(char_move, '_'))[1])[Moves %like% paste0('.*', unlist(strsplit(char_move, '_'))[2], '.*')]
  
  # For the moment, take the move with the greatest BKB and (in case of ties) KBS
  move_data[, .SD[which.max(nvl(Knockback.Scaling, -1))], by = Base.Knockback][, .SD[which.max(Base.Knockback)]][1]
}
