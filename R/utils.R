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

nvl <- function(x, y) {
  if (isTRUE(is.na(x)) | isTRUE(is.null(x)) | isTRUE(length(x) == 0)) {
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
                    str_replace_all('back', 'b') %>%
                    str_replace_all('neutral', 'n')
                  ][
                    , 'Hitstun.Modifier' := gsub('x', '', Hitstun.Modifier, fixed = TRUE)]
  
  cols_to_num <- c('Base.Knockback', 'Knockback.Scaling', 'Angle', 'Damage', 'Hitstun.Modifier')
  char_moves_data[ , (cols_to_num) := lapply(.SD, function(x) {
    as.numeric(nvl(x, -1))
  }), .SDcols = cols_to_num] 
  char_moves_data[, Knockback.Scaling := Knockback.Scaling / 100]
  
  return(char_moves_data)
}

parse_char_stats <- function(chars_victim = chars_victim) {
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

snap_to <- function(elements, x, y, snap_tol) {
  
  is_in_neighborood_x <- lapply(elements, function(e) {
    between(x, e$x0, e$x1)
  })
  
  is_in_neighborood_y <- lapply(elements, function(e) {
    between(y, e$y1 - snap_tol, e$y1 + snap_tol)
  })
  
  snap_to = intersect(which(is_in_neighborood_x == TRUE), which(is_in_neighborood_y == TRUE))[1]
  
  return(snap_to)
}

parse_angle <- function(move, is_grounded) {
  if (isTRUE(move[,Angle] == 361)) { 
    if (is_grounded == TRUE) 40 else 45
  } else { nvl(move[,Angle], 0) }
}
