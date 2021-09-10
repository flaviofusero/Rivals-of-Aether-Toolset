###################################

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

  # # For the moment, take the move with the greatest BKB and (in case of ties) KBS
  # move_data[, .SD[which.max(nvl(Knockback.Scaling, -1))], by = Base.Knockback][, .SD[which.max(Base.Knockback)]][1]
}

###########################################




chars <-  c('Absa', 'Clairen', 'Etalus', 'Elliana', 'Forsburn', 'Kragg', 'Maypul', 'Orcane', 'Ori', 'Ranno',
          'Shovel Knight', 'Sylvanos', 'Wrastor', 'Zetterburn')

chars_victim <- c(chars, 'Etalus (armor)') %>% sort

scaling_factor = 0.5

for (c in chars) {
  assign(c, suppressWarnings(parse_char_moves_data(c)))
}

char_stats <- parse_char_stats()

drift_val = 1.25

canvas_w = 2000 * scaling_factor
canvas_h = 1400 * scaling_factor

capital_plats = list(c(192, 64, 128), c(96, 194, 124), c(96, 514, 124), c(192, 640, 128))
armada_plats = list(c(176, -4, 184), c(176, 508, 184))
rock_plats = list(c(96, 128, 128), c(192, 128, 128), c(96, 512, 128), c(192, 512, 128))
merchant_plats = list(c(96, 16, 112), c(176, 214, 80), c(176, 378, 80), c(96, 544, 112))
treetop_plats = list(c(96, 4, 192), c(162, 320, 188))
hideout_plats = list(c(128, 128, 384))
tempest_plats = list(c(32, -174, 124), c(96, 66, 124), c(96, 322, 124), c(32, 562, 124))
frozen_plats = list(c(192, 96, 162), c(96, 308, 154), c(192, 512, 162))
tower_plats = list(c(96, 64, 128), c(192, 256, 128), c(96, 448, 128))
gates_plats = list(c(112, 36, 156), c(112, 452, 156), c(112, 164, 156), c(112, 324, 156))
spirit_plats = list(c(82, -96, 192), c(82, 480, 192))
forest_plats = list(c(96, 92, 120), c(96, 492, 120))
jules_plats = list(c(96, -96, 768), c(192, -96, 768))
troupple_plats = list(c(96, -96, 186), c(192, 164, 186), c(96, 418, 186))
treetopD_plats = list(c(96, 100, 192), c(162, 416, 188))
forestD_plats = list(c(96, 0, 120), c(192, 192, 120), c(96, 384, 128), c(192, 576, 120), c(96, 768, 120))

stages <- list('The Endless Abyss' = list(top = 570, # floor to top blastzone
                                          side = 464, # ledge to side blastzone
                                          bottom = 432, # floor to bottom blastzone
                                          ground = 336, # half of ground width 
                                          plats = NA, # array of platforms
                                          camera = 182), # starting camera height
               'Fire Capital' = list(top = 612, side = 484, bottom = 432, ground = 832 / 2, plats = capital_plats, camera = 154), 
               'Treetop Lodge (D)' = list(top = 612, side = 388, bottom = 368, ground = 704 / 2, plats = treetopD_plats, camera = 150),
               'Air Armada' = list(top = 564, side = 396, bottom = 416, ground = 688 / 2, plats = armada_plats, camera = 198, pineapple = 81), 
               'The Rock Wall' = list(top = 580, side = 356, bottom = 400, ground = 768 / 2, plats = rock_plats, camera = 182), 
               'Merchant Port' = list(top = 596, side = 452, bottom = 384, ground = 672 / 2, plats = merchant_plats, camera = 182), 
               'Treetop Lodge' = list(top = 612, side = 484, bottom = 368, ground = 512 / 2, plats = treetop_plats, camera = 150), 
               'Blazing Hideout' = list(top = 596, side = 500, bottom = 384, ground = 640 / 2, plats = hideout_plats, camera = 166), 
               'Tempest Peak' = list(top = 628, side = 536, bottom = 400, ground = 512 / 2, plats = tempest_plats, camera = 182), 
               'Frozen Fortress' = list(top = 600, side = 442, bottom = 396, ground = 768 / 2, plats = frozen_plats, camera = 150), 
               'Tower Of Heaven' = list(top = 596, side = 420, bottom = 384, ground = 640 / 2, plats = tower_plats, camera = 166), 
               'Aethereal Gates' = list(top = 612, side = 500, bottom = 376, ground = 640 / 2, plats = gates_plats, camera = 182), 
               'The Spirit Tree' = list(top = 556, side = 484, bottom = 392, ground = 576 / 2, plats = spirit_plats, camera = 187), 
               'The Forest Floor' = list(top = 564, side = 440, bottom = 376, ground = 704 / 2, plats = forest_plats, camera = 138), 
               'Julesvale' = list(top = 590, side = 460, bottom = 400, ground = 576 / 2, plats = jules_plats, camera = 154), 
               'Troupple Pond' = list(top = 600, side = 500, bottom = 416, ground = 512 / 2, plats = troupple_plats, camera = 182))

stages <- lapply(stages, function(x) {
  lapply(x, function(y) {
    if (is.numeric(y)) scaling_factor * y else {
      lapply(y, '*', scaling_factor)
    }
  })
})
