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
                    str_replace_all('back', 'b')
                  ][
                    , 'Hitstun.Modifier' := gsub('x', '', Hitstun.Modifier, fixed = TRUE)]
  
  cols_to_num <- c('Base.Knockback', 'Knockback.Scaling', 'Angle', 'Damage', 'Hitstun.Modifier')
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
  } else { move[,Angle] }
}

################################### 

chars <-  c('Absa', 'Clairen', 'Etalus', 'Elliana', 'Forsburn', 'Kragg', 'Maypul', 'Orcane', 'Ori', 'Ranno',
          'Shovel Knight', 'Sylvanos', 'Wrastor', 'Zetterburn')

chars_victim <- c(chars, 'Etalus (armor)') %>% sort

scaling_factor <- 0.5
js_click_x_offset <- 275 # offsert given by js_clickanywhere.js. Not sure why this happens. Must be corrected by hand
js_click_y_offset <- -185 #-130 # offsert given by js_clickanywhere.js. Not sure why this happens. Must be corrected by hand

for (c in chars) {
  assign(c, suppressWarnings(parse_char_moves_data(c)))
}

char_stats <- parse_char_stats()

drift_val = 1.25

canvas_w = 1900 * scaling_factor
canvas_h = 1250 * scaling_factor

agates_plats = list(c(112, 36, 156), c(112, 452, 156), c(112, 164, 156), c(112, 324, 156))
armada_plats = list(c(176, -4, 184), c(176, 508, 184))
capital_plats = list(c(192, 64, 128), c(96, 194, 124), c(96, 514, 124), c(192, 640, 128))
forest_plats = list(c(96, 92, 120), c(96, 492, 120))
forestD_plats = list(c(96, 0, 120), c(192, 192, 120), c(96, 384, 128), c(192, 576, 120), c(96, 768, 120))
frozen_plats = list(c(192, 96, 162), c(96, 308, 154), c(192, 512, 162))
hideout_plats = list(c(128, 128, 384))
jules_plats = list(c(96, -96, 768), c(192, -96, 768))
merchant_plats = list(c(96, 16, 112), c(176, 214, 80), c(176, 378, 80), c(96, 544, 112))
rock_plats = list(c(96, 128, 128), c(192, 128, 128), c(96, 512, 128), c(192, 512, 128))
spirit_plats = list(c(82, -96, 192), c(82, 480, 192))
tempest_plats = list(c(32, -174, 124), c(96, 66, 124), c(96, 322, 124), c(32, 562, 124))
treetop_plats = list(c(96, 4, 192), c(162, 320, 188))
tower_plats = list(c(96, 64, 128), c(192, 256, 128), c(96, 448, 128))
troupple_plats = list(c(96, -96, 186), c(192, 164, 186), c(96, 418, 186))
treetopD_plats = list(c(96, 100, 192), c(162, 416, 188))

stages <- list('The Endless Abyss' = list(top = 570, # floor to top blastzone
                                          side = 464, # ledge to side blastzone
                                          bottom = 432, # floor to bottom blastzone
                                          ground = 336, # half of ground width 
                                          plats = NA, # array of platforms
                                          camera = 182), # starting camera height
               'Aethereal Gates' = list(top = 612, side = 500, bottom = 376, ground = 640 / 2, plats = agates_plats, camera = 182), 
               'Air Armada' = list(top = 564, side = 396, bottom = 416, ground = 688 / 2, plats = armada_plats, camera = 198, pineapple = 81), 
               'Fire Capital' = list(top = 612, side = 484, bottom = 432, ground = 832 / 2, plats = capital_plats, camera = 154), 
               'Treetop Lodge (D)' = list(top = 612, side = 388, bottom = 368, ground = 704 / 2, plats = treetopD_plats, camera = 150),
               'The Rock Wall' = list(top = 580, side = 356, bottom = 400, ground = 768 / 2, plats = rock_plats, camera = 182), 
               'Merchant Port' = list(top = 596, side = 452, bottom = 384, ground = 672 / 2, plats = merchant_plats, camera = 182), 
               'Treetop Lodge' = list(top = 612, side = 484, bottom = 368, ground = 512 / 2, plats = treetop_plats, camera = 150), 
               'Blazing Hideout' = list(top = 596, side = 500, bottom = 384, ground = 640 / 2, plats = hideout_plats, camera = 166), 
               'Tempest Peak' = list(top = 628, side = 536, bottom = 400, ground = 512 / 2, plats = tempest_plats, camera = 182), 
               'Frozen Fortress' = list(top = 600, side = 442, bottom = 396, ground = 768 / 2, plats = frozen_plats, camera = 150), 
               'Tower Of Heaven' = list(top = 596, side = 420, bottom = 384, ground = 640 / 2, plats = tower_plats, camera = 166), 
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

# Corresponding to default x, y for zet fair at default inputs
# Used for the initial plot

x_default = canvas_w / 2 + c(0.000000,6.959514,13.919028,20.878541,27.838055,34.797569,41.757083,48.716596,55.676110,62.635624,69.595138,76.554651
      ,83.514165,90.473679,97.433193,104.392706,111.352220,118.311734,125.271248,132.230762,139.190275,146.149789,153.109303,160.068817
      ,167.028330,173.987844,180.947358,187.906872,194.866385,201.825899,208.785413,215.744927,222.704441,229.663954,236.623468,243.582982
      ,250.542496,257.502009,264.461523,271.421037,278.380551,285.340064,292.299578,299.259092,306.218606,313.178119,320.137633,327.097147
      ,334.056661,341.016175,347.975688,354.935202,361.894716,368.854230,375.813743,382.773257,389.732771,396.692285)

y_default = canvas_h / 2 - 50 +c(0.000000,5.839725,11.429451,16.769176,21.858902,26.698627,31.288353,35.628078,39.717803,43.557529,47.147254,50.486980
      ,53.576705,56.416431,59.006156,61.345882,63.435607,65.275332,66.865058,68.204783,69.294509,70.134234,70.723960,71.063685
      ,71.153410,70.993136,70.582861,69.922587,69.012312,67.852038,66.441763,64.781488,62.871214,60.710939,58.300665,55.640390
      ,52.730116,49.569841,46.159566,42.499292,38.589017,34.428743,30.018468,25.358194,20.447919,15.287645,9.877370,4.217095
      ,-1.693179,-7.853454,-14.263728,-20.924003,-27.834277,-34.994552,-42.404827,-50.065101,-57.975376,-66.135650)
