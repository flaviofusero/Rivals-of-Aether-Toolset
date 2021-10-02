library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
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
library(shinyBS)

chars <-  c('Absa', 'Clairen', 'Etalus', 'Elliana', 'Forsburn', 'Kragg', 'Maypul', 'Orcane', 'Ori', 'Ranno',
            'Shovel Knight', 'Sylvanos', 'Wrastor', 'Zetterburn')

chars_victim <- c(chars, 'Elliana (snake)', 'Etalus (armor)', 'Shovel Knight (mail)') %>% sort
# TODO: might need to confirm stats for Elliana (snake), Etalus (armor), SK (mail)

angle_flippers <- as.data.table(read.xlsx(
  'input/Rivals of Aether Academy Frame Data - Updated for 2.0.7.0.xlsx', 
  sheet = 'Angle Flippers',
  rows = 1:12,
  cols = 1:13))[,
                Angle.Flipper := as.numeric(Angle.Flipper)]

framedata <- loadWorkbook('input/Rivals of Aether Academy Frame Data - Updated for 2.0.7.0.xlsx')

icons <- sapply(chars, function(x) {glue(
  "<img src='Icons/{x}.png' width=25px></img>"
)}
)

icons_victims <- sapply(chars_victim, function(x) {glue(
  "<img src='Icons/{x}.png' width=25px></img>"
)}
)

icons_chars <- as.data.table(cbind(icons, chars))[, paste0(icons, '&nbsp;&nbsp;', chars)]
icons_chars_victim <- as.data.table(cbind(icons_victims, chars_victim))[, paste0(icons_victims, '&nbsp;&nbsp;', chars_victim)]

# Moves with omnidirectional knockback angle
omni_moves <- c('Zetterburn_nspecial', 'Zetterburn_uspecial', 'Forsburn_dspecial', 'Forsburn_fspecial', 'Ori_nspecial')

scaling_factor <- 1

snap_tol = 30

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

max_bz_bottom = lapply(stages, '[[', 'bottom') %>% unlist %>%  max 
max_bz_top = lapply(stages, '[[', 'top') %>% unlist %>%  max
max_bz_h = max_bz_bottom + max_bz_top
max_bz_w = 2 * (lapply(stages, '[[', 'ground') %>% unlist + lapply(stages, '[[', 'side') %>% unlist) %>% max
buffer_w = 100
buffer_h = 50

canvas_w = max_bz_w + buffer_w
canvas_h = max_bz_h + buffer_h
center_w = 0 # canvas_w / 2
center_h = 0 # max_bz_bottom + buffer_h / 2
xrange = list(-canvas_w / 2, canvas_w / 2)
yrange = list(-(max_bz_bottom + buffer_h / 2), max_bz_top + buffer_h)

# Corresponding to default x, y for zet fair at default inputs
# Used for the initial plot
x_default_custom = center_w + c(0.00000,13.91903,27.79806,41.63708,55.43611,69.19514,82.91417,96.59319,110.23222,123.83125,137.39028,150.90930,164.38833,
                                177.82736,191.22639,204.58541,217.90444,231.18347,244.42250,257.62152,270.78055,283.89958,296.97861,310.01763,323.01666,335.97569,
                                348.89472,361.77374,374.61277,387.41180,400.17083,412.88985,425.56888,438.20791,450.80694,463.36596,475.88499,488.36402,500.80305,
                                513.20207,525.56110,537.88013,550.15916,562.39818,574.59721,586.75624,598.87527,610.95429,622.99332,634.99235,646.95138,658.87040,
                                670.74943,682.58846,694.38749,706.14651,717.86554,729.54457,741.18360)
x_default_in = center_w + c(0.000000,9.628633,19.217266,28.765899,38.274532,47.743165,57.171798,66.560431,75.909064,85.217697,94.486330,103.714963,
                            112.903596,122.052229,131.160862,140.229495,149.258128,158.246762,167.195395,176.104028,184.972661,193.801294,202.589927,211.338560,
                            220.047193,228.715826,237.344459,245.933092,254.481725,262.990358,271.458991,279.887624,288.276257,296.624890,304.933523,313.202156,
                            321.430789,329.619422,337.768055,345.876688,353.945321,361.973954,369.962587,377.911220,385.819853,393.688486,401.517119,409.305752,
                            417.054385,424.763019,432.431652,440.060285,447.648918,455.197551,462.706184,470.174817,477.603450,484.992083,492.340716)
x_default_out = center_w + c(0.00000,16.84693,33.65386,50.42079,67.14772,83.83465,100.48158,117.08851,133.65545,150.18238,166.66931,183.11624,199.52317,
                             215.89010,232.21703,248.50396,264.75089,280.95782,297.12475,313.25168,329.33861,345.38554,361.39247,377.35940,393.28634,409.17327,
                             425.02020,440.82713,456.59406,472.32099,488.00792,503.65485,519.26178,534.82871,550.35564,565.84257,581.28950,596.69643,612.06336,
                             627.39029,642.67723,657.92416,673.13109,688.29802,703.42495,718.51188,733.55881,748.56574,763.53267,778.45960,793.34653,808.19346,
                             823.00039,837.76732,852.49425,867.18119,881.82812,896.43505,911.00198)

y_default_custom = center_h + c(0,11.67945,22.8589,33.53835,43.7178,53.39725,62.57671,71.25616,79.43561,87.11506,94.29451,100.974,107.1534,112.8329,118.0123,122.6918,126.8712,130.5507,133.7301,136.4096,138.589,140.2685,141.4479,142.1274,142.3068,141.9863,141.1657,139.8452,138.0246,135.7041,132.8835,129.563,125.7424,121.4219,116.6013,111.2808,105.4602,99.13968,92.31913,84.99858,77.17803,68.85749,60.03694,50.71639,40.89584,30.57529,19.75474,8.434191,-3.386358,-15.70691,-28.52746,-41.84801,-55.66855,-69.9891,-84.80965,-100.1302,-115.9508,-132.2713,-149.0918)
y_default_in = center_h + c(0,15.40903,30.31807,44.7271,58.63614,72.04517,84.9542,97.36324,109.2723,120.6813,131.5903,141.9994,151.9084,161.3174,170.2265,178.6355,186.5445,193.9536,200.8626,207.2716,213.1807,218.5897,223.4987,227.9078,231.8168,235.2258,238.1349,240.5439,242.4529,243.862,244.771,245.1801,245.0891,244.4981,243.4072,241.8162,239.7252,237.1343,234.0433,230.4523,226.3614,221.7704,216.6794,211.0885,204.9975,198.4065,191.3156,183.7246,175.6336,167.0427,157.9517,148.3607,138.2698,127.6788,116.5878,104.9969,92.9059,80.31493,67.22397)
y_default_out = center_h + c(0,6.806602,13.1132,18.91981,24.22641,29.03301,33.33961,37.14621,40.45281,43.25942,45.56602,47.37262,48.67922,49.48582,49.79243,49.59903,48.90563,47.71223,46.01883,43.82543,41.13204,37.93864,34.24524,30.05184,25.35844,20.16505,14.47165,8.278249,1.58485,-5.608548,-13.30195,-21.49534,-30.18874,-39.38214,-49.07554,-59.26894,-69.96234,-81.15573,-92.84913,-105.0425,-117.7359,-130.9293,-144.6227,-158.8161,-173.5095,-188.7029,-204.3963,-220.5897,-237.2831,-254.4765,-272.1699,-290.3633,-309.0567,-328.2501,-347.9435,-368.1369,-388.8303,-410.0237,-431.7171)
