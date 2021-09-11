make_stage_elements <- function(stage) {
  
  stage_outline <- list(type = "rect",
                        fillcolor = '' , 
                        line = list(color = "black"),
                        x0 = canvas_w / 2 - stages[[stage]][['ground']], 
                        y0 = canvas_h / 2 - nvl(x = stages[[stage]][['pineapple']], y = stages[[stage]][['bottom']]) - 50,
                        x1 = canvas_w / 2 + stages[[stage]][['ground']],
                        y1 = canvas_h / 2 - 50)
  
  blastzone_outline <- list(type = "rect",
                            fillcolor = '' , 
                            line = list(color = "black"),
                            x0 = canvas_w / 2 - stages[[stage]][['ground']] - stages[[stage]][['side']], 
                            y0 = canvas_h / 2 - stages[[stage]][['bottom']] - 50,
                            x1 = canvas_w / 2 + stages[[stage]][['ground']] + stages[[stage]][['side']],
                            y1 = canvas_h / 2 + stages[[stage]][['top']] - 50)
  
  plats_outline <- vector(mode = 'list', length = length(stages[[stage]][['plats']]))
  
  if (stage != 'The Endless Abyss') {
    for (i in 1:length(plats_outline)) {
      plats_outline[[i]] <- list(
        type = "rect",
        fillcolor = '' , 
        line = list(color = "black"),
        x0 = canvas_w / 2 - stages[[stage]][['ground']] + stages[[stage]][['plats']][[i]][2],
        y0 = canvas_h / 2 + stages[[stage]][['plats']][[i]][1] - 50,
        x1 = canvas_w / 2 - stages[[stage]][['ground']] + stages[[stage]][['plats']][[i]][2] + stages[[stage]][['plats']][[i]][3],
        y1 = canvas_h / 2 + stages[[stage]][['plats']][[i]][1] - 50
      )
    }
  }
  
  stage_elements = append(blastzone_outline %>% list,
                          stage_outline %>% list) %>% 
    append(plats_outline)
  
  return(stage_elements)
}
