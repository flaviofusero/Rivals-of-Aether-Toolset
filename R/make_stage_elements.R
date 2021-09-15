make_stage_elements <- function(stage) {
  
  stage_outline <- list(type = "rect",
                        fillcolor = '' , 
                        line = list(color = "black"),
                        x0 = center_w - stages[[stage]][['ground']], 
                        y0 = center_h - nvl(x = stages[[stage]][['pineapple']], y = stages[[stage]][['bottom']]),
                        x1 = center_w + stages[[stage]][['ground']],
                        y1 = center_h)
  
  blastzone_outline <- list(type = "rect",
                            fillcolor = '' , 
                            line = list(color = "black"),
                            x0 = center_w - stages[[stage]][['ground']] - stages[[stage]][['side']], 
                            y0 = center_h - stages[[stage]][['bottom']],
                            x1 = center_w + stages[[stage]][['ground']] + stages[[stage]][['side']],
                            y1 = center_h + stages[[stage]][['top']])
  
  plats_outline <- vector(mode = 'list', length = length(stages[[stage]][['plats']]))
  
  if (stage != 'The Endless Abyss') {
    for (i in 1:length(plats_outline)) {
      plats_outline[[i]] <- list(
        type = "rect",
        fillcolor = '' , 
        line = list(color = "black"),
        x0 = center_w - stages[[stage]][['ground']] + stages[[stage]][['plats']][[i]][2],
        y0 = center_h + stages[[stage]][['plats']][[i]][1],
        x1 = center_w - stages[[stage]][['ground']] + stages[[stage]][['plats']][[i]][2] + stages[[stage]][['plats']][[i]][3],
        y1 = center_h + stages[[stage]][['plats']][[i]][1]
      )
    }
  }
  
  stage_elements = append(blastzone_outline %>% list,
                          stage_outline %>% list) %>% 
    append(plats_outline)
  
  return(stage_elements)
}
