draw_stage <- function(stage) {
  
  m <- list(
    l = 0,
    r = 0,
    b = 0,
    t = 0,
    pad = 4
  )
  
  p <- plotly_empty(width = canvas_w, height = canvas_h) %>% 
    layout(shapes = list(
      list(type = "rect",
           fillcolor = '' , line = list(color = "black"),
           x0 = canvas_w / 2 - stages[[stage]][['ground']] - stages[[stage]][['side']], 
           y0 = canvas_h / 2 - stages[[stage]][['bottom']],
           x1 = canvas_w / 2 + stages[[stage]][['ground']] + stages[[stage]][['side']],
           y1 = canvas_h / 2 + stages[[stage]][['top']]),
      list(type = "rect",
           fillcolor = '' , line = list(color = "black"),
           x0 = canvas_w / 2 - stages[[stage]][['ground']], 
           y0 = canvas_h / 2 - nvl(x = stages[[stage]][['pineapple']], y = stages[[stage]][['bottom']]),
           x1 = canvas_w / 2 + stages[[stage]][['ground']],
           y1 = canvas_h / 2)
    ),
    xaxis = list(title = '',
                 range = c(0, canvas_w),
                 automargin = FALSE),
    yaxis = list(title = '',
                 range = c(0, canvas_h),
                 automargin = FALSE),
    autosize = F,
    margin = m,
    showlegend = FALSE
    )
  
  for (plat in stages[[stage]][['plats']]) {
    p <- p %>% add_segments(
      line = list(color = "black"),
      x = canvas_w / 2 - stages[[stage]][['ground']] + plat[2],
      y = canvas_h / 2 + plat[1],
      xend = canvas_w / 2 - stages[[stage]][['ground']] + plat[2] + plat[3],
      yend = canvas_h / 2 + plat[1],
      hoverinfo = 'skip'
    )
  } 
  
  return(p)
  
}