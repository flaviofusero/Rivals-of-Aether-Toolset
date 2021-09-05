draw_stage <- function(stage) {
  
  m <- list(
    l = 0,
    r = 0,
    b = 0,
    t = 0,
    pad = 4
  )
  
  plotly_empty(width = canvas_w, height = canvas_h) %>% 
    layout(shapes = list(
      list(type = "rect",
           fillcolor = '' , line = list(color = "black"),
           x0 = canvas_w / 2 - stages[[stage]][['ground']] - stages[[stage]][['side']], 
           x1 = canvas_w / 2 + stages[[stage]][['ground']] + stages[[stage]][['side']],
           y0 = canvas_h / 2 - stages[[stage]][['bottom']],
           y1 = canvas_h / 2 + stages[[stage]][['top']]),
      list(type = "rect",
           fillcolor = '' , line = list(color = "black"),
           x0 = canvas_w / 2 - stages[[stage]][['side']], 
           x1 = canvas_w / 2 + stages[[stage]][['side']],
           y0 = canvas_h / 2 - stages[[stage]][['bottom']],
           y1 = canvas_h / 2)),
      xaxis = list(title = '',
                   range = c(0, canvas_w),
                   automargin = FALSE),
      yaxis = list(title = '',
                   range = c(0, canvas_h),
                   automargin = FALSE),
      autosize = F,
      margin = m
    )
}