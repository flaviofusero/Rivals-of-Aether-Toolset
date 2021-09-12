draw_stage <- function(stage_elements) {
  
  m <- list(
    l = 0,
    r = 0,
    b = 0,
    t = 0,
    pad = 0
  )
  
  p <- plotly_empty(
    type = 'scatter',
    mode = 'markers',
    x = x_default, 
    y = y_default,
    # name = 'Custom DI',
    # customdata = t,
    hovertemplate = paste0('(%{x:.0f}, %{y:.0f}<extra></extra>)'
                           # '<br><b>Frame</b>: %{customdata[0]}',
                           # '<br><b>Drift</b>: %{customdata[2]}'
    )) %>% 
    layout(shapes = stage_elements,
           xaxis = list(title = '',
                        range = list(0, canvas_w),
                        automargin = FALSE
           ),
           yaxis = list(title = '',
                        range = list(0, canvas_h),
                        scaleanchor = "x",
                        scaleratio = 1,
                        automargin = FALSE
           ),
           # autosize = F,
           margin = m,
           showlegend = FALSE
    ) %>% 
    onRender(click_anywhere, data = "clickposition")
  
  
  p$x$layout$margin$l <- p$x$layout$margin$r <- p$x$layout$margin$b <- p$x$layout$margin$t <- p$x$layout$margin$pad <- 0
  
  return(p)
}
