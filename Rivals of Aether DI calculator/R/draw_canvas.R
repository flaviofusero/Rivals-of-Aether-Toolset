draw_stage <- function(stage_elements) {
  
  m <- list(
    l = 0,
    r = 0,
    b = 0,
    t = 0,
    pad = 0
  )
  
  # p <- plotly_empty(
  p <- plot_ly(
    type = 'scatter',
    mode = 'markers',
    # x = x_default, 
    # y = y_default,
    # name = 'Custom DI',
    # customdata = t,
    hovertemplate = paste0('(%{x:.0f}, %{y:.0f}<extra></extra>)'
                           # '<br><b>Frame</b>: %{customdata[0]}',
                           # '<br><b>Drift</b>: %{customdata[2]}'
    )) %>% 
    add_trace(x = x_default_in, y = y_default_in, name = 'DI in', text = paste0('<br>Frame: ', 0:(length(x_default_in) - 1)),
              hovertemplate = paste0('(%{x:.0f}, %{y:.0f}) %{text}')) %>% 
    add_trace(x = x_default_custom, y = y_default_custom, name = 'Custom DI', text = paste0('<br>Frame: ', 0:(length(x_default_custom) - 1)),
              hovertemplate = paste0('(%{x:.0f}, %{y:.0f}) %{text}')) %>% 
    add_trace(x = x_default_out, y = y_default_out, name = 'DI out', text = paste0('<br>Frame: ', 0:(length(x_default_out) - 1)),
              hovertemplate = paste0('(%{x:.0f}, %{y:.0f}) %{text}')) %>%
    layout(shapes = stage_elements,
           xaxis = list(title = '',
                        range = xrange,
                        automargin = FALSE,
                        zeroline = FALSE,
                        showgrid = FALSE
           ),
           yaxis = list(title = '',
                        range = yrange,
                        scaleanchor = "x",
                        scaleratio = 1,
                        automargin = FALSE,
                        zeroline = FALSE,
                        showgrid = FALSE
           ),
           # autosize = F,
           # margin = m,
           showlegend = FALSE
    ) %>% 
    config(displayModeBar = FALSE) %>% 
    onRender(click_anywhere, data = "clickposition")
  
  
  # p$x$layout$margin$l <- p$x$layout$margin$r <- p$x$layout$margin$b <- p$x$layout$margin$t <- p$x$layout$margin$pad <- 0
  
  return(p)
}
