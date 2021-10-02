draw_stage <- function(stage_elements, traj) {
  
  m <- list(
    l = 0,
    r = 0,
    b = 0,
    t = 0,
    pad = 0
  )
  
  p <- plot_ly(
    type = 'scatter',
    mode = 'markers',
    # name = 'Custom DI',
    # customdata = t,
    # hovertemplate = paste0('(%{x:.0f}, %{y:.0f}<extra></extra>)')
  ) %>% 
    add_trace(x = traj[['x0']] + traj[['x_in']], 
              y = traj[['y0']] + traj[['y_in']], 
              name = 'DI in', 
              marker = list(color = '#009E73'),
              customdata = as.list(0:(length(traj[['x_in']])-1)),
              hovertemplate = paste0('(%{x:.0f}, %{y:.0f})<br>Frame: %{customdata}')) %>% 
    add_trace(x = traj[['x0']] + traj[['x_custom']], 
              y = traj[['y0']] + traj[['y_custom']], 
              name = 'Custom DI', 
              marker = list(color = '#56B4E9'),
              customdata = as.list(0:(length(traj[['x_custom']])-1)),
              hovertemplate = paste0('(%{x:.0f}, %{y:.0f})<br>Frame: %{customdata}')) %>% 
    add_trace(x = traj[['x0']] + traj[['x_out']], 
              y = traj[['y0']] + traj[['y_out']], 
              name = 'DI out', 
              marker = list(color = '#E69F00'),
              customdata = as.list(0:(length(traj[['x_out']])-1)),
              hovertemplate = paste0('(%{x:.0f}, %{y:.0f})<br>Frame: %{customdata}')) %>% 
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
           margin = m,
           showlegend = FALSE
           # images = list(
           #   list(
           #     source = "test.png",
           #     x = 0,
           #     y = -1920 / 2,
           #     sizex = 1920,
           #     sizey = 1080,
           #     xref = "x",
           #     yref = "y",
           #     opacity = 1.0,
           #     layer = "below",
           #     sizing = "stretch"
           #   )
           # )
    ) %>% 
    config(displayModeBar = FALSE) %>% 
    onRender(click_anywhere, data = "clickposition")
  
  # p$x$layout$margin$l <- p$x$layout$margin$r <- p$x$layout$margin$b <- p$x$layout$margin$t <- p$x$layout$margin$pad <- 0
  
  return(p)
}
