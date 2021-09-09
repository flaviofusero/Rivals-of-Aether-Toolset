add_quadratic <- function(p, x0, y0, v0, theta, t_max, g, air_friction, drift, name) {
  
  #' Overlays quadratic curve to a ggplotly object
  #'
  #' @param p Initial point of application 
  #' @param x0 x0 of the quadratic curve
  #' @param y0 y0 of the quadratic curve
  #' @param v0 Initial velocity
  #' @param theta Initial angle of the quadratic curve, in radians, with the x axis 
  #' @param t_max Final time of the object travel
  #' @param g (Positive) value of gravity acceleration
  #' @param drift Either -1 (drift in), 0 (no drift) or 1 (drift out)
  #' @param name Name for the trace
  #' @return A list with a ggplotly object and 4 numerics
  
  t = 0:round(t_max)
  drift_text <- ifelse(drift == -1, 'in', ifelse(drift == 0, 'none', 'out'))

  v_x <- rep(0, length(t))
  v_x[1] <- v0 * cos(theta)
  
  for (i in 2:length(t)) {
    
    drift_accel <- drift * drift_val * 0.1 * ifelse(
      abs(v_x[i-1]) <=  scaling_factor * 5, scaling_factor, ifelse(
        abs(v_x[i-1]) > scaling_factor * 5 & abs(v_x[i-1]) < scaling_factor * 10, scaling_factor * (1.5 - 0.1 * abs(v_x[i-1])), 
        scaling_factor * 0.5
      )
    )
    
    air_friction_accel <- (abs(v_x[i-1]) >= air_friction) * (-sign(v_x[i-1])) * air_friction
    
    v_x[i] <- v_x[i-1] + drift_accel + air_friction_accel 
    
    # print(i)
    # print(v_x[i])
    # print(v_x[i-1])
    # print(air_friction_accel)
    # print(drift_accel)
  }
  
  x <-  x0 + shift(cumsum(v_x), fill = 0)
  x_valid_idx <- which(x > 10 & x < canvas_w - 10)
  y <- y0 + v0 * sin(theta) * t - (1/2) * g * t * t
  y_valid_idx <- which(y > 10 & y < canvas_h - 10)
  x <- x[intersect(x_valid_idx, y_valid_idx)]
  y <- y[intersect(x_valid_idx, y_valid_idx)]
  
  p <- p %>% add_trace(x = ~x,
                       y = ~y,
                       type = 'scatter',
                       mode = 'markers',
                       name = name,
                       # customdata = t,
                       hovertemplate = paste0('(%{x:.0f}, %{y:.0f})'
                                              # '<br><b>Frame</b>: %{customdata[0]}',
                                              # '<br><b>Drift</b>: %{customdata[2]}'
                                              )
  ) 
  
  return(list(plot = p,
              xmin = min(x),
              xmax = max(x),
              ymin = min(y),
              ymax = max(y)
  ))
}

#  plotly_empty(width = canvas_w, height = canvas_h) %>% add_quadratic(canvas_w / 2, canvas_h / 2, 10, pi / 4, 30, 0.5)