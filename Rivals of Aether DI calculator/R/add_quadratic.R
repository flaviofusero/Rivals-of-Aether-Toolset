add_quadratic <- function(p, x0, y0, v0, theta, t_max, g, air_friction, drift) {
  
  #' Overlays quadratic curve to a ggplotly object
  #'
  #' @param p Initial point of application 
  #' @param x0 x0 of the quadratic curve
  #' @param y0 y0 of the quadratic curve
  #' @param v0 Initial velocity
  #' @param theta Initial angle of the quadratic curve, in radians, with the x axis 
  #' @param t_max Final time of the object travel
  #' @param g (Positive) value of gravity acceleration
  #' @param drift Acceleration on the x axis given by drift DI
  #' @return A ggplotly object, with the overlayed quadratic curve
  
  cos_th = cos(theta) %>% 
    round(digits = 10)
  
  t = 0:round(t_max)
  
  v_x <- rep(v0 * cos_th, length(t)) -
    sign(cos_th) * air_friction * t +
    drift * air_friction * t
  
  if(cos_th != 0) {
    v_x[sign(v_x) != sign(cos_th)] <- v_x[sign(v_x) != sign(cos_th)] + 
      sign(cos_th) * air_friction * (which(sign(v_x) != sign(cos_th)) - which(sign(v_x) != sign(cos_th))[1])
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
                       mode = 'markers'
  ) 
  
  p <- p %>% 
    layout(showlegend = FALSE
    )
  
  return(list(plot = p,
              xmin = min(x),
              xmax = max(x),
              ymin = min(y),
              ymax = max(y)
  ))
}

#  plotly_empty(width = canvas_w, height = canvas_h) %>% add_quadratic(canvas_w / 2, canvas_h / 2, 10, pi / 4, 30, 0.5)