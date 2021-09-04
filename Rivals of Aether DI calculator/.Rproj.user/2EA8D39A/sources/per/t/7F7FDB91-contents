library(shiny)
library(shinydashboard)
library(plotly)

server = function(input, output) { 
  output$image <- renderUI({
    tags$img(src = "zet_fair.png", width = '200px', height = '170px', style = 'text-align:middle;')
  })
  
  add_quadratic <- function(p, x0, y0, v0, theta, t_max, g) {
    t = 1:round(t_max)
    x = x0 + v0*cos(theta)*t
    y = y0 + v0*sin(theta)*t - (1/2)*g*t*t
    
    p <- p %>% add_trace(x = ~x,
                         y = ~y,
                         type = 'scatter',
                         mode = 'markers')
    p
  }
  
  canvas_w = 1200
  canvas_h = 1000
  
  top = 570 # floor to top blastzone
  side = 464 # ledge to side blastzone
  bottom = 432 # floor to bottom blastzone
  ground = 336 # half of ground width 
  plats = NULL # array of platforms
  camera = 182 # starting camera height
  
  p <- plotly_empty(width = canvas_w, height = canvas_h) %>% 
    layout(shapes = list(
      list(type = "rect",
           fillcolor = '' , line = list(color = "black"),
           x0 = canvas_w / 2 - ground - side, 
           x1 = canvas_w / 2 + ground + side,
           y0 = canvas_h / 2 - bottom,
           y1 = canvas_h / 2 + top,
           xref = "x",
           yref = "y"),
      list(type = "rect",
           fillcolor = '' , line = list(color = "black"),
           x0 = canvas_w / 2 - ground, 
           x1 = canvas_w / 2 + ground,
           y0 = canvas_h / 2 - bottom,
           y1 = canvas_h / 2,
           xref = "x",
           yref = "y")))
  
  BKB = 8
  Knockback_Adj = 1
  Damage = 100
  Knockback_scaling = 0.75
  
  KB = BKB + (Knockback_scaling * Knockback_Adj * Damage * 0.12)
  hitstun = BKB * 4 * ((Knockback_Adj - 1) * 0.6 + 1) + Damage * 0.12 * Knockback_scaling * 4 * 0.65 * Knockback_Adj
  v0 = 9
  
  p <- p %>% add_quadratic(canvas_w / 2, canvas_h / 2, 
                      v0 = KB,
                      theta = pi / 4,
                      t_max = hitstun,
                      g = 0.5)
  
  output$plot <- renderPlotly(p)
  
  # list(type = 'line',
  #      line = list(color = 'pink'),
  #      x0 = 0.5, x1 = 0.75, xref = "x",
  #      y0 = 2, y1 = 2, yref = "y")))
  
}