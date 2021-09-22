assist_di <- function(angle, DI, v0) {
  
  #' Calculate the correct angle after factoring in DI assist.
  #' For weak moves which send you at an angle from 20 to 70 with low enough KB (depending on the angle), holding "out" gives you full DI out.
  #' For all the rest, DIing withing 22 degrees of perpendicular gives you full DI in / out.
  #'
  #' @param angle Launch angle in 1st or 4st quadrant, i.e. between 270 and 360 or between 0 and 90
  #' @param DI DI angle, between 0 and 360
  #' @param v0 Knobkback of the move in pixel / second
  
  if (between(angle, 20, 70) &
      v0 < 6 + ((angle - 20) / 50) * 6 & # KB limit for determining "weak" moves. Goes from 6 to 12 depending on the angle.
      abs(short_arc_between(DI, 0)) <= 22) { # TODO: is 22 degress from 0 the correct threshold for full DI out on weak moves?
    return(-1)     
  } else if (abs(short_arc_between(DI, angle + 90)) <= 22) {
    return(+1)
  } else if (abs(short_arc_between(DI, angle - 90)) <= 22) {
    return(-1)
  } else { 
    return(sin((pi / 180) * (DI - angle)))
  }
}