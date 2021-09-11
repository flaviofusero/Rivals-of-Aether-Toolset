#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int csign(double x) {
  if (x > 0) {
	return(1); 
  } else if (x < 0) {
	return(-1);
  } else {
	return(0);
  }
}

// [[Rcpp::export]]
NumericVector make_x(int t_max, double scaling_factor, double v0x, double drift, double air_friction) {
  NumericVector v_x(t_max + 1);
  NumericVector x(t_max + 1);
  double drift_factor;
  double drift_accel;
  double air_friction_accel;
  
  v_x[0] = v0x;
  x[0] = 0;
  
  for (int i = 1; i <= t_max; i++) {
  
      if (fabs(v_x[i-1]) <= scaling_factor * 5) {
      drift_factor = scaling_factor;
      } else if ((fabs(v_x[i-1]) > scaling_factor * 5) & (fabs(v_x[i-1]) < scaling_factor * 10)) {
      drift_factor = scaling_factor * (1.5 - 0.1 *  fabs(v_x[i-1]));
      } else {
      drift_factor = scaling_factor * 0.5;
      };
      drift_accel = drift * 1.25 * 0.1 * drift_factor;
      
      if (fabs(v_x[i-1]) >= air_friction) {
          air_friction_accel = (csign(-v_x[i-1])) * air_friction;
      } else {
          air_friction_accel = 0;
      };
      
      v_x[i] = v_x[i-1] + drift_accel + air_friction_accel;
	    x[i] = x[i-1] + v_x[i-1];
  }
  
  return(x);
}

// [[Rcpp::export]]
NumericVector make_y(int t_max, double scaling_factor, double v0y, double g) {
  NumericVector v_y(t_max + 1);
  NumericVector y(t_max + 1);
  
  v_y[0] = v0y;
  y[0] = 0;
  
  for (int i = 1; i <= t_max; i++) {
      v_y[i] = v_y[i-1] - g;
	    y[i] = y[i-1] + v_y[i-1];
  }
  
  return(y);
}