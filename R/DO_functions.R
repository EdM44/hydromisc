#' @title Functions to calculate a Streeter-Phelps dissolved oxygen sag curve
#'
#' @description These functions are used in a classroom exercise for dissolved oxygen
#' deficit calculations. Functions are `Mixture()` to calculate the resulting temperature
#' or concentration from two streams; `O2sat()` to calculate the saturation concentration
#' of oxygen; `DOdeficit()` to find the dissolved oxygen deficit at a given time; and
#' `Kadj_deox()` and `Kadj_reox()` to adjust deoxygenation and reaeration coefficients for
#' actual temperature prior to calculating dissolved oxygen deficit.
#'
#' @param T the water temperature [\eqn{^{\circ}C}{C} or \eqn{^{\circ}F}{F}]
#' @param Q1 first input flow rate to Mixture function [\eqn{m^3 s^{-1}}{m^3/s}]
#' @param T1 the first conservative constituent to the Mixture function, usually
#' a temperature or concentration
#' @param Q2 the second input flow rate to Mixture function [\eqn{m^3 s^{-1}}{m^3/s}]
#' @param T2 the second conservative constituent to the Mixture function, usually
#' a temperature or concentration
#'
#' @param T the water temperature [\eqn{^{\circ}C}{C}]
#' @param P atmospheric pressure (optional, default = 1) [atm]
#'
#' @param t time [d]
#' @param K1 deoxygenation coefficient (at 20\eqn{^{\circ}C}, base e) [\eqn{d^{-1}}{1/d}]
#' @param K2 reaeration coefficient (at 20\eqn{^{\circ}C}, base e) [\eqn{d^{-1}}{1/d}]
#' @param L0 the ultimate BOD (at t=infinity) [\eqn{mg L^{-1}}{mg/L}]
#' @param D0 the initial DO deficit [\eqn{mg L^{-1}}{mg/L}]
#'
#' @return From the Mixture function: The concentration or temperature of the mixture
#'
#' @return From the Kadj_deox and Kadj_reox functions: The adjusted K value [\eqn{d^{-1}}{1/d}]

#' @return From O2sat function, the saturation dissolved oxygen [\eqn{mg L^{-1}}{mg/L}]
#'
#' @return from DOdeficit function, the dissolved oxygen at the given time(s) [\eqn{mg L^{-1}}{mg/L}]
#'
#' @author Ed Maurer
#'
#' @name DO_functions
NULL
#' @export
#' @rdname DO_functions
Mixture <- function (Q1=NULL, T1=NULL, Q2=NULL, T2=NULL) {
  checks <- c(Q1, T1, Q2, T2)
  if (length(checks) < 4) {
    stop("Tmixture requires 4 input variables.")
  }
  if( (Q1 <= 0) | (Q2 <= 0)) {
    stop("Flows must be positive values")
  }
  T <- (Q1*T1+Q2*T2)/(Q1+Q2)
  return(T)
}

#' @export
#' @rdname DO_functions
Kadj_deox <- function (K1=NULL, T=NULL) {
  checks <- c(K1, T)
  if (length(checks) < 2) {
    stop("Kadj_deox requires 2 input variables.")
  }
  if(T <= 20){
    theta=1.135
  } else {
    theta=1.056
  }
  return(K1*(theta^(T-20)))
}

#' @export
#' @rdname DO_functions
Kadj_reox <- function (K2=NULL, T=NULL) {
  checks <- c(K2, T)
  if (length(checks) < 2) {
    stop("Kadj_reox requires 2 input variables.")
  }
  return(K2*(1.024^(T-20)))
}

#' @export
#' @rdname DO_functions
O2sat <- function (T=NULL, P=1.0) {
  #P in atmospheres, T in Celsius, DO in mg/L
  F1 <- P * exp(7.7117-1.31403*log(T+45.93))
  F2 <- 1-exp(11.8571-(3840.7/(T+273.15)) - (216961/((T+273.15)^2)))/P
  F3 <- 1-(0.000975-(0.00001426*T)+(0.00000006436*(T^2)))*P
  F4 <- 1-exp(11.8571-(3840.7/(T+273.15))-(216961/((T+273.15)^2)))
  F5 <- 1-(0.000975-(0.00001426*T)+(0.00000006436*(T^2)))
  return((F1 * F2 * F3)/ F4 / F5)
}

#' @export
#' @rdname DO_functions
DOdeficit <- function (t=NULL, K1=NULL, K2=NULL, L0=NULL, D0=NULL) {
  DOdef <- (K1*L0)/(K2-K1)*(exp(-1*K1*t)-exp(-K2*t))+D0*exp(-K2*t)
  return(DOdef)
}
