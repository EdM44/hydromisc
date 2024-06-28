#' Solve for the groundwater parameters transmissivity and storativity based on
#' the Theis solution. Includes the simplifcation of Cooper Jacob.
#'
#' All code was adapted from the rhytool code produced by
#' Philippe Renard, François Bertone (https://github.com/FrancoisBertone/rhytool).
#'
#' The original code uses a MIT license, which persists in this portion of the
#' code: Permission is hereby granted, free of charge, to any person obtaining a
#' copy of this software and associated documentation files (the "Software"), to
#' deal in the Software without restriction, including without limitation the
#' rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
#' sell copies of the Software, and to permit persons to whom the Software is
#' furnished to do so, subject to the following conditions:
#' The above copyright notice and this permission notice shall be included
#' in all copies or substantial portions of the Software.
#'
#' @param Q Pumping rate [L^3/T]
#' @param r radial distance from pumping well to observation point [L]
#' @param t a vector of times [T]
#' @param s a numeric vector of drawdowns at each time t recorded at distance r [L]+
#'
#' @return a list of aquifer parameters: transmissivity, Tr [L^2/T] and storativity, St []
#'
#' @references Renard, Philippe (2017). Hytool: an open source matlab toolbox for the interpretation of hydraulic tests using analytical solutions. Journal of Open Source Software, 2(19), 441, \href{http://joss.theoj.org/papers/10.21105/joss.00441}{doi:10.21105/joss.00441}
#'
#' @importFrom expint expint_E1
#' @importFrom optimx optimx
#'
#' @name theis
NULL
#'
#' @export
#' @rdname theis
cooper.jacob <- function(Q, r, t, s) {
  if(length(t) != length(s)){
    stop("time and drawdown data must have the same length")
  }
  if(length(t) <  3){
    stop("Insufficient length of observed drawdown data")
  }

  fn <- paste0(".","ths_gss")
  p <- do.call(fn,list(t, s))
  p <- .ths_computeparams(p,c(Q, r))
  return(list(Tr=p[1], St=p[2]))
}

aspar_estimate <- function(Q, r, t, s) {
  if(length(t) != length(s)){
    stop("time and drawdown data must have the same length")
  }
  if(length(t) <  3){
    stop("Insufficient length of observed drawdown data")
  }

  fn <- paste0(".","ths_gss")
  p <- do.call(fn,list(t, s))
  return(c(p[1], p[2]))
}

#' Fit parameters of the analytical Theis solution

#' @export
#' @rdname theis
theis_params <- function(Q, r, t, s) {
  pars <- aspar_estimate(Q, r, t, s)
  fn <- paste0(".","ths_dim")
  opt.fn <- function(b,mydata){
    sum((mydata$s-do.call(fn,list(b,mydata$t)))^2)
  }
  suppressWarnings(cap.optx <- optimx::optimx(par=pars, fn=opt.fn, mydata=list(t=t, s=s), method=c("Nelder-Mead","nlminb", "L-BFGS-B"),
                                              control=list(save.failures=TRUE, maxit=2000)))
  res <- summary(cap.optx, order = "value")
  #print(utils::head(res))
  n <- nrow(res)
  for(i in 1:n){
    new.aspar <- as.vector(as.matrix(res[i, 1:length(pars)]))
    if(length(which(new.aspar<=0))==0){
      p <- .ths_computeparams(c(new.aspar[1], new.aspar[2]),c(Q, r))
      return(list(Tr=p[1], St=p[2]))
    }
  }
  stop("Impossible fitting with non-negative parameters. Please change initial parameters.")
}

##################################################################################################

.ths_details <- function(useless=NULL) {
  a <- "Theis (1935) model"
  b <- c("slope a","intercept t0 (T)")
  c <- c("Transmissivity T (L^2/T)","Storativity S (-)")
  d <- c("Pumping rate Q (L^3/T)", "Distance r between the observation and the pumping well (L)")
  return(list(a,b,c,d))
}

.ths_computeparams <- function(adp,data) {
  if(length(adp)<2) {
    stop("Incorrect length for the adp vector. Length of 2 expected.")
  }
  if(length(adp)>2) {
    warning("Incorrect length for the adp vector. Length of 2 expected. Extra parameters ignored.")
  }
  if(length(data)<2) {
    stop("Incorrect length for the data vector. Length of 2 expected.")
  }
  if(length(data)>2) {
    warning("Incorrect length for the data vector. Length of 2 expected. Extra parameters ignored.")
  }
  Tr <- 0.1832339*data[1]/adp[1]
  S <- 2.2458394*Tr*adp[2]/data[2]^2
  if(length(data)>=3) {
    Ri <- 2*sqrt(Tr*data[3]/S)
    return(c(Tr,S,Ri))
  }
  return(c(Tr,S))
}

.ths_dim <- function(p,t) {
  td <- 0.5625*p[2]/t
  suppressWarnings(s <- p[1]/log(10)*expint::expint_E1(td))
  d <- p[1]/log(10)*exp(-td)
  return(s)
}

.ths_gss <- function(t, s) {
  end <- length(t)
  n <- round(end/3)
  t<-t[n:end]
  s<-s[n:end]
  p <- .jcb_gss(t,s)
  return(p)
}

.jcb_gss <- function(t,s) {
  g <- cbind(log10(t),rep(1,length(t)))
  p <- (solve(t(g)%*%g)%*%t(g))%*%s
  a <- p[1]
  c <- p[2]
  t0 <- 10^(-c/a)
  p[2] <- t0
  return(as.vector(p))
}
