#' @title Functions adapted from geotech package related to grainsize distributions
#'
#' @description These functions are portions of the `geotech` package used to
#' analyze and plot grainsize distributions for soils. Primary functions are
#' `grainSize.coefs()` to calculate distribution characteristics; `Dsize()` to
#' return the size associated with a percent finer; `pctfromSize()` to return
#' the percent finer for a given diameter; `percentComponents` to
#' calculate the percents of gravel, sand, and fines; `percentSSC()` to return
#' percents sand, silt, and clay (excluding gravel); grainSize.plot()` to create
#' a typical grainsize distribution plot. Most are adapted from the `geotech` package
#' code originally written by Kyle Elmy and Jim Kaklamanos, 2015.

#' @param sieve an input vector of sieve numbers. Used if particle diameters are not provided
#' @param size an input numeric vector of particle sizes [mm or in]
#' @param percent the percent finer corresponding to each sieve or size [percent]
#' @param N a single percent finer used in the Dsize function [percent]
#' @param DN a single diameter used in the pctfromSize function [mm or in]
#' @param metric boolean for units of sizes: TRUE = mm, FALSE = in (Default = TRUE)

#' @return From the grainSize.coefs function: a list of the uniformity coefficient (Cu), curvature coefficient (Cc) and the effective size (D10) of the soil sample
#'
#' @return From the grainSize.plot function: a grainsize distribution plot
#'
#' @return From the percentComponents function: The percents of gravel, sand, and fines
#'
#' @return From the Dsize function: The size associated with the given percent finer
#'
#' @return From the pctfromSize function: The percent finer associated with the given diameter
#'
#' @name GrainSize
NULL

########################################################################################
##  1. GRAIN SIZES OF SIEVES
##
##  Description:  Calculate a set of grain sizes corresponding to a set of sieves
##
##  Output:   Vector of grain sizes (in or mm)
##
##  Inputs:   sieve = vector of sieve numbers according to ASTM D422
##            metric = logical variable: TRUE for metric units (mm), FALSE for English units (in)
##
##  Notes: o  For sieves larger than the no. 4 sieve, the user should specify the
##            sieve size in inches (e.g., 3/8, 3/4, 1, 1.5, 2, 3, etc.)
##         o  Either sieve numbers OR grain sizes must be provided.

size.from.sieve <- function(sieve, metric = TRUE){

  sieve.no <- c(3, 2, 1.5, 1, 3/4, 3/8, 4, 8, 10, 16, 20, 30, 40, 50, 60, 100, 140, 200)
  sieve.in <- c(3, 2, 1.5, 1, 3/4, 3/8, 0.187, 0.0929, 0.0787, 0.0465, 0.0335, 0.0236,
                0.0167, 0.0118, 0.00984, 0.00591, 0.00417, 0.00295)
  sieve.mm <- c(75, 50, 37.5, 25, 19, 9.5, 4.75, 2.36, 2.00, 1.18, 0.850, 0.600, 0.425,
                0.300, 0.250, 0.150, 0.106, 0.075)
  size <- vector(length = length(sieve))
    if(metric == TRUE){
      for(i in 1:length(sieve)){
        size[i] <- sieve.mm[match(sieve[i], sieve.no)]
      }
    } else{
      if(metric == FALSE){
        for(i in 1:length(sieve)){
          size[i] <- sieve.in[match(sieve[i], sieve.no)]
        }
      }
    }

  return(size)
}

########################################################################################
##  2. GRAIN-SIZE PLOT

##  Output:   Plot of soil's grain-size distribution
##
##  Inputs:   sieve = vector of sieve numbers according to ASTM D422
##            size = vector of grain sizes (in or mm)
##            percent = vector of percent passing
##            metric = logical variable: TRUE for metric units (mm), FALSE for English units (in)
##
##  Notes: o  For sieves larger than the no. 4 sieve, the user should specify the
##            sieve size in inches (e.g., 3/8, 3/4, 1, 1.5, 2, 3, etc.)
##         o  Either sieve numbers OR grain sizes must be provided.

#' @export
#' @rdname GrainSize
grainSize.plot <- function(sieve = NULL, size = NULL, percent, metric = TRUE){

  ##  Obtain grain sizes from sieves, if sieve numbers are provided
  if(!(is.null(sieve))){
    size <- size.from.sieve(sieve = sieve, metric = metric)
  }

  ##  Axis labels
  if(metric == TRUE){
    xlab <- "Particle size, D (mm)"
  } else{
    if(metric == FALSE){
      xlab <- "Particle size, D (in)"
    }
  }

  ##  Create plot
  par(las = 1, cex = 1.1)
  plot(x = size, y = percent, log = "x", xaxt = "n", yaxs = "i",
       xlab = xlab,
       ylab = "Percent finer by weight",
       ylim = c(0,100),
       xlim = c(100, 0.001),
       type = "l", lwd = 3)
  #lines(x = size, y = percent, lwd = 3)
  ##  Add logarithmic axis and vertical gridlines
  logAxis(x = size, gridY = TRUE)
  ##  Add horizontal gridlines in increments of 10
  abline(h = seq(from = 0, to = 100, by = 10), col = "gray50")

  ##  Replot points and lines (so they are on top of the plot)
  #points(size, percent, pch = 16, lwd = 4)
  lines(size, percent, lwd = 3)

  ##  Add box back to border
  box()
}

##  Example:
##
##  sieve.example <- c(3/8, 4, 10, 20, 40, 140, 200)
##  percent.example <- c(95.72, 90.23, 81.49, 66.36, 50.00, 8.51, 4.82)
##  grainSize.plot(sieve = sieve.example, percent = percent.example, metric = TRUE)

##  BACKGROUND FUNCTION logAxis.plot = plotting function (one axis at a time)
##
##  Arguments:  data = vector of data values for plot;
##              type = "x" for horizontal axis; "y" for vertical axis
##              vertical = TRUE for vertical gridlines across plot;
##                         FALSE otherwise (default = FALSE)

logAxis.plot <- function(data, type, gridlines){

  ##  Range of data
  pow.min <- 10^floor(log10(min(data[data > 0])))
  pow.max <- 10^ceiling(log10(max(data)))

  ##  Tick marks
  major.ticks <- 10^seq(from = log10(pow.min), to = log10(pow.max), by = 1)
  minor.ticks <- c()
  j <- 1
  for(i in 1:(length(major.ticks)-1)){
    minor.ticks[j:(j+9)] <- seq(from = major.ticks[i], to = major.ticks[i+1],
                                by = major.ticks[i])
    j <- j + 10
  }
  labels <- formatC(major.ticks, format = "fg")

  ##  Side for axis
  if(type == "x"){
    side <- 1
  } else{
    if(type == "y"){
      side <- 2
    }
  }

  ##  Draw axes
  axis(side = side, at = major.ticks, labels = labels, tcl = -0.6)
  axis(side = side, at = minor.ticks, labels = FALSE, tcl = -0.3)

  ##  Draw gridlines
  if(gridlines == TRUE){
    axis(side = side, at = major.ticks, labels = FALSE, tck = 1,
         col = "gray70", lwd = 2)
    axis(side = side, at = minor.ticks, labels = FALSE, tck = 1,
         col = "gray50", lwd = 1)
  }
}

########################################################################################
##  BACKGROUND FUNCTION logAxis = wrapper function for performing calculations
##
##  Arguments:  x = data for x-axis, y = data for y-axis
##              gridX and gridY are TRUE if gridlines in the vertical or horizontal
##                  directions are to be drawn; FALSE otherwise
##

logAxis <- function(x = NULL, y = NULL, gridX = FALSE, gridY = FALSE){

  if(!(is.null(x)) && is.null(y)){
    logAxis.plot(data = x, type = "x", gridlines = gridY)

  } else{
    if(is.null(x) == TRUE && is.null(y) == FALSE){
      logAxis.plot(data = y, type = "y", gridlines = gridX)

    } else{
      if(is.null(x) == FALSE && is.null(y) == FALSE){
        logAxis.plot(data = x, type = "x", gridlines = gridY)
        logAxis.plot(data = y, type = "y", gridlines = gridX)
      }
    }
  }
}

########################################################################################
##  3. PERCENT COMPONENTS

##  Output:   A three-element list containing:
##            pg = Percent gravel
##            ps = Percent sand
##            pf = Percent fines
##
##  Inputs:   sieve = vector of sieve numbers according to ASTM D422
##            size = vector of grain sizes (in or mm)
##            percent = vector of percent passing
##            metric = logical variable: TRUE for metric units (mm), FALSE for English units (in)
##
##  Notes: o  For sieves larger than the no. 4 sieve, the user should specify the
##            sieve size in inches (e.g., 3/8, 3/4, 1, 1.5, 2, 3, etc.)
##         o  Either sieve numbers OR grain sizes must be provided.
##         o  This function assumes that the no. 4 and no. 200 sieves have been used.

#' @export
#' @rdname GrainSize
percentComponents <- function(sieve = NULL, size = NULL, percent, metric = TRUE){
  ##  Obtain grain sizes from sieves, if sieve numbers are provided
  if(!(is.null(sieve))){
    size <- size.from.sieve(sieve = sieve, metric = metric)
  }
  ##  Obtain percent components
  if(metric == TRUE){
    pg <- 100 - pctfromSize(4.75, size = size, percent = percent, metric = TRUE)
    pf <- pctfromSize(0.075, size = size, percent = percent, metric = TRUE)
    ps <- 100 - pg - pf
  } else{
    if(metric == FALSE){
      pg <- 100 - pctfromSize(0.187, size = size, percent = percent, metric = FALSE)
      pf <- pctfromSize(0.003, size = size, percent = percent, metric = FALSE)
      ps <- 100 - pg - pf
    }
  }
  return(list(pg = pg, ps = ps, pf = pf))
}

##  Example:
##
##  sieve.example <- c(3/8, 4, 10, 20, 40, 140, 200)
##  percent.example <- c(95.72, 90.23, 81.49, 66.36, 50.00, 8.51, 4.82)
##  percentComponents(sieve = sieve.example, percent = percent.example, metric = TRUE)

######################################################################################
#' @export
#' @rdname GrainSize
percentSSC <- function(sieve = NULL, size = NULL, percent, metric = TRUE) {
  ##  Obtain grain sizes from sieves, if sieve numbers are provided
  if(!(is.null(sieve))){
    size <- size.from.sieve(sieve = sieve, metric = metric)
  }
  ##  Obtain percent components excluding gravel
  if(metric == TRUE){
    pg <- 100 - pctfromSize(4.75, size = size, percent = percent, metric = TRUE)
    pclay <- pctfromSize(0.005, size = size, percent = percent, metric = TRUE)
    psilt <- approx(size,percent,0.075)$y - pclay
    pclay <- pclay/(1 - pg/100)
    psilt <- psilt/(1 - pg/100)
    psand <- 100 - psilt - pclay
  } else{
    if(metric == FALSE){
      pg <- 100 - pctfromSize(0.187, size = size, percent = percent, metric = TRUE)
      pclay <- pctfromSize(0.0002, size = size, percent = percent, metric = TRUE)
      psilt <- pctfromSize(0.003, size = size, percent = percent, metric = TRUE) - pclay
      pclay <- pclay/(1 - pg/100)
      psilt <- psilt/(1 - pg/100)
      psand <- 100 - psilt - pclay
    }
  }
  return(list(psand = signif(psand, 3), psilt = signif(psilt, 3), pclay = signif(pclay, 3)))
}

########################################################################################
##  4. D-SIZE

##  Output:   The grain size corresponding to a certain percent finer (N), given
##            a grain-size distribution
##
##  Inputs:   N = the percent corresponding to the desired D-size
##            sieve = vector of sieve numbers (according to ASTM D422) that make up the
##                    grain-size distribution
##            size = vector of grain sizes (in or mm) of the distribution
##            percent = vector of percent passing of the grain-size distribution
##            metric = logical variable: TRUE for metric units (mm), FALSE for English units (in)
##
##  Notes: o  For sieves larger than the no. 4 sieve, the user should specify the
##            sieve size in inches (e.g., 3/8, 3/4, 1, 1.5, 2, 3, etc.)
##         o  Either sieve numbers OR grain sizes must be provided.  The "metric" variable
##            is only required if sieve numbers are provided.
##         o  The function uses logarithmic interpolation to calculate the D-size from the
##            provided grain-size distribution
##         o  Log-linear extrapolation is used for grain sizes beyond the range of the data,
##            and a warning is provided

#' @export
#' @rdname GrainSize
Dsize <- function(N, sieve = NULL, size = NULL, percent, metric = TRUE){

  ##  Obtain grain sizes from sieves, if sieve numbers are provided
  if(!(is.null(sieve))){
    size <- size.from.sieve(sieve = sieve, metric = metric)
  }

  ##  Calculate D-size
  DN <- 10^approx(x = percent, y = log10(size), xout = N)$y

  ##  Extrapolate if N is outside the range of data
  if(N > max(percent)){
    delPercent <- sort(percent)[length(percent)] - sort(percent)[length(percent) - 1]
    delSize <- log10(sort(size))[length(size)] - log10(sort(size))[length(size) - 1]
    slope <- delSize / delPercent
    DN <- 10^(log10(sort(size))[length(size)] + slope * (N - sort(percent)[length(percent)]))
    warning("Desired percent is beyond the range of the data; extrapolation is used.")
  } else{
    if(N < min(percent)){
      delPercent <- sort(percent)[2] - sort(percent)[1]
      delSize <- log10(sort(size))[2] - log10(sort(size))[1]
      slope <- delSize / delPercent
      DN <- 10 ^ (log10(sort(size))[1] - slope * (sort(percent)[1] - N))
      warning("Desired percent is beyond the range of the data; extrapolation is used.")
    }
  }
  DN <- max(DN, 0)
  ##  Return
  return(DN)
}

##  Example:
##
##  sieve.example <- c(3/8, 4, 10, 20, 40, 140, 200)
##  percent.example <- c(95.72, 90.23, 81.49, 66.36, 50.00, 8.51, 4.82)
##  Dsize(N = 50, sieve = sieve.example, percent = percent.example, metric = TRUE)

########################################################################################
# Returns the percent finer associated with a given size
#' @export
#' @rdname GrainSize
pctfromSize <- function(DN, sieve = NULL, size = NULL, percent, metric = TRUE){

  ##  Obtain grain sizes from sieves, if sieve numbers are provided
  if(!(is.null(sieve))){
    size <- size.from.sieve(sieve = sieve, metric = metric)
  }

  ##  Calculate D-size
  pct <- approx(x = log10(size), y= percent, xout=log10(DN))$y

  ##  if N is outside the range of data
  if(DN > max(size)){
    delPercent <- sort(percent)[length(percent)] - sort(percent)[length(percent) - 1]
    delSize <- log10(sort(size))[length(size)] - log10(sort(size))[length(size) - 1]
    slope <- delPercent / delSize
    pct <- sort(percent)[length(percent)] + slope * (log10(DN) - log10(sort(size))[length(size)])
    warning("Desired percent is beyond the range of the data; extrapolation is used.")
  } else{
    if(DN < min(size)){
      delPercent <- sort(percent)[2] - sort(percent)[1]
      delSize <- log10(sort(size))[2] - log10(sort(size))[1]
      pct <- sort(percent)[1] - slope * (log10(sort(size))[1] - log10(DN))
      warning("Desired percent is beyond the range of the data; Solution extrapolated.")
    }
  }
  pct <- min(100, max(pct, 0))
  ##  Return
  return(pct)
}

########################################################################################
##  5. COEFFICIENTS OF UNIFORMITY AND CURVATURE

##  Output:   A two-element list containing:
##            Cu = Coefficient of uniformity (D60 / D10)
##            Cc = Coefficient of curvature (D30^2 / (D10 * D60))
##
##  Inputs:   percent = vector of percent passing
##            sieve = vector of sieve numbers according to ASTM D422
##            size = vector of grain sizes
##            D10, D30, D60 = D-sizes corresponding to 10, 30, and 60 percent, respectively
##
##  Notes: o  For sieves larger than the no. 4 sieve, the user should specify the
##            sieve size in inches (e.g., 3/8, 3/4, 1, 1.5, 2, 3, etc.)
##         o  The user has three options for input to this function:
##            1.  Sieve numbers (sieve); and percent passing
##            2.  Grain sizes (size); and percent passing
##            3.  D10, D30, and D60; and percent passing

#' @export
#' @rdname GrainSize
grainSize.coefs <- function(percent, sieve = NULL, size = NULL, D10 = NULL, D30 = NULL, D60 = NULL){

  if(is.null(D10) && is.null(D30) && is.null(D60)){

    ##  Obtain grain sizes from sieves, if sieve numbers are provided
    if(!(is.null(sieve))){
      size <- size.from.sieve(sieve = sieve, metric = FALSE)
    }

    ##  Obtain D sizes
    if(!(is.null(size))){
      D10 <- Dsize(N = 10, size = size, percent = percent)
      D30 <- Dsize(N = 30, size = size, percent = percent)
      D60 <- Dsize(N = 60, size = size, percent = percent)
    }
  }

  ##  Calculate Cu and Cc
  Cu <- D60 / D10
  Cc <- D30^2 / (D10 * D60)

  ##  Return
  return(list(Cu = Cu, Cc = Cc, D10 = D10))
}

##  Example 1:
##
##  sieve.example <- c(3/8, 4, 10, 20, 40, 140, 200)
##  percent.example <- c(95.72, 90.23, 81.49, 66.36, 50.00, 8.51, 4.82)
##  grainSize.coefs(sieve = sieve.example, percent = percent.example)
##
##  Example 2:
##  grainSize.coefs(D60 = 0.10, D30 = 0.03, D10 = 0.002)
