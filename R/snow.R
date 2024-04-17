## hydromad: Hydrological Modelling and Analysis of Data

#' Simple degree day factor snow model
#'
#' Simple degree day factor snow model, modified by E. Maurer to account for
#' snowpack cold content, and to recognize data format from snotelr package.
#' Also added ability to enter two values for early and late season melt indices.
#'
#' SWE snow water equivalent
#' ISWE water equivalent of ice in the snowpack
#' LSWE liquid water retained in the snowpack
#'
#' @name snow
#' @aliases snow.sim
#' @param DATA a \code{\link{ts}}-like object with named columns: \describe{
#' \item{list("P")}{ time series of areal rainfall depths, usually in mm. May be "precipitation". }
#' \item{list("T")}{ time series of temperature. May be "temperature_mean". } }
#' @param Tmax temperature threshold for rain, all precipitation is liquid above this
#' threshold.
#' @param Tmin temperature threshold for rain, all precipitation is snow below this
#' threshold.
#' @param Tmelt temperature threshold for snowmelt and freezing in the
#' snowpack.
#' @param deltaT fixed temperature to add to all T values (default = 0)
#' @param kd degree day factor for snowmelt. Can be a numeric value or vector of length 2.
#' @param kdmonth if kd is a vector of 2 values, the month at which kd[2] applies. Ignored otherwise.
#' @param kf degree day factor for freezing (default = 1).
#' @param rcap retention parameter for liquid water capacity of snowpack (default = 0.025).
#' @param cr correction factor for rainfall (default = 1).
#' @param cs correction factor for snowfall. (default = 1)
#' @param LSWE_0,ISWE_0 initial values of state variables (defaults = 0).
#'
#' @return Returns a data frame containing:
#' \itemize{
#'   \item date
#'   \item swe_simulated - the simulated snow water equivalent
#'   \item snowmeltdischarge - the water discharged from the snow pack
#'   \item rain - the precipitation falling as rain
#'   \item snow - the precipitation falling as snow
#' }
#'
#' @author Coded in R by Jarkko Koskela @@tkk.fi 2010-02-26.
#'
#' Converted to C by Felix Andrews \email{felix@@nfrac.org}.
#'
#' @references Kokkonen T., Jakeman A.J, Koivusalo.H, Norton.J.: COMPUTATIONAL
#' METHODS FOR WATER RESOURCE ASSESSMENTS: AN EXERCISE KIT Educational Series
#' on Modelling and Software iEMSs International Modelling and Software Society
#' Available through www.iemss.org
#'
#' @importFrom stats as.ts
#'
#' @export
snow.sim <-
  function(DATA, Tmax, Tmin, kd, kdmonth=NULL, kf=1, rcap=0.025, Tmelt = 0, deltaT = 0,
           cr = 1, cs = 1, LSWE_0 = 0, ISWE_0 = 0 ) {
    #catch for data frame from SNOTELR
    if("precipitation" %in% colnames(DATA)) {
        names(DATA)[names(DATA)=="precipitation"] <- "P"
        names(DATA)[names(DATA)=="temperature_mean"] <- "T"
    }
    stopifnot(c("P", "T") %in% colnames(DATA))
    ## check values
    if(length(kd) > 1) {
      if (is.null(kdmonth) | (as.integer(kdmonth) != kdmonth)) {stop("With a vector of kd values, integer kdmonth is required.")}
      if ( length(kd) != 2 ) {
        stop("kd must be a single numeric value or a vector of length 2.")
      }
      if ( kdmonth < 1 | kdmonth > 12 ) {
        stop("kdmonth must be a month between 1 and 12.")
      }
      #assemble kd value indices for each month, wrap across month 12
      idx <- c(rep(1,kdmonth-1),rep(2,6),rep(1,6),rep(2,6),rep(1,6))[13:24]
    } else {
      idx <- rep(1,12)
    }
    stopifnot(all(kd >= 0))
    #stopifnot(0 <= kd)
    stopifnot(0 <= kf)
    stopifnot(0 <= cr)
    stopifnot(0 <= cs)
    stopifnot(0 <= rcap)
    Tmin <- min(Tmax, Tmin)
	  Tmax <- max(Tmax, Tmin)

    sdate <- DATA$date
    DATA$date <- as.Date(DATA$date)
	  inAttr <- attributes(DATA[, 1])
    DATA <- as.ts(DATA)
    P <- DATA[, "P"]
    T <- DATA[, "T"] + deltaT

    ## rainfall or snowfall
    fr <- (T - Tmin) / (Tmax - Tmin)
    fr <- pmax(pmin(fr, 1, na.rm=TRUE), 0)
    Prain <- fr * cr * P
    Psnow <- (1 - fr) * cs * P

	  ## implementation in R for cross-checking (slow)
	  ## time loop
	  SWE <- Sdischarge <- P * 0
	  LSWEprev <- LSWE_0
	  ISWEprev <- ISWE_0
	  PackAge <- PackTsum <- PackT <- ISWE <- 0
	  for (t in seq(1, length(P))) {
  		# EdM: need month for implementation of monthly varying melt factors
	    mon <- as.integer(format(as.Date(sdate[t]),"%m"))
	    ## Melt (degree day model)
	    melt_potential <- min(max(kd[idx[mon]]*(T[t]-Tmelt),0),ISWEprev)
      # EdM: add cold content formulation of Schaefli and Huss, 2011
  		if(ISWE > 0) {
          PackAge <- PackAge + 1
          PackTsum <- PackTsum + T[t]
          PackT <- min(PackTsum/PackAge,0, na.rm=TRUE)
          CC <- 0.0062 * ISWE * ( Tmelt - PackT )
        } else {
          PackAge <- 0
          PackTsum <- 0
          PackT <- 0
          CC <- 0
        }
      melt <- max(0,melt_potential-CC)
      #melt <- min(max(kd * (T[t] - Tmelt), 0), ISWEprev)
  		## Freezing (degree day model)
  		freeze <- min(max(kf * (Tmelt - T[t]), 0), LSWEprev)
  		## Mass balance for the snowpack
  		##
  		## Ice in the snowpack
  		ISWE <- max(ISWEprev + Psnow[t] + freeze - melt, 0)
  		## Water in the snowpack
  		LSWE <- max(min(rcap * ISWE, LSWEprev + Prain[t] + melt - freeze), 0)
  		## Rain/melt is snowmelt discharge when there is snow on the ground,
  		## and rainfall in snow-free periods.
  		Sdischarge[t] <- max(Prain[t] + melt - freeze - (rcap * ISWE - LSWEprev), 0)
  		SWE[t] <- LSWE + ISWE
  		ISWEprev <- ISWE
  		LSWEprev <- LSWE
  	  }

	  #DATA[, "P"] <- Sdischarge

	  # return a data frame (easier on the formatting)
    return(data.frame(date=sdate,
                      swe_simulated=as.numeric(SWE),
                      snowmeltdischarge=as.numeric(Sdischarge),
                      rain = Prain,
                      snow = Psnow
                      )
           )
}

snow.ranges <- function() {
  list(
    Tmax = c(0, 2),
    Tmin = c(-1, 1),
    cr = c(0.8, 2),
    cs = c(0.8, 2),
    kd = c(2, 5),
    kf = c(0, 2),
    rcap = c(0, 1),
    f = c(0.01, 3),
    e = c(0.01, 1.5),
    d = 200
  )
}
