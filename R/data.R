#' SCC County
#'
#' A county boundary for Santa Clara County
#'
#' @format An sf object with 1 rows and many columns
"scc_county"

#' USGS Double Mass curve data
#'
#' Precipitation data from Table 1 from Double-Mass Curves,
#' USGS Geological Survey Water Supply Paper 1541-B, Washington, DC, 1960
#'
#' @format A data frame with year and two columns of precipitation
"precip_double_mass"

#' Snow Water Equivalent from SNOTEL
#'
#' SNOTEL data downloaded using this command
#' snotelr::snotel_download(site_id = 1050, internal = TRUE)
#'
#' @format A data frame with daily values of snow water equivalent
"snow_data"

#' USGS Peak flow data
#'
#' USGS peak flow data downloaded using this command
#' dataRetrieval::readNWISpeak(siteNumbers="11169000")
#'
#' @format A data frame with annual peak flow values for station 11169000
"Qpeak_download"

#' Steps to produce four_trees_swe data frame
#' Snow Water Equivalent data accessed through
#' https://www.nrcs.usda.gov/wps/portal/wcc/
#'
#' Navigated to the snow data for
#' Four Trees (FOR) California  COOPERATOR SNOW SENSORS Site - 5150 ft#
#' Manually extracted April 1 data and renamed columns.
#' four_trees_swe <- readr::read_csv("./data-raw/four_trees_swe.csv")
#' usethis::use_data(four_trees_swe)
#'
#' @format A data frame with April 1 snow water equivalent values for 1980-2023
"four_trees_swe"

#' inflows_20years produced using the following
#' synth_mean <- c(NA)*12
#' inflows <- c(95,112,170,250,265,62,35,18,55,88,85,90)
#' sdi <- exp(.7*log(inflows))
#' inflows_synth <- rnorm(240,inflows,sdi)
#' inflows_synth <- ts(inflows_synth,start = c(2000, 1), frequency = 12)
#' synth_mean <- aggregate(c(inflows_synth), list(month = cycle(inflows_synth)), mean)
#' scale_ratios <- inflows/synth_mean$x
#' scaled_synth_flows <- inflows_synth*scale_ratios
#' inflows_20years <- ts(scaled_synth_flows,start = c(2000, 1), frequency = 12)
#'
#' inflows are from Wurbs and James, Water Resources Engineering, Example 11.7
#'
#' @format A time series of 20 years of synthetic monthly streamflow in 10^6 m^3
"inflows_20years"

#' Snow index model output from monte carlo simulation
#' snow_data used as observed swe and input precip and
#' temp to snow model
#'
#' See https://edm44.github.io/hydr-watres-book/fate-of-precipitation.html
#' save(out, file = "snow_mc_out.rda")
#'
#' @format A vector of RMSE for each of 10,000 simulation runs
"snow_mc_out"
