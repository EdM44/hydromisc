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

