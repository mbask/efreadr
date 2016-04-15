#' @title Read European Fluxes CSV Files
#'
#' @description The European Eddy Fluxes Database Cluster distributes fluxes of different Green House Gases measured mainly using the eddy covariance technique acquired in sites involved in EU projects but also single sites in Europe, Africa and others continents that decided to share their measurements in the database (cit. http://gaia.agraria.unitus.it ). The package provides two functions to load and row-wise bind CSV files distributed by the database. Currently only L3 and L4 (L=Level), half-hourly and daily (aggregation) files are supported.
#'
#' @name efreadr-package
#' @docType package
#' @author Marco Bascietto \email{marco.bascietto@@crea.gov.it}
#' @keywords package
#' @references Source code is hosted at GitHub (\url{https://github.com/mbask/efreadr})
NULL

#' @author Marco Bascietto \email{marco.bascietto@@crea.gov.it}
`: dataframe_with_pathname` <- ensures_that(
  is.data.frame(.),
  "pathname" %in% colnames(.),
  err_desc = "Something wrong with the returned dataframe, are any fluxes files present in the directories?")

#' @author Marco Bascietto \email{marco.bascietto@@crea.gov.it}
`: dataframe_with_level_aggr_and_fluxes` <- ensures_that(
  is.data.frame(.),
  sum(c("level", "aggr", "fluxes") %in% colnames(.)) == 3,
  err_desc = "Something wrong with the returned dataframe")

globalVariables(c(".", "level", "aggr", "col_types_l"))

efreadr_env <- new.env()

local(
  col_types_l <- list(
    "h" = cols(
      Month   = col_integer(),
      Day     = col_integer(),
      Hour    = col_double(),
      DoY     = col_double(),
      Rg_f    = col_double(),
      Rg_fqc  = col_integer(),
      Ta_f    = col_double(),
      Ta_fqc  = col_integer(),
      VPD_f    = col_double(),
      VPD_fqc  = col_integer(),
      Ts_f    = col_double(),
      Ts_fqc  = col_integer(),
      Precip  = col_double(),
      SWC     = col_double(),
      H_f     = col_double(),
      H_fqc   = col_integer(),
      LE_f    = col_double(),
      LE_fqc  = col_integer(),
      qf_NEE_st = col_double(),
      qf_NEE_or = col_double(),
      Reco_st   = col_double(),
      Reco_or   = col_double(),
      NEE_st_fMDS   = col_double(),
      NEE_st_fMDSqc = col_integer(),
      GPP_st_MDS    = col_double(),
      NEE_or_fMDS   = col_double(),
      NEE_or_fMDSqc = col_integer(),
      GPP_or_MDS    = col_double(),
      NEE_st_fANN   = col_double(),
      NEE_st_fANNqc = col_integer(),
      GPP_st_ANN    = col_double(),
      NEE_or_fANN   = col_double(),
      NEE_or_fANNqc = col_integer(),
      GPP_or_ANN    = col_double()),
    "d" = cols(
      Month   = col_integer(),
      Day     = col_integer(),
      DoY     = col_double(),
      Rg_f    = col_double(),
      Rg_sqc  = col_double(),
      Ta_f    = col_double(),
      Ta_sqc  = col_double(),
      VPD_f   = col_double(),
      VPD_sqc = col_double(),
      Ts_f    = col_double(),
      Ts_sqc  = col_double(),
      Precip  = col_double(),
      SWC     = col_double(),
      H_f     = col_double(),
      H_sqc   = col_double(),
      LE_f    = col_double(),
      LE_sqc  = col_double(),
      Reco_st = col_double(),
      Reco_or = col_double(),
      NEE_st_fMDS    = col_double(),
      NEE_st_fMDSsqc = col_double(),
      GPP_st_MDS     = col_double(),
      NEE_or_fMDS    = col_double(),
      NEE_or_fMDSsqc = col_double(),
      GPP_or_MDS     = col_double(),
      NEE_st_fANN    = col_double(),
      NEE_st_fANNsqc = col_double(),
      GPP_st_ANN     = col_double(),
      NEE_or_fANN    = col_double(),
      NEE_or_fANNsqc = col_double(),
      GPP_or_ANN     = col_double()),
    "R" = cols( # Level 2
      TIMESTAMP_START = col_datetime(format = "%Y%m%d%H%M"),
      TIMESTAMP_END   = col_datetime(format = "%Y%m%d%H%M"),
      DTime           = col_double(),
      FC              = col_double())),
  envir = efreadr_env
)

local(
  file_name_regex <- list(
    L34 = paste0(
      "^([A-Z_]{4,})",
      "_L([3-4])", # level
      "_([hdwm])", # aggr
      "_([A-Z]{2})([A-Za-z]{3})", #country and site code
      "_(20\\d{2})", # year
      "_v(\\d{2})", # version
      "?\\.txt$"),
    L12 = paste0(
      "^([A-Z_]{4,})",
      "_L([1-2])", # level
      "_([a-zA-Z]{3})", # Flx
      "_([A-Z]{2})([A-Za-z]{3})", #country and site code
      "_(20\\d{2})", # year
      "_v(\\d{2})", #version
      "_([a-zA-Z0-9]*)", # resolution
      "?\\.txt$")),
  envir = efreadr_env
)

local(
  file_name_names <- list(
    L34 = c("project", "level", "aggr", "country_id", "site_id", "year", "version"),
    L12 = c("project", "level", "type", "country_id", "site_id", "year", "version", "resolution")),
  envir = efreadr_env
)