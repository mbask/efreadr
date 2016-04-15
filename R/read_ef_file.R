#' Load a CSV file from the European Flux Database
#'
#' File name is parsed to extract year, site identification and aggregation type.
#' The file name must point to a valid European Fluxes file, in CSV format and must
#' resolve to a valid file format name.
#'
#' File name is added as a field in the returned data frame
#' as  \code{pathname}. The \code{pathname} variable may be used to join a dataframe with
#' file metadata such as year, site code, level, aggregation, as \code{reaf_ef_files} does.
#'
#' The fluxes files code not available measures as -9999 (integer variables) or -9999.00 (floating point variables).
#' Default behaviour of \code{read_ef_file} is to treat them as \code{NA}s. All -9999L, and -9999.0, -9999.00,
#' -9999.000 values are converted to \code{NA} during import of files.
#' Occasionally, -9999.00 (floting point representation of NA) appear in variables that are inherently integer
#' (i.e. sqc variables in daily flux file). This forces a type conversion of the entire variable to \code{double}.
#'
#' @note For semi-hourly L4 aggregation (i.e. "h" aggregation in file name) the last row is
#' reported as month 1, day 1, hour 00:00. A normal date conversion would convert this date to
#' be the very first half-hour in January 1st of the current year whereas it should be the first half-hour of the
#' January 1st of the following year.
#' Therefore a class date field ('efreader_date') is added to the returned data frame holding the correct
#' date (ie: 1st January of the following year).
#'
#' @param file_name Full path to 1 fluxes file
#' @param fill_value a code for a not available (\code{NA}) observation in CSV file. All the observations with 'fill_value' values are converted to \code{NA}s during import. Default is -9999L.
#' @importFrom readr read_csv
#' @importFrom readr problems
#' @importFrom readr cols
#' @importFrom readr col_double
#' @importFrom readr col_integer
#' @importFrom ensurer ensure_that
#' @importFrom magrittr %>%
#' @export
#' @examples
#' file_name <- system.file(package = "efreadr", "examples", "CEIP_EC_L4_d_FABar_2015_v02.txt")
#' read_ef_file(file_name)
#' @return a data frame as loaded from the file, added with 'pathname' column, and 'efreadr_date' column for half-hourly fluxes files
read_ef_file <- function(file_name, fill_value = -9999L) `: dataframe_with_pathname` ({

  file_name %>% length() %>% ensure_that(. == 1, err_desc = "Trying to load too many files at once or none at all, try with one at a time...")
  file_name %>% ensure_that(file.exists(.), err_desc = "File does not exist.")

  file_metadata <- c(
    sapply(
      strsplit(
        basename(file_name),
        "_",
        fixed = TRUE),
      `[`,
      c(4, 5, 6)))

  file_metadata %>% length() %>% ensure_that(. == 3, err_desc = "Fluxes file name malformed; is it really a fluxes file from European Fluxes Database?")
  names(file_metadata) <- c("aggregation", "site_code", "year")

  message(sprintf("Loading file '%s'...", file_name))

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
      GPP_or_ANN     = col_double()))

  if (is.numeric(fill_value)) {
    missing_values <- c(fill_value, sprintf(paste0("%.", 1:3, "f"), fill_value))
  } else {
    missing_values <- NULL
  }
  message(sprintf("Using %s as missing value...\n", missing_values))

  flux_data <- readr::read_csv(
    file      = file_name,
    col_types = col_types_l[[file_metadata["aggregation"]]],
    na        = missing_values)

  parsing_problems <- problems(flux_data)
  if (nrow(parsing_problems) >  0) {
    message("Parsing problems:")
    print(parsing_problems)
  }

  if (file_metadata["aggregation"] == "h") {
    c("Month", "Day") %in% colnames(flux_data) %>% sum() %>% ensure_that(. == 2, err_desc = "'Month' and/or 'Day' columns are missing from fluxes file; is it really a fluxes file from European Fluxes Database?")

    flux_data$efreadr_date <- as.Date(paste(file_metadata["year"], flux_data$Month, flux_data$Day, sep = "-"))
    flux_data$efreadr_date[nrow(flux_data)] <- as.Date(
      paste(
        as.numeric(file_metadata["year"]) + 1,
        01,
        01,
        sep = "-"))
  }

  flux_data$pathname <- file_name

  message(sprintf("Imported flux data for site '%s', year %s", flux_data$efreadr_site_id[1], file_metadata["year"]))

  flux_data
})
