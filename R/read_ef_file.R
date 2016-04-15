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
#' Therefore a class date field (\code{efreader_date}) is added to the returned data frame holding the correct
#' date (ie: 1st January of the following year).
#'
#' @param file_name Full path to 1 fluxes file
#' @param aggregation character string, aggregation of the flux file to be imported (default \code{NA_character_}). This is important to properly define variable types for each aggregation type. When \code{NA_character_} it is assumed no aggregation (eg L2 files).
#' @param year integer value (default \code{NA_integer}). Important to correctly add \code{efreadr_date} field
#' @param fill_value a code for a not available (\code{NA}) observation in CSV file. All the observations with 'fill_value' values are converted to \code{NA}s during import. Default is -9999L.
#' @importFrom readr read_csv
#' @importFrom readr problems
#' @importFrom readr cols
#' @importFrom readr col_double
#' @importFrom readr col_datetime
#' @importFrom readr col_integer
#' @importFrom ensurer ensure_that
#' @importFrom magrittr %>%
#' @export
#' @examples
#' file_name <- system.file(package = "efreadr", "examples", "CEIP_EC_L4_d_FABar_2015_v02.txt")
#' read_ef_file(file_name)
#' @return a data frame as loaded from the file, added with 'pathname' column, and 'efreadr_date' column for half-hourly fluxes files
read_ef_file <- function(file_name, aggregation = NA_character_, year = NA_integer_, fill_value = -9999L) `: dataframe_with_pathname` ({

  file_name %>% length() %>% ensure_that(. == 1, err_desc = "Trying to load too many files at once or none at all, try with one at a time...")
  file_name %>% ensure_that(file.exists(.), err_desc = "File does not exist.")

  # This is to correctly match col_types_l for L2 flux files
  if (is.na(aggregation)) {
    aggregation = "R"
  }

  year <- as.integer(year)

  if (is.numeric(fill_value)) {
    missing_values <- c(fill_value, sprintf(paste0("%.", 1:3, "f"), fill_value))
  } else {
    missing_values <- NULL
  }

  message(sprintf("Loading file '%s'...", file_name))
  message(sprintf("Using %s as missing value...\n", missing_values))

  assign("aggregation", aggregation, envir = efreadr_env)
  flux_data <- readr::read_csv(
    file      = file_name,
    col_types = local(col_types_l[[aggregation]], envir = efreadr_env),
    na        = missing_values)

  parsing_problems <- problems(flux_data)
  if (nrow(parsing_problems) >  0) {
    message("Parsing problems:")
    print(parsing_problems)
  } else {
    message(sprintf("Imported %d rows", nrow(flux_data)))
  }

  if (aggregation == "h") {
    c("Month", "Day") %in% colnames(flux_data) %>% sum() %>% ensure_that(. == 2, err_desc = "'Month' and/or 'Day' columns are missing from fluxes file; is it really a fluxes file from European Fluxes Database?")

    flux_data$efreadr_date <- as.Date(
      paste(
        year,
        flux_data$Month,
        flux_data$Day,
        sep = "-"))
    flux_data$efreadr_date[nrow(flux_data)] <- as.Date(
      paste(
        year + 1,
        01,
        01,
        sep = "-"))
    message(sprintf("Last row with %s date efreadr_date", flux_data$efreadr_date[nrow(flux_data)]))
  }

  flux_data$pathname <- basename(file_name)

  flux_data
})
