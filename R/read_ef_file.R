#' Load a CSV file from the European Flux Database
#'
#' File name is parsed to extract year, site identification and aggregation type.
#' The file name must point to a valid European Fluxes file, in CSV format and must
#' resolve to a valid file format name.
#'
#' Year, file name and site identification are added as fields in the returned data frame
#' as 'efreadr_year', 'efreader_file_name' and 'efreader_site_id'.
#'
#' The fluxes files code not available measures as -9999 (integer variables) or -9999.00 (floating point variables).
#' Default behaviour of \code{read_ef_file} is to treat them as \code{NA}s. All -9999L, and -9999.0, -9999.00,
#' -9999.000 values are converted to \code{NA} during import of files.
#' Occasionally, -9999.00 (floting point representation of NA) appear in variables that are inherently integer
#' (i.e. quality control flags). This forces a type conversion of the entire variable to \code{double}.
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
#' @importFrom ensurer ensure_that
#' @importFrom magrittr %>%
#' @export
#' @examples
#' file_name <- system.file("extdata", "CEIP_EC_L4_d_FABar_2015_v02.txt", package = "efreadr")
#' read_ef_file(file_name)
#' @return a data frame as loaded from the file, added with 'efreadr_year', 'efreadr_file_name' and 'efreadr_site_id' columns, and 'efreadr_date' column for half-hourly fluxes
read_ef_file <- function(file_name, fill_value = -9999L) `: dataframe_with_filename_and_siteid` ({
  file_name %>% length() %>% ensure_that(. == 1, err_desc = "Trying to load too many files at once or none at all, try with one at a time...")

  if (is.numeric(fill_value)) {
    missing_values <- c(fill_value, sprintf(paste0("%.", 1:3, "f"), fill_value))
  } else {
    missing_values <- NULL
  }
  message(sprintf("Using %s as missing value...\n", missing_values))

  message(sprintf("Loading file '%s'...", file_name))

  file_name %>% ensure_that(file.exists(.), err_desc = "File does not exist.")


  flux_data     <- readr::read_csv(
    file_name,
    na = c(NA, "", missing_values))

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

  flux_data$efreadr_year      <- file_metadata["year"]
  flux_data$efreadr_site_id   <- paste(substr(file_metadata["site_code"], 1, 2), substr(file_metadata["site_code"], 3, 5), sep = "-")
  flux_data$efreadr_file_name <- file_name

  message(sprintf("Imported flux data for site '%s', year %s", flux_data$efreadr_site_id[1], file_metadata["year"]))

  flux_data
})
