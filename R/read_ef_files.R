#' Load all European Fluxes CSV files in one or more directories, bind all observations in a list of dataframes
#'
#' European fluxes CSV files are distributed as one or more zip-compressed files from http://gaia.agraria.unitus.it .
#' Once unzipped, all CSV files are to be found in uniquely identifed directories.
#'
#' All CSV files in that or those directories will be loaded and returned as a single row-wise bound data frame.
#' The function assumes the file name regular expression pattern is like \code{^[A-Z]{4}_EC_Ln_a_[A-Z]{2}[a-ZA-Z]{3}_20[0-9]{2}_v[0-9]{2}\\.txt$}
#' where n is level \code{[1-4]} and a is aggregation period \code{[hdwm]} (optionally given as function arguments)
#'
#' @note Files in the same directory may belong to different aggregation/level
#' combinations. The returned dataframe will keep aggregation/level
#' combinations in separate rows.
#'
#' @note For semi-hourly L4 aggregation (i.e. "h" aggregation in file name) the last row is
#' reported as month 1, day 1, hour 00:00. A normal date conversion would convert this date to
#' be the very first half-hour of the current year whereas it should be the very first half-hour of the
#' following year. Therefore a class date field ('efreader_date') is added to the returned data frame holding the correct
#' date (ie: January 1st of the following year).
#'
#' @param dirs    a vector of directories where fluxes files are looked for. Defaults to current directory.
#' @param level_l level of fluxes files (defaults to NULL). Allowed levels are (currently) 3 and 4. When NULL, either L3 and L4 files are looked for.
#' @param aggregation aggregation of data (defaults to NULL) Allowed aggregations are (currently) "h" (half-hourly) and "d" (daily). When NULL, either "d" and "h" files are looked for.
#' @param ... additional arguments to be passed to \code{read_ef_file}, specifically \code{fill_value}
#'
#' @importFrom ensurer  ensure_that
#' @importFrom ensurer  check_that
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom dplyr    bind_rows
#' @importFrom dplyr    mutate
#' @importFrom dplyr    do
#' @importFrom dplyr    group_by
#' @importFrom dplyr    left_join
#' @importFrom dirdf    dirdf_parse
#' @examples
#' dir_name <- system.file(package = "efreadr", "examples")
#' read_ef_files(dir_name)
#' @export
#' @return a data frame of 3 variables: \code{level}, \code{aggr}, and \code{fluxes}. \code{fluxes} is a dataframe that
#' binds the rows of all fluxes files imported for each level/aggregation combination found.
#' Additional columns to \code{fluxes} include metadata parsed from the file names: \code{project}, \code{ec},
#' \code{level}, \code{aggr}, \code{country_id}, \code{site_id}, \code{year}, \code{version}, \code{pathname}, \code{dirname}
read_ef_files <- function(dirs = getwd(), level_l = NULL, aggregation = NULL, ...) `: dataframe_with_level_aggr_and_fluxes` ({

  allowed_levels <- c(3, 4)
  allowed_aggr   <- c("h", "d")
  file_name_regex <- paste0(
    "^([A-Z]{4})_(EC)_L([1-4])",
    "_([hdwm])_([A-Z]{2})([A-Za-z]{3})",
    "_(20\\d{2})_v(\\d{2})?\\.txt$")
  file_name_names <- c("project", "ec", "level", "aggr", "country_id", "site_id", "year", "version")

  dirs %>% dir.exists() %>% sum() %>% ensure_that(. > 0, err_desc = "At least one directory must exist!")

  if (is.null(level_l)) {
    level_l <- allowed_levels
    message(sprintf("Using %s as level", paste(level_l, collapse=",")))
  }

  if (is.null(aggregation)) {
    aggregation <- allowed_aggr
    message(sprintf("Using %s as aggregation", paste(aggregation, collapse = ",")))
  }

  level_l     %in% allowed_levels %>% sum() %>% ensure_that(. > 0, err_desc = "Level not allowed")
  aggregation %in% allowed_aggr   %>% sum() %>% ensure_that(. > 0, err_desc = "Aggregation not allowed")

  file_list <- unlist(lapply(
    dirs,
    list.files,
    pattern = file_name_regex,
    full.names = TRUE))

  file_list %>% length(.) %>% check_that(. > 0, err_desc = "Trying to load too many files at once or none at all, try with one at a time...")

  file_metadata_tbl  <- dirdf_parse(
    pathnames = basename(file_list),
    regexp    = file_name_regex,
    colnames  = file_name_names) %>%
    mutate(dirname = dirname(file_list))

  file_data_tbl <- file_metadata_tbl %>%
    filter(level %in% level_l, aggr %in% aggregation) %>%
    group_by(level, aggr) %>%
    do(
      fluxes = bind_rows(
        lapply(
        X   = paste(.$dirname, .$pathname, sep = "/"),
        FUN = read_ef_file,
        ...)) %>%
      left_join(
        file_metadata_tbl,
        by = "pathname"))

  file_data_tbl
})
