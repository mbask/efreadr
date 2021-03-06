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
#' @param dirs       a vector of directories where fluxes files are looked for. Defaults to current directory.
#' @param only_level levels of fluxes files (defaults to NULL). Allowed levels are (currently) 3 and 4. When NULL, either L3 and L4 files are looked for.
#' @param only_aggr  aggregations of data (defaults to NULL). Allowed aggregations are (currently) "h" (half-hourly) and "d" (daily). When NULL, either "d" and "h" files are looked for.
#' @param ... additional arguments to be passed to \code{read_ef_file}, specifically \code{fill_value}
#'
#' @importFrom ensurer  ensure_that
#' @importFrom ensurer  check_that
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom dplyr    bind_rows
#' @importFrom dplyr    setequal
#' @importFrom dplyr    setdiff
#' @importFrom dplyr    mutate
#' @importFrom dplyr    do
#' @importFrom dplyr    group_by
#' @importFrom dplyr    left_join
#' @examples
#' dir_name <- system.file(package = "efreadr", "examples")
#' read_ef_files(dir_name)
#' @export
#' @return a data frame of 3 variables: \code{level}, \code{aggr}, and \code{fluxes}. \code{fluxes} is a dataframe that
#' binds the rows of all fluxes files imported for each level/aggregation combination found.
#' Additional columns to \code{fluxes} include metadata parsed from the file names:
#' For levels 3 and 4: \code{project}, \code{level}, \code{aggr}, \code{country_id}, \code{site_id}, \code{year}, \code{version}, \code{pathname}, \code{dirname}
#' For level 2: \code{project}, \code{level}, \code{type}, \code{country_id}, \code{site_id}, \code{year}, \code{version}, \code{time_res}, \code{pathname}, \code{dirname}
read_ef_files <- function(dirs = getwd(), only_level = NULL, only_aggr = NULL, ...) `: dataframe_with_level_aggr_and_fluxes` ({

  allowed_levels <- c(2, 3, 4)
  allowed_aggr   <- c(NA, "h", "d") # NA codes for L2 flux files (L2 data are raw data not aggregated)

  dirs %>%
    dir.exists() %>%
    sum() %>%
    ensurer::ensure_that(. > 0, err_desc = "At least one directory must exist!")

  if (is.null(only_level)) {
    only_level <- allowed_levels
    message(sprintf("Using %s as level", paste(only_level, collapse = ",")))
  }

  if (is.null(only_aggr)) {
    only_aggr <- allowed_aggr
    message(sprintf("Using %s as aggregation", paste(only_aggr, collapse = ",")))
  }

  only_level %in% allowed_levels %>%
    sum() %>%
    ensurer::ensure_that(. > 0, err_desc = "Level not allowed")
  only_aggr  %in% allowed_aggr %>%
    sum() %>%
    ensurer::ensure_that(. > 0, err_desc = "Aggregation not allowed")

  file_name_regex <- get("file_name_regex", envir = efreadr_env)
  file_name_names <- get("file_name_names", envir = efreadr_env)

  file_list <- bind_rows(
    lapply(
      dirs,
      list_files_by_dir,
      pattern = file_name_regex))

  file_list %>%
    colnames(.) %>%
    dplyr::setequal(., c("pathname", "level")) %>%
    ensurer::ensure_that(isTRUE(.), err_desc = "This is embarassing, 'file_list' data.frame is malformed")
  file_list$level %>%
    unique(.) %>%
    dplyr::setdiff(., names(file_name_regex)) %>%
    length(.) %>%
    ensurer::ensure_that(. == 0, err_desc = "This is embarassing, 'level' in 'file_list' data.frame is malformed")
  file_list %>%
    nrow(.) %>%
    ensurer::ensure_that(. > 0, err_desc = "Trying to load too many files at once or none at all, try with one at a time...")

  # Parse all file names into a unique dataframe
  file_metadata_tbl <- file_list %>%
    dplyr::group_by(level) %>%
    dplyr::do(
      data.frame(
        dirname = dirname(.$pathname),
        dirdf_parse(
          pathnames = basename(.$pathname),
          regexp    = file_name_regex[[.$level[1]]],
          colnames  = file_name_names[[.$level[1]]])))


  # Filter out unwanted levels and/or aggregations and group table
  # according to level/aggregations combinations
  # import flux files as list elements
  # bind all list elements in a dataframe
  # join the dataframe to the metadata using pathname as join key
  file_data_tbl <- file_metadata_tbl %>%
    dplyr::filter(
      level %in% only_level,
      aggr  %in% only_aggr) %>%
    dplyr::group_by(level, aggr) %>%
    dplyr::do(
      fluxes = bind_rows(
        lapply(
          X   = paste(.$dirname, .$pathname, sep = "/"),
          FUN = read_ef_file,
          aggregation = .$aggr[1], # as parsed by dirdf::dirdf_parse
          year        = .$year[1], # as parsed by dirdf::dirdf_parse
          ...)) %>%
        dplyr::left_join(
        file_metadata_tbl,
        by = "pathname"))

  file_data_tbl
})
