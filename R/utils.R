#' Title
#'
#' @param path
#' @param patterns
#'
#' @return
#'
#' @examples
list_files <- function(path = ".", patterns = list()) {
  bind_rows(
    lapply(
      names(patterns),
      function(pattern) {
        file_list <- list.files(path, pattern = patterns[[pattern]], full.names = TRUE)
        data.frame(pathname = file_list, level = pattern, stringsAsFactors = FALSE)
        }))
}
