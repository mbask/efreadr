list_files_by_pattern <- function(pattern, dir_name, patterns) {
  file_list <- list.files(
    dir_name,
    pattern    = patterns[[pattern]],
    full.names = TRUE)

  if (length(file_list) > 0) {
    return_data <- data.frame(
      pathname         = file_list,
      level            = pattern,
      stringsAsFactors = FALSE)
  } else {
    return_data <- NULL
  }

  return_data
}

list_files_by_dir <- function(dir_name, patterns = list()) {
  bind_rows(
    lapply(
      names(patterns),
      list_files_by_pattern,
      dir_name,
      patterns))
}
