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

#' @importFrom dplyr bind_rows
list_files_by_dir <- function(dir_name, patterns = list()) {
  dplyr::bind_rows(
    lapply(
      names(patterns),
      list_files_by_pattern,
      dir_name,
      patterns))
}

# Imported from dirdf package as it is no more developed as of 2017/05/05
# and not available for R 3.4.0+:
# https://github.com/ropenscilabs/dirdf/blob/master/R/regex_utils.R
regexprMatchToDF <- function(values, m, colnames = NULL, missing = NA_character_) {
  cstart <- attr(m, "capture.start", exact = TRUE)
  clength <- attr(m, "capture.length", exact = TRUE)
  cnames <- attr(m, "capture.names", exact = TRUE)

  if (is.null(cstart) || is.null(clength) || is.null(cnames)) {
    stop("Unexpected match format; please use regexpr or gregexpr")
  }

  if (length(cnames) == 0) {
    stop("Nothing captured")
  }

  success <- m >= 0
  colstr <- ifelse(cstart >= 1,
                   substring(values, cstart, cstart + clength - 1),
                   missing
  )
  df <- as.data.frame(colstr,
                      stringsAsFactors=FALSE,
                      check.names=FALSE
  )
  if (!is.null(colnames)) {
    names(df) <- colnames
  }

  #cbind(success = success, df)
  df
}

# Imported from dirdf package as it is no more developed as of 2017/05/05
# and not available for R 3.4.0+:
# https://github.com/ropenscilabs/dirdf/blob/master/R/templates.R
templateToRegex <- function(template) {
  stopifnot(is.character(template))
  stopifnot(length(template) == 1)

  # Match on variable names, possibly with trailing '?'
  m <- gregexpr("[a-z0-9]+\\??", template, ignore.case = TRUE)

  # mstr holds the variable names
  mstr <- regmatches(template, m)[[1]]
  # sep holds the literal values that come between the variable
  # names (the separators), including the part of the string
  # before the first variable and the part of the string after
  # the last variable, even if they are empty. So basically the
  # template is:
  # sep[1] + mstr[1] + sep[2] + mstr[2] + ... + sep[n] + mstr[n] + sep[n+1]
  sep <- regmatches(template, m, invert = TRUE)[[1]]

  stopifnot(length(sep) == length(mstr) + 1)

  # col names minus trailing ?
  bareNames <- sub("\\?", "", mstr)

  # Intentionally not using mapply because in one particular case we may
  # need to mutate sep during iteration.
  patterns <- vapply(1:length(mstr), FUN.VALUE = character(1), function(i) {
    col <- mstr[i]
    colBare <- bareNames[i]
    pre <- sep[i]
    post <- sep[i+1]

    # col is the colname, possibly with a trailing '?'
    # pre is the separator that comes to the left
    # post is the separator that comes to the right
    #
    # The result of the callback function is a regex pattern that matches the
    # previous separator and the variable data. We need the next separator
    # just to help us form the regex for the variable data.

    colPattern <- if (nchar(post) == 1) {
      sprintf("(?P<%s>[^/%s]*?)", colBare, escapeRegexBrackets(post))
    } else {
      sprintf("(?P<%s>[^/]*?)", colBare)
    }
    # See weird sub call below
    stopifnot(grepl("\\*\\?\\)$", colPattern))

    isOptional <- grepl("\\?$", col)
    if (isOptional) {
      if (i == 1 && pre == "" && grepl("^/", post)) {
        # Special case: the leading path element is an optional
        # var. In this case, we want to steal the "/" from the
        # next element.
        sep[[2]] <<- substring(sep[[2]], 2)
        pat <- optional(paste0(colPattern, escapeRegex("/")))
      } else if (pre == "/" && post != "/") {
        # If the previous separator is "/" but next separator
        # is not "/", we've made the beginning of a path element
        # optional--we won't remove the preceding separator, lest
        # we combine two variables that are at different levels
        # of the directory hierarchy. For example:
        # "one/two?_three" must not interpret "foo_bar" as
        # c(one = "foo", two = NA, three = "bar"), but rather it
        # shouldn't match at all.
        #
        # (But if both previous and post are "/", then it means
        # the entire level of hierarchy is optional, so that's
        # fine, e.g. "one/two?/three" can match on "foo/bar")
        #
        # The weird subbing of + for * is to avoid an annoying
        # edge case:
        #
        # template: "dir/prefix?-name",
        # path:     "foo/-bar",
        #
        # Without replacing * with +, prefix matches on an empty
        # string instead of not matching at all (NA); the latter
        # is what we want.
        pat <- paste0(escapeRegex(pre),
                      optional(sub("\\*\\?\\)", "+?)", colPattern))
        )
      } else {
        pat <- optional(paste0(escapeRegex(pre), colPattern))
      }
    } else {
      pat <- paste0(escapeRegex(pre), colPattern)
    }
    pat
  })

  pattern <- paste0(
    "^",  # Match the beginning of the string
    paste0(patterns, collapse = ""),  # The variables and separators
    escapeRegex(utils::tail(sep, 1)),  # The trailing separator
    "$"   # Match the end of the string
  )

  pattern
}

# Imported from dirdf package as it is no more developed as of 2017/05/05
# and not available for R 3.4.0+:
# https://github.com/ropenscilabs/dirdf/blob/master/R/templates.R
escapeRegex <- function(val) {
  gsub("([.?*+^$[\\\\(){}|\\-\\]])", "\\\\\\1", val, perl = TRUE)
}

# Imported from dirdf package as it is no more developed as of 2017/05/05
# and not available for R 3.4.0+:
# https://github.com/ropenscilabs/dirdf/blob/master/R/templates.R
escapeRegexBrackets <- function(val) {
  gsub("([\\]\\\\^-])", "\\\\\\1", val, perl = TRUE)
}

# Imported from dirdf package as it is no more developed as of 2017/05/05
# and not available for R 3.4.0+:
# https://github.com/ropenscilabs/dirdf/blob/master/R/templates.R
optional <- function(regex) {
  sprintf("(?:%s)?", regex)
}

# Imported from dirdf package as it is no more developed as of 2017/05/05
# and not available for R 3.4.0+:
# https://github.com/ropenscilabs/dirdf/blob/master/R/dirdf_parse.R

dirdf_parse <- function(pathnames, template = NULL, regexp = NULL, colnames = NULL, missing = NA_character_, ignore.case = FALSE, perl = TRUE) {
  stopifnot(xor(!is.null(template), !is.null(regexp)))
  stopifnot(length(missing) == 1L)

  if (!is.null(template)) {
    regexp <- templateToRegex(template)
    ignore.case <- FALSE
    perl <- TRUE
  }

  ## Parse
  m <- regexpr(regexp, pathnames, ignore.case = ignore.case, perl = perl)
  nonMatching <- pathnames[!is.na(match(m, -1))]
  if (length(nonMatching) > 0) {
    stop("Unexpected path(s) found:", paste0("\n", nonMatching))
  }

  df <- regexprMatchToDF(pathnames, m, colnames = colnames, missing = missing)

  ## Coerce to data.frame
  df <- cbind(df, pathname = pathnames, stringsAsFactors = FALSE)
  class(df) <- c("dirdf", class(df))

  df
}
