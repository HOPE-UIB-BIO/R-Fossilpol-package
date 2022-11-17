#' @title Replace NULL with N
#' @param x Any R object
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
#' Helper function. If `x` is `NULL` then replace with `NA`
util_replace_null_with_na <-
  function(x) {
    lifecycle::deprecate_warn(
      "0.0.2", "util_replace_null_with_na()", "RUtilpol::replace_null_with_na()"
    )
    ifelse(is.null(x) == TRUE, NA, x) %>%
      return()
  }
