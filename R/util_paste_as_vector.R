#' @title Paste any list of values as a vector
#' @param var_list List of values
#' @param sep Separator which should be used to wrap each object in
#' the `var_list`
#' @keywords internal
#' @description 
#' `r lifecycle::badge("deprecated")`
util_paste_as_vector <-
  function(var_list, sep = "'") {
    lifecycle::deprecate_warn(
      "0.0.2", "util_paste_as_vector()", "RUtilpol::paste_as_vector()"
    )
    paste(
      paste0(sep, var_list, sep),
      collapse = ", "
    ) %>%
      return()
  }
