#' @title Helper function to extract data safely
#' @param var Name of the variable
#' @param dataset list of lists
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
util_extract_var_safe <-
  function(var, dataset) {
    lifecycle::deprecate_warn(
      "0.0.2", "util_extract_var_safe()", "RUtilpol::extract_var_from_list()"
    )
    RUtilpol::check_class("var", "character")

    RUtilpol::check_class("dataset", "list")

    ifelse(var %in% names(dataset),
      RUtilpol::replace_null_with_na(dataset[[var]]),
      NA
    )
  }
