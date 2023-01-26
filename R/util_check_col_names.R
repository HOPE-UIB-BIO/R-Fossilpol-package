#' @title Check the presence of variables in data.frame
#' @param data_source Name of the data.frame to check
#' @param var_list Vector with names
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
#' Function will test the presence of ALL names in `var_list`
#' within the `data_source` data.frame and return error message
#' @keywords internal
util_check_col_names <-
  function(data_source, var_list) {
    lifecycle::deprecate_warn(
      "0.0.2", "util_check_col_names()", "RUtilpol::check_col_names()"
    )
    parent_frame <- sys.parent()

    parent_env <- sys.frame(which = parent_frame)

    data_source_obj <- get(data_source, envir = parent_env)

    RUtilpol::check_class("data_source_obj", "data.frame")

    RUtilpol::check_class("var_list", "character")

    assertthat::assert_that(
      all(var_list %in% names(data_source_obj)),
      msg = paste0(
        "'", data_source, "' must contains following columns: ",
        RUtilpol::paste_as_vector(var_list)
      )
    )
  }
