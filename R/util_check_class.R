#' @title Assert the class of object
#' @param data_source The name of the object in quotes
#' @param sel_class The name of the class in quotes
#' @return `TRUE` if class match or error message
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
#' The function will evaluate the object of `data_source` name
#' and test if ANY of the classes in equal to `sel_class`
util_check_class <-
  function(data_source, sel_class) {
    lifecycle::deprecate_warn(
      "0.0.2", "util_check_class()", "RUtilpol::check_class()"
    )
    parent_frame <- sys.parent()

    parent_env <- sys.frame(which = parent_frame)

    data_source_class <- class(get(data_source, envir = parent_env))

    assertthat::assert_that(
      any(data_source_class %in% sel_class),
      msg = paste0(
        "'", data_source, "' must be one of the following: ",
        RUtilpol::paste_as_vector(sel_class)
      )
    )
  }
