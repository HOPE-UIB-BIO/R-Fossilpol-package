#' @title Confirm that the selected file is present in selected environment
#' @param file_name Name of the selected file
#' @param env The environment to test the presence of the file in
#' @param silent Logical. Should there be no message?
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
util_check_if_loaded <-
  function(file_name, env = rlang::current_env, silent = FALSE) {
    lifecycle::deprecate_warn(
      "0.0.2", "util_check_if_loaded()", "RUtilpol::check_if_loaded()"
    )
    RUtilpol::check_class("file_name", "character")

    RUtilpol::check_class("env", "environment")

    RUtilpol::stop_if_not(
      exists(eval(file_name), envir = env),
      false_msg = ifelse(
        silent == TRUE,
        "",
        paste(
          RUtilpol::paste_as_vector(file_name), "was not loaded"
        )
      ),
      true_msg = ifelse(
        silent == TRUE,
        "",
        paste(
          RUtilpol::paste_as_vector(file_name), "was successfully loaded"
        )
      )
    )
  }
