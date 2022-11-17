#' @title Testing any criteria and open folder with msg if not true
#' @param ... Unnamed expressions that describe the conditions to be tested
#' @param env The environment in which to evaluate the test
#' @param msg Error message to be printed
#' @param dir Path to the selected folder
#' @description
#' `r lifecycle::badge("deprecated")`
#' Test a selected criteria an open selected directory if not true
util_open_dir_if_not <-
  function(...,
           env = parent.frame(),
           dir,
           msg) {
    lifecycle::deprecate_warn(
      "0.0.2", "util_open_dir_if_not()", "RUtilpol::open_dir_if_not()"
    )
    res <-
      assertthat::see_if(..., env = env, msg = msg)

    if (
      !res
    ) {
      cat("\n")
      usethis::ui_oops(
        paste("WARNING:", attr(res, "msg"))
      )
      cat("\n")

      # open the folder
      RUtilpol::open_dir(dir)

      RUtilpol::stop_quietly()
    }
  }
