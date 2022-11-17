#' @title Stop the script
#' @description  
#' `r lifecycle::badge("deprecated")`
#' Stop the script without producing error
#' @keywords internal
util_stop_quietly <-
  function() {
      lifecycle::deprecate_warn(
      "0.0.2", "util_stop_quietly()", "RUtilpol::stop_quietly()"
    )
    opt <-
      options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }
