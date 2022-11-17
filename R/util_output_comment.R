#' @title Output message in console
#' @param msg String with the message that should be printed
#' @keywords internal
#' @description 
#' `r lifecycle::badge("deprecated")`
util_output_comment <-
  function(msg = "") {
    lifecycle::deprecate_warn(
      "0.0.2", "util_output_comment()", "RUtilpol::output_comment()"
    )
    assertthat::assert_that(
      assertthat::is.string(msg),
      msg = "'msg' must be a 'string'"
    )

    cat("\n")
    usethis::ui_info(msg)
    cat("\n")
  }
