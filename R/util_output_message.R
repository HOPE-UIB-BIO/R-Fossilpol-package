#' @title Output boxed message in console
#' @param msg String with the message that should be printed
#' @return NULL
#' @keywords internal
#' @description 
#' `r lifecycle::badge("deprecated")`
util_output_message <-
  function(msg = "") {
    lifecycle::deprecate_warn(
      "0.0.2", "util_output_message()", "RUtilpol::output_heading()"
    )
    assertthat::assert_that(
      assertthat::is.string(msg),
      msg = "'msg' must be a 'string'"
    )

    sep_line <-
      "#----------------------------------------------------------#"

    cat("\n")
    usethis::ui_line(sep_line)
    RUtilpol::output_comment(msg = msg)
    usethis::ui_line(sep_line)
    cat("\n")
  }
