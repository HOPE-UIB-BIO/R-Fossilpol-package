#' @title Open selected directory
#' @param dir Path to the directory
#' @return NULL
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
util_open_dir <-
  function(dir) {
    lifecycle::deprecate_warn(
      "0.0.2", "util_open_dir()", "RUtilpol::open_dir()"
    )
    RUtilpol::check_class("dir", "character")

    if (
      .Platform["OS.type"] == "windows"
    ) {
      shell.exec(dir)
    } else {
      system(
        paste(
          Sys.getenv("R_BROWSER"), dir
        )
      )
    }
  }
