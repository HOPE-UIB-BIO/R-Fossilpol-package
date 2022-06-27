#' @title Open selected directory
#' @param dir Path to the directory
#' @return NULL
#' @export
util_open_dir <-
  function(dir) {
    util_check_class("dir", "character")

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
