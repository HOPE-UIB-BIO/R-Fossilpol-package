#' @title Make Bchron Crash file
#' @param dir Path to the directory
util_make_chron_crashfile <-
  function(dir) {
    RUtilpol::check_class("dir", "character")

    crash_file <- list.files(dir)

    if (
      length(crash_file) < 1
    ) {
      readr::write_csv(
        x = as.data.frame("dataset_id"),
        file = paste0(dir, "/Crash_file.csv"),
        col_names = FALSE
      )
    }
  }
