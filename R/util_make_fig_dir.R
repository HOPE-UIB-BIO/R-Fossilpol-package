#' @title Create folders for figure outputs
#' @param dir Path to the directory
#' @param region_vector Vector with names of folders to create
#' @param sel_date Date to be used
#' @return NULL
#' @description Create a series of folders for all `region_vector` with
#' `sel_date` inside of `dir`
#' @keywords internal
util_make_fig_dir <-
  function(dir, region_vector, sel_date) {
    RUtilpol::check_class("dir", "character")

    RUtilpol::check_class("region_vector", "character")

    RUtilpol::check_class("sel_date", "Date")

    suppressWarnings(
      dir.create(
        paste0(
          dir,
          "/",
          sel_date
        )
      )
    )

    suppressWarnings(
      for (i in seq_along(region_vector)) {
        dir.create(
          paste0(
            dir,
            "/",
            sel_date,
            "/",
            region_vector[i]
          )
        )
      }
    )
  }
