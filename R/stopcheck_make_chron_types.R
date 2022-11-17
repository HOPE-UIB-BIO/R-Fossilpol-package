#' @title Create file with list of chronology control point types
#' @param data_source Data.frame with the chronology control types
#' @param dir Path to save file
#' @return Data.frame including all chronology control points
#' @keywords internal
stopcheck_make_chron_types <-
  function(data_source,
           dir) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_class("dir", "character")

    chron_control_point_types <-
      data_source %>%
      dplyr::mutate(
        include = FALSE,
        calibrate = FALSE
      )

    RUtilpol::check_col_names(
      "chron_control_point_types",
      c("include", "calibrate")
    )

    return(chron_control_point_types)
  }
