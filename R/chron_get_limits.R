#' @title Detect the youngest and oldest age of chronology control points
#' @param data_source Chronology control table
#' @return Vector with youngest and oldest.
#' @keywords internal
chron_get_limits <-
  function(data_source) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "chroncontrolage")

    suppressWarnings(
      res <-
        c(
          min(data_source$chroncontrolage, na.rm = TRUE),
          max(data_source$chroncontrolage, na.rm = TRUE)
        )
    )

    return(res)
  }
