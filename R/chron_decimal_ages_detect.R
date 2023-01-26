#' @title Test if chronology control table has ages with decimal places
#' @param data_source Chronology control table
#' @param rc_control_types Vector with all radiocarbon types
#' @return Logical
#' @description Test if chronology control table has a potential candidate
#' of Percentage modern carbon
#' @keywords internal
chron_decimal_ages_detect <-
  function(data_source,
           rc_control_types = NULL) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", c("chroncontroltype", "chroncontrolage"))

    RUtilpol::check_class("rc_control_types", c("character", "NULL"))

    marked_points <-
      chron_decimal_ages_mark(
        data_source = data_source,
        rc_control_types = rc_control_types
      )

    res <-
      any(marked_points$fulfil_criteria == TRUE, na.rm = TRUE)

    return(res)
  }
