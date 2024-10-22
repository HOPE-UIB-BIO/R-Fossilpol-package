#' @title Check if there are any radiocarbon ages with negative values
#' @param data_source Chronology control table
#' @param postbomb_age Threshold old for use of the postbomb curve
#' @param rc_control_types Vector with all RC types
#' @return Logical
#' @description  Detect radiocarbon ages with negative values
#' @keywords internal
chron_check_negative_rc_ages <-
  function(data_source,
           postbomb_age = 199,
           rc_control_types = NULL) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", c("chroncontroltype", "chroncontrolage"))

    RUtilpol::check_class("postbomb_age", "numeric")

    RUtilpol::check_class("rc_control_types", c("character", "NULL"))

    # default is FALSE
    need_to_fix <- FALSE

    y <-
      data_source %>%
      dplyr::filter(chroncontroltype %in%
        rc_control_types) %>%
      dplyr::filter(chroncontrolage < postbomb_age)


    if (
      nrow(y) > 0
    ) {
      need_to_fix <- TRUE
    }

    return(need_to_fix)
  }
