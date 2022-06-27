#' @title Change calibration curve to postbomb if necessary
#' @param data_source Chronology control table
#' @param post_bomb_cal_curve Name of the postbomb curve to use
#' @param postbomb_age Threshold old for use of the postbomb curve
#' @param rc_control_types Vector with all RC types
#' @return Chronology control table with corrected calibration curves
#' @description Change calibration curve to postbomb if necessary, based on
#' type of chronology control point types and `postbomb_age``
chron_add_postbomb_curve <-
  function(data_source,
           post_bomb_cal_curve,
           postbomb_age = 199,
           rc_control_types = NULL) {
    util_check_class("data_source", "data.frame")

    util_check_col_names(
      "data_source",
      c("chroncontroltype", "chroncontrolage", "cal_curves")
    )

    util_check_class("post_bomb_cal_curve", "character")

    util_check_class("postbomb_age", "numeric")

    util_check_class("rc_control_types", c("character", "NULL"))

    data_with_postbomb <-
      data_source %>%
      dplyr::mutate(
        cal_curves = ifelse(
          chroncontroltype %in% rc_control_types &&
            chroncontrolage < postbomb_age,
          post_bomb_cal_curve,
          cal_curves
        )
      )

    util_check_col_names("data_with_postbomb", "cal_curves")

    return(data_with_postbomb)
  }
