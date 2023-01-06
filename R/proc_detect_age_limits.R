#' @title Test if record is long enough for selected age criteria
#' @param data_source Data.frame with level ages
#' @param age_limit_young Young age limit for selected site
#' @param age_limit_old Old age limit for selected site
#' @param test_quantiles Test quantiles of age prediction?
#' @return Logical
#' @description Test if the youngest limit of any level is younger than
#' age limit as well as if the oldest limit of any level is older than age
#' limit
proc_detect_age_limits <-
  function(data_source,
           age_limit_young,
           age_limit_old,
           test_quantiles = FALSE) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_class("test_quantiles", "logical")

    if (
      test_quantiles == TRUE
    ) {
      RUtilpol::check_col_names("data_source", c("upper", "lower"))
    } else {
      RUtilpol::check_col_names("data_source", "age")
    }

    RUtilpol::check_class("age_limit_young", "numeric")

    RUtilpol::check_class("age_limit_old", "numeric")


    fulfill_criteria <- FALSE

    if (
      test_quantiles == TRUE
    ) {
      fulfill_criteria <-
        min(data_source$upper) < age_limit_young &
          max(data_source$lower) > age_limit_old
    } else {
      fulfill_criteria <-
        min(data_source$age) < age_limit_young &
          max(data_source$age) > age_limit_old
    }

    return(fulfill_criteria)
  }
