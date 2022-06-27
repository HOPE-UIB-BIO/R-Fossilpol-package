#' @title Mark chronology control points, which have ages with decimal places
#' @param data_source Chronology control table
#' @param only_rc Should only radiocarbon control points be tested?
#' @param rc_control_types Vector with all radiocarbon types
#' @param age_range The age period to search for decimal places. Default is
#' 80-120 as decimal places occur around 100.
#' @return The original data.frame with a new column `fulfil_criteria` which is
#' marking ages with place
#' @description Detect ages or radiocarbon control points, which have
#' decimal places are are between 80 and 120.
chron_decimal_ages_mark <-
  function(data_source,
           only_rc = TRUE,
           rc_control_types = NULL,
           age_range = c(80, 120)) {
    util_check_class("data_source", "data.frame")

    util_check_col_names("data_source", c("chroncontroltype", "chroncontrolage"))

    util_check_class("rc_control_types", c("character", "NULL"))

    if (
      only_rc
    ) {
      data_source <-
        data_source %>%
        # only include values to calibrate
        dplyr::filter(chroncontroltype %in% rc_control_types)
    }

    data_marked <-
      data_source %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        # round the value
        age_round = round(chroncontrolage, digits = 0),
        # test the presence of decimal places
        same_age = purrr::map2(
          .x = chroncontrolage,
          .y = age_round,
          .f = ~ all.equal(.x, .y, tolerance = 1e-04)
        ),
        # detect if the age is between age_range
        #   100 is most common presence of percentage modern carbon
        correct_age_range = chroncontrolage < max(age_range) &
          chroncontrolage > min(age_range)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        same_age_logical = purrr::map_lgl(same_age, is.logical)
      ) %>%
      dplyr::mutate(
        fulfil_criteria = same_age_logical != TRUE & correct_age_range == TRUE
      ) %>%
      dplyr::select(
        dplyr::any_of(
          c(
            names(data_source),
            "fulfil_criteria"
          )
        )
      )

    util_check_col_names("data_marked", "fulfil_criteria")

    return(data_marked)
  }
