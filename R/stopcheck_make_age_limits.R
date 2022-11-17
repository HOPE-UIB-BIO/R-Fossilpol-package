#' @title Create file with age limits for regions
#' @param data_source Data.frame with the regions
#' @param dir Path to save file
#' @return Data.frame including age limits for all regions
stopcheck_make_age_limits <-
  function(data_source,
           dir) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_class("dir", "character")

    regional_age_limits <-
      data_source %>%
      dplyr::mutate(
        young_age = NA,
        old_age = NA,
        end_of_interest_period = Inf
      )

    RUtilpol::check_col_names(
      "regional_age_limits",
      c(
        "young_age",
        "old_age",
        "end_of_interest_period"
      )
    )

    return(regional_age_limits)
  }
