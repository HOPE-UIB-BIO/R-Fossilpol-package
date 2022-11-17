#' @title Back-transform percent modern carbon to C14 ages
#' @param data_source Chronology control table
#' @return Return chronology control table with corrected ages
#' @description Back-transform percent modern carbon percentage using
#' IntCal package to real radiocarbon ages which will likely be negative and
#' fall in the future. These needs a follow step calibrating with postbomb
#' calibration curves
chron_change_pmc_to_ages <-
  function(data_source) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names(
      "data_source",
      c("chroncontroltype", "chroncontrolage", "error")
    )

    # Mark chronology control points, which have ages with decimal places
    data_w <-
      chron_decimal_ages_mark(data_source, only_rc = FALSE)

    suppressWarnings(
      res <-
        IntCal::pMC.age(
          mn = data_source$chroncontrolage,
          sdev = data_source$error
        ) %>%
        matrix(.,
          ncol = 2
        )
    )

    data_pmc_ages_fixed <-
      data_w %>%
      dplyr::mutate(
        age_backtransform = ifelse(fulfil_criteria == TRUE, res[, 1], chroncontrolage),
        error_backtransform = ifelse(fulfil_criteria == TRUE, res[, 2], error)
      ) %>%
      dplyr::mutate(
        chroncontrolage = ifelse(is.na(age_backtransform), chroncontrolage, age_backtransform),
        error = ifelse(is.na(error_backtransform), error, error_backtransform)
      ) %>%
      dplyr::select(names(data_source))

    RUtilpol::check_col_names(
      "data_pmc_ages_fixed",
      c("chroncontroltype", "chroncontrolage", "error")
    )

    return(data_pmc_ages_fixed)
  }
