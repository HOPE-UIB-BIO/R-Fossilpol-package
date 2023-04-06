#' @title Prepare all chronology control tables for age-depth modelling
#' @param data_source Data.frame containing `chron_control` and `curve_name`
#' @param chron_control_types Named list with `chroncontrol_included_types` and
#' `radiocarbon_control_types`
#' @param default_thickness value to use if thickness is not present
#' @param default_error value to use if error is not present
#' @param max_age_error maximum error value to accept
#' @param guess_depth maximum depth to be accepted "Guess" as valid type
#' @param min_n_of_control_points Minimal number of chronology control points
#' each table has to have to be included. Minimal is 2
#' @param dir Path to the data storage folder
#' @description Format the chronology control tables, adjust postbomb-ages (if
#' necessary), adjust Percentage Modern Carbon (if necessary), detect age limits
#' of chronology control tables, output only variables needed for age-depth
#' modelling
#' @export
chron_prepare_for_ad_modelling <-
  function(data_source,
           chron_control_types,
           default_thickness,
           default_error,
           max_age_error,
           guess_depth,
           min_n_of_control_points = 2,
           dir) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names(
      "data_source",
      c(
        "chron_control",
        "curve_name"
      )
    )

    RUtilpol::check_class("chron_control_types", "list")

    RUtilpol::check_class("default_thickness", "numeric")

    RUtilpol::check_class("default_error", "numeric")

    if (
      missing(max_age_error)
    ) {
      max_age_error <- Inf
    }

    RUtilpol::check_class("max_age_error", "numeric")

    if (
      missing(guess_depth)
    ) {
      guess_depth <- -Inf
    }

    RUtilpol::check_class("guess_depth", "numeric")

    RUtilpol::check_class("min_n_of_control_points", "numeric")

    assertthat::assert_that(
      min_n_of_control_points >= 2,
      msg = "Minimal 'min_n_of_control_points' must be 2"
    )


    # Format the chronology control tables  -----

    RUtilpol::output_heading(
      msg = "Start formatting chron control tables"
    )

    chron_tables_formated <-
      chron_prepare_control_tables(
        data_source = data_source,
        chron_control_types = chron_control_types,
        default_thickness = default_thickness, # [config_criteria]
        default_error = default_error, # [config_criteria]
        max_age_error = max_age_error, # [config_criteria]
        guess_depth = guess_depth, # [config_criteria]
        min_n_of_control_points = min_n_of_control_points # [config_criteria]
      )

    # Post bomb adjustment  -----

    RUtilpol::output_heading(
      msg = "Start adjusting for postbomb ages"
    )

    chron_postbomb_adjusted <-
      chron_fix_postbomb(
        data_source = chron_tables_formated,
        chron_control_types = chron_control_types,
        set_postbomb_age = 199
      )


    # Percentage modern carbon   -----

    RUtilpol::output_heading(
      msg = "Start adjusting Percentage Modern Carbon"
    )

    chron_pmc_adjusted <-
      chron_fix_pmc(
        data_source = chron_postbomb_adjusted,
        chron_control_types = chron_control_types,
        dir = dir
      )

    # Chronology limits   -----

    RUtilpol::output_comment(
      msg = "Adding limits of chronology"
    )

    chron_with_limits <-
      chron_add_limits(chron_pmc_adjusted)

    # Subset variables  -----

    chron_data_prepared <-
      chron_with_limits %>%
      dplyr::select(
        dplyr::any_of(
          c(
            "dataset_id",
            "region",
            "age_type",
            "n_chron_control",
            "chron_control_format",
            "chron_control_limits"
          )
        )
      )

    util_check_data_table(chron_data_prepared)

    return(chron_data_prepared)
  }
