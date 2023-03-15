#' @title Change calibration curves to postbomb if needed in all chronology
#' control tables
#' @param data_source Data.frame with `dataset_id`, `chron_control_format`, and
#' `postbomb_curve_name`
#' @param chron_control_types Named list with `chroncontrol_included_types` and
#' `radiocarbon_control_types`
#' @param set_postbomb_age Age determining the start of period using postbomb
#' curves. Default value is  199 as all postbomb curves are able to handle
#' values younger than 199
#' @keywords internal
chron_fix_postbomb <-
  function(data_source, chron_control_types, set_postbomb_age = 199) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names(
      "data_source",
      c(
        "dataset_id",
        "chron_control_format",
        "postbomb_curve_name"
      )
    )

    RUtilpol::check_class("chron_control_types", "list")

    RUtilpol::check_class("set_postbomb_age", "numeric")

    # check if any radiocarbon is younger than the limitation of post-bomb
    #   curve
    data_post_bomb <-
      data_source %>%
      dplyr::mutate(
        need_to_correct = purrr::map_lgl(
          .progress = "Checking potential candidates for postbomb",
          .x = chron_control_format,
          .f = ~ chron_check_negative_rc_ages(
            data_source = .x,
            postbomb_age = set_postbomb_age,
            rc_control_types = chron_control_types$radiocarbon_control_types
          )
        )
      )

    RUtilpol::check_col_names("data_post_bomb", "need_to_correct")

    # check which datasets should be adjusted
    dataset_with_potential_postbomb <-
      data_post_bomb %>%
      dplyr::filter(need_to_correct == TRUE) %>%
      dplyr::select(dataset_id, chron_control_format) %>%
      tidyr::unnest(chron_control_format)

    if (
      nrow(dataset_with_potential_postbomb) > 0
    ) {
      dataset_with_potential_postbomb <-
        dataset_with_potential_postbomb %>%
        dplyr::filter(chroncontroltype %in% chron_control_types$radiocarbon_control_types &
          chroncontrolage < set_postbomb_age)

      RUtilpol::output_comment(
        msg = "Showing datasets that need post-bomb curve"
      )

      # show them to user
      utils::View(dataset_with_potential_postbomb)
    }

    # add postbomb curve to chron control points which needed it
    data_post_bomb_fixed <-
      data_post_bomb %>%
      dplyr::mutate(
        chron_control_format = ifelse(
          need_to_correct == TRUE,
          purrr::map2(
            .progress = TRUE,
            .x = chron_control_format,
            .y = postbomb_curve_name,
            .f = ~ chron_add_postbomb_curve(
              data_source = .x,
              post_bomb_cal_curve = .y,
              postbomb_age = set_postbomb_age,
              rc_control_types = chron_control_types$radiocarbon_control_types
            )
          ),
          chron_control_format
        )
      ) %>%
      dplyr::select(-need_to_correct)

    RUtilpol::check_col_names("data_post_bomb_fixed", "chron_control_format")

    return(data_post_bomb_fixed)
  }
