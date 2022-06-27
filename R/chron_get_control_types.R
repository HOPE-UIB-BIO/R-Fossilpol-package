#' @title Get the selected chronology control control point types
#' @param data_source Data.frame with `chron_control`
#' @param dir Path to the data storage folder
#' @description Use `stopcheck_table` to get the valid chronology control point
#' types.
#' @return Named list with `chroncontrol_included_types` and
#' `radiocarbon_control_types`
#' @export
chron_get_control_types <-
  function(data_source, dir) {
    util_check_class("data_source", "data.frame")

    util_check_col_names("data_source", "chron_control")

    util_output_message(
      msg = "Starting preparation of chron control point types"
    )

    # create list of all presented chron.control types
    chron_control_point_types_data <-
      data_source %>%
      dplyr::select(chron_control) %>%
      tidyr::unnest(cols = c(chron_control)) %>%
      dplyr::distinct(chroncontroltype) %>%
      dplyr::arrange(chroncontroltype)

    chron_control_point_types_path <-
      paste0(
        dir, "/Data/Input/Chronology_setting/Chron_control_point_types/"
      )

    # load/create table with chronology control points
    chron_control_point_types <-
      stopcheck_table(
        data_source = chron_control_point_types_data,
        file_name = "chron_control_point_types",
        dir = chron_control_point_types_path,
        sel_method = "chron_control",
        msg = paste(
          "Select which chron.control types should be kept ('include' = TRUE/FALSE)",
          "and which should be calibrated using calibration curves ('calibrate' = TRUE/FALSE)"
        )
      )

    # Create list for radiocarbon selection and calibration
    chroncontrol_included_types <-
      chron_control_point_types %>%
      dplyr::filter(include == TRUE) %>%
      purrr::pluck("chroncontroltype")

    util_open_dir_if_not(
      length(chroncontrol_included_types) > 0,
      dir = chron_control_point_types_path,
      msg = paste(
        "There is no chron.control point types included.",
        "Please review the latest file in:",
        chron_control_point_types_path
      )
    )

    util_output_comment(
      msg = paste(
        "There is", length(chroncontrol_included_types),
        "chroncontrol point types included:",
        util_paste_as_vector(chroncontrol_included_types)
      )
    )

    radiocarbon_control_types <-
      chron_control_point_types %>%
      dplyr::filter(
        include == TRUE,
        calibrate == TRUE
      ) %>%
      purrr::pluck("chroncontroltype")

    util_output_comment(
      msg = paste(
        "There is", length(radiocarbon_control_types),
        "chroncontrol point types to be calibrated:",
        util_paste_as_vector(radiocarbon_control_types)
      )
    )

    res <-
      list(
        chroncontrol_included_types,
        radiocarbon_control_types
      ) %>%
      purrr::set_names(
        nm = c("chroncontrol_included_types", "radiocarbon_control_types")
      )

    return(res)
  }
