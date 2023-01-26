#' @title Add limits of each chronology control table
#' @param data_source Data.frame with `chron_control_format`
#' @keywords internal
chron_add_limits <-
  function(data_source) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "chron_control_format")

    # Detect the first and last chronology control point
    data_with_limits <-
      data_source %>%
      dplyr::mutate(
        chron_control_limits = purrr::map(
          .x = chron_control_format,
          .f = ~ chron_get_limits(
            data_source = .x
          )
        )
      )

    RUtilpol::check_col_names("data_with_limits", "chron_control_limits")

    return(data_with_limits)
  }
