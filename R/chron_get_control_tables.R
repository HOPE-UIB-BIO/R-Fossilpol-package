#' @title Extract chronology control tables from data assembly
#' @param data_source Data.frame with `dataset_id`,`region`, `chron_control`,
#' `n_chron_control`, `curve_name`, and`postbomb_curve_name`
#' @param min_n_of_control_points Minimal number of chronology control points
#' each table has to have to be included. Minimal is 2
#' @export
chron_get_control_tables <-
  function(data_source, min_n_of_control_points = 2) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names(
      "data_source",
      c(
        "dataset_id",
        "region",
        "chron_control",
        "n_chron_control",
        "curve_name",
        "postbomb_curve_name"
      )
    )

    RUtilpol::check_class("min_n_of_control_points", "numeric")

    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    assertthat::assert_that(
      min_n_of_control_points >= 2,
      msg = "'min_n_of_control_points' has to be higher than 1"
    )

    # prepare data.frame with important information to process chron.control table
    chron_chron_control_data <-
      # prefilter by number of control points
      proc_filter_by_min(
        data_source = data_source,
        var_name = "n_chron_control",
        min_n = min_n_of_control_points
      ) %>%
      # select only important columns
      dplyr::select(
        dplyr::any_of(
          c(
            "dataset_id",
            "region",
            "chron_control",
            "n_chron_control",
            "age_type",
            "curve_name",
            "postbomb_curve_name"
          )
        )
      )

    # check table
    util_check_data_table(
      data_source = chron_chron_control_data,
      msg = "Change the 'min_n_of_control_points' criterium"
    )

    chron_control_tables <-
      chron_chron_control_data %>%
      dplyr::mutate(
        # set classes to the data variables needed for processing
        chron_control = purrr::map(
          .x = chron_control,
          .f = ~ .x %>%
            dplyr::mutate(
              chroncontrolid = as.character(chroncontrolid),
              depth = as.double(depth),
              chroncontrolage = as.double(chroncontrolage),
              agelimitolder = as.double(agelimitolder),
              agelimityounger = as.double(agelimityounger),
              chroncontroltype = as.character(chroncontroltype)
            )
        )
      )

    RUtilpol::check_if_loaded(
      file_name = "chron_control_tables",
      env = current_env
    )

    RUtilpol::check_class("chron_control_tables", "data.frame")

    RUtilpol::check_col_names("chron_control_tables", "chron_control")

    util_check_data_table(
      data_source = chron_control_tables
    )

    return(chron_control_tables)
  }
