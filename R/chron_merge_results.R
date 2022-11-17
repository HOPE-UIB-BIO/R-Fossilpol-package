#' @title Load and merge processed data with their predicted ages
#' @param dir Path to the data storage folder
#' @export
chron_merge_results <-
  function(dir) {
    RUtilpol::check_class("dir", "character")

    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    # look for most recent processed file and merge with
    #   most recent file with chronology output

    RUtilpol::output_heading(
      msg = "Loading data"
    )

    # load the processed data
    data_merged <-
      RUtilpol::get_latest_file(
        file_name = "data_merged",
        dir = paste0(dir, "/Data/Processed/Data_merged")
      ) %>%
      dplyr::select(
        !any_of(
          c(
            "chron_control",
            "n_chron_control",
            "sample_depth"
          )
        )
      )

    RUtilpol::check_if_loaded(
      file_name = "data_merged",
      env = current_env
    )

    RUtilpol::check_class("data_merged", "data.frame")

    RUtilpol::check_col_names(
      "data_merged",
      c(
        "dataset_id", "handle", "siteid", "sitename",
        "long", "lat", "altitude",
        "depositionalenvironment",
        "region", "country", "harmonisation_region",
        "raw_counts", "pollen_percentage",
        "young_age", "old_age", "end_of_interest_period",
        "source_of_data", "data_publicity",
        "doi"
      )
    )

    # load Chronology
    chron_output <-
      RUtilpol::get_latest_file(
        file_name = "chron_predicted_ages",
        dir = paste0(
          dir, "/Data/Processed/Chronology/Predicted_ages"
        )
      ) %>%
      dplyr::select(
        dplyr::all_of(
          c(
            "dataset_id",
            "chron_control_format",
            "n_chron_control",
            "chron_control_limits",
            "levels",
            "age_uncertainty"
          )
        )
      )

    RUtilpol::check_if_loaded(
      file_name = "chron_output",
      env = current_env
    )

    RUtilpol::check_class("chron_output", "data.frame")

    # merge them together
    data_ages <-
      dplyr::inner_join(
        data_merged,
        chron_output,
        by = "dataset_id"
      )

    RUtilpol::check_if_loaded(
      file_name = "data_ages",
      env = current_env
    )

    RUtilpol::check_class("data_ages", "data.frame")

    RUtilpol::output_comment("Data were succesfully merged with age-depth models")

    util_check_data_table(data_ages)


    #----------------------------------------------------------#
    # 3. Order levels in all datasets  -----
    #----------------------------------------------------------#

    RUtilpol::output_heading(
      msg = "Sorting of levels"
    )

    data_ages_sorted <-
      data_ages %>%
      dplyr::mutate(
        # arrange levels by ages
        levels = purrr::map(
          .x = levels,
          .f = ~ .x %>%
            dplyr::arrange(age)
        )
      ) %>%
      dplyr::mutate(
        # extract sample_id of levels
        valid_id = purrr::map(
          .x = levels,
          .f = ~ .x$sample_id
        )
      ) %>%
      # subset and order raw counts by sample_id from levels
      proc_subset_all_data_by_id(
        data_source = .,
        variable_vec = "raw_counts"
      )

    RUtilpol::check_col_names(
      "data_ages_sorted",
      c("levels", "raw_counts")
    )

    RUtilpol::stop_if_not(
      all(purrr::map_dbl(data_ages_sorted$levels, nrow) ==
        purrr::map_dbl(data_ages_sorted$raw_counts, nrow)),
      true_msg = "All sequences have the same number of levels",
      false_msg = paste(
        "The sorting of levels between 'levels' and 'raw_counts' was not",
        "successful."
      )
    )

    data_with_chronologies <-
      data_ages_sorted %>%
      # update the number of levels
      dplyr::mutate(
        n_sample_counts = purrr::map_dbl(
          .x = levels,
          .f = nrow
        ),
        age_min = purrr::map_dbl(
          .x = levels,
          .f = ~ .x %>%
            pluck("age") %>%
            min()
        ),
        age_max = purrr::map_dbl(
          .x = levels,
          .f = ~ .x %>%
            pluck("age") %>%
            max()
        )
      ) %>%
      dplyr::relocate(
        dataset_id, handle, siteid, sitename,
        long, lat, altitude,
        depositionalenvironment,
        region, country, harmonisation_region,
        levels, n_sample_counts, age_min, age_max, age_uncertainty,
        chron_control_format, n_chron_control, chron_control_limits,
        age_type, curve_name, postbomb_curve_name,
        raw_counts, pollen_percentage,
        young_age, old_age, end_of_interest_period,
        source_of_data, data_publicity,
        doi,
        dplyr::everything()
      )

    return(data_with_chronologies)
  }
