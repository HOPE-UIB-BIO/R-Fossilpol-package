#' @title Filter out levels and records based on the criteria
#'
#' @param data_source Data.frame with  `dataset_id`.
#' @param variable_vec Vector with name sof columns which should be filtered.
#' By default the columns are `levels`, `raw_counts`, `counts_harmonised`, and
#' `age_uncertainty`
#' @param msg A message to be outputted when error occur with age limits
#' @param filter_by_pollen_sum Logical. If true, levels and records will be
#' filtered out by `min_n_grains`, `target_n_grains`, `percentage_samples`
#' @param min_n_grains Number of individual pollen grains which each level has
#' to have
#' @param target_n_grains Number of individual pollen grains which each levels
#' 'preferably' has to have
#' @param percentage_samples Threshold of number of levels with 'preferable' counts
#' @param filter_by_age_limit Logical. If true, records will be filtered out
#' based on their age limits, defined by `young_age` and `old_age`
#' @param filter_by_extrapolation Logical. If true, records will be filtered
#' out based on the age of the last chronology control point
#' @param maximum_age_extrapolation Maximum age, which be can be extrapolated
#' beyond the oldest chronology control point
#' @param filter_by_interest_region Logical. If true, filter out levels beyond
#' the age of interest
#' @param filter_by_number_of_levels Logical. If true, filter out records based
#' on the number of levels
#' @param min_n_levels Minimal number of levels each record has to have
#' @param use_age_quantiles Logical. Should 95th age quantile be used for data
#' filtration? This will result in more stable data assembly between different
#' result of AD modelling BUT require additional data preparation before
#' analytical part
#' @param use_bookend_level Logical. Should all data filtration omit one
#' additional level in the old period? This will result of "bookend" level,
#' which can help to provide anchor information after the period of interest
#' @export
proc_filter_all_data <-
  function(data_source,
           msg = NA_character_,
           variable_vec = c(
             "levels",
             "raw_counts",
             "counts_harmonised",
             "age_uncertainty"
           ),
           filter_by_pollen_sum = TRUE,
           min_n_grains = 0,
           target_n_grains = 100,
           percentage_samples = 0,
           filter_by_age_limit = TRUE,
           filter_by_extrapolation = TRUE,
           maximum_age_extrapolation = Inf,
           filter_by_interest_region = TRUE,
           filter_by_number_of_levels = TRUE,
           min_n_levels = TRUE,
           use_age_quantiles = FALSE,
           use_bookend_level = FALSE) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_class("variable_vec", "character")

    RUtilpol::check_col_names(
      "data_source",
      c(
        "dataset_id",
        variable_vec
      )
    )


    RUtilpol::check_class("msg", "character")

    RUtilpol::check_class("filter_by_pollen_sum", "logical")

    if (
      filter_by_pollen_sum == TRUE
    ) {
      RUtilpol::check_class("min_n_grains", "numeric")

      RUtilpol::check_class("target_n_grains", "numeric")

      RUtilpol::check_class("percentage_samples", "numeric")
    }

    RUtilpol::check_class("filter_by_age_limit", "logical")

    if (
      filter_by_age_limit == TRUE
    ) {
      RUtilpol::check_col_names(
        "data_source",
        c(
          "young_age",
          "old_age"
        )
      )
    }

    RUtilpol::check_class("filter_by_extrapolation", "logical")

    if (
      filter_by_extrapolation == TRUE
    ) {
      RUtilpol::check_class("maximum_age_extrapolation", "numeric")

      RUtilpol::check_col_names("data_source", "chron_control_limits")
    }

    RUtilpol::check_class("filter_by_interest_region", "logical")

    if (
      filter_by_interest_region == TRUE
    ) {
      RUtilpol::check_col_names("data_source", "end_of_interest_period")
    }

    RUtilpol::check_class("filter_by_number_of_levels", "logical")

    if (
      filter_by_number_of_levels == TRUE
    ) {
      RUtilpol::check_class("min_n_levels", "numeric")
    }

    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    # Potential ways to filter data:
    #   A) Filter out LEVELS by pollen sum
    #   B) Filter out RECORDS based on pollen sums
    #   C) Filter out RECORDS based on age limits
    #   D) Filter out LEVELS by the last control point
    #   E) Filter out LEVELS beyond age limit
    #   F) Filters out RECORDS based on N of levels

    # remove duplicated records
    data_unique <-
      data_source %>%
      dplyr::distinct(dataset_id, .keep_all = TRUE)

    # Sort LEVELS by Age -----

    RUtilpol::output_heading(
      msg = "Sorting levels by age"
    )

    data_ages_sorted <-
      data_unique %>%
      # sort the samples by ages
      dplyr::mutate(
        valid_id = purrr::map(
          .x = levels,
          .f = ~ .x %>%
            dplyr::arrange(age) %>%
            dplyr::select(sample_id) %>%
            purrr::pluck(1)
        )
      ) %>%
      # subset
      proc_subset_all_data_by_id(
        data_source = .,
        variable_vec = variable_vec
      )

    RUtilpol::check_if_loaded(
      file_name = "data_ages_sorted",
      env = current_env
    )

    RUtilpol::check_class("data_ages_sorted", "data.frame")

    RUtilpol::output_comment("All levels were sorted by ages")

    # Filter out LEVELS with duplicated age -----

    data_ages_unique_age <-
      data_ages_sorted %>%
      # check only levels which have unique age
      dplyr::mutate(
        valid_id = purrr::map(
          .x = levels,
          .f = ~ .x %>%
            dplyr::mutate(age_diff = c(1, diff(age))) %>%
            dplyr::filter(age_diff > 0) %>%
            dplyr::select(sample_id) %>%
            purrr::pluck(1)
        )
      ) %>%
      # subset
      proc_subset_all_data_by_id(
        data_source = .,
        variable_vec = variable_vec
      )

    RUtilpol::stop_if_not(
      purrr::map_lgl(
        data_ages_unique_age$levels,
        ~ .x$age %>%
          duplicated() %>%
          any() %>%
          isFALSE()
      ) %>%
        all(),
      false_msg = "All levels DO NOT have unique age",
      true_msg = "All levels have unique age"
    )

    # Filter out by pollen sum -----

    if (
      filter_by_pollen_sum == TRUE
    ) {
      RUtilpol::output_heading(
        msg = "Filtering levels by pollen sums"
      )

      data_pollen_sum_filtered <-
        data_ages_unique_age %>%
        # get valid sample_id
        dplyr::mutate(
          valid_id = purrr::map2(
            .x = counts_harmonised,
            .y = pollen_percentage,
            .f = ~ proc_get_sampleid_rowsums(
              data_source = .x,
              min_n_grains = ifelse(.y == FALSE,
                min_n_grains,
                0
              )
            )
          )
        ) %>%
        # subset
        proc_subset_all_data_by_id(
          data_source = .,
          variable_vec = variable_vec
        )

      RUtilpol::check_if_loaded(
        file_name = "data_pollen_sum_filtered",
        env = current_env
      )

      RUtilpol::check_class("data_pollen_sum_filtered", "data.frame")

      RUtilpol::output_comment("All levels were filtered out by pollen sum")

      util_check_data_table(data_pollen_sum_filtered)

      RUtilpol::output_heading(
        msg = "Filtering records by pollen sums"
      )

      RUtilpol::stop_if_not(
        any(
          purrr::map_lgl(data_pollen_sum_filtered$young_age, is.na),
          purrr::map_lgl(data_pollen_sum_filtered$old_age, is.na)
        ) %>%
          isFALSE(),
        false_msg = paste(
          "There are some missing data for 'young_age' and 'old_age',",
          "which are needed for selected filtering.",
          msg
        ),
        true_msg = paste("All data have a age criterium")
      )

      # test if each record fulfil the criteria
      #    and filter out records which validates the criteria
      data_percentage_filtered <-
        data_pollen_sum_filtered %>%
        dplyr::mutate(
          fullfil_test = ifelse(
            test = pollen_percentage == FALSE,
            yes = purrr::pmap_lgl(
              .l = list(counts_harmonised, levels, young_age, old_age),
              .f = ~ proc_detect_rowsum_distribution(
                data_counts = ..1,
                data_levels = ..2,
                age_limit_young = ..3,
                age_limit_old = ..4,
                target_n_grains = target_n_grains,
                percentage_samples = percentage_samples
              )
            ),
            no = TRUE
          )
        ) %>%
        dplyr::filter(fullfil_test == TRUE) %>%
        dplyr::select(-fullfil_test)

      RUtilpol::check_if_loaded(
        file_name = "data_percentage_filtered",
        env = current_env
      )

      RUtilpol::check_class("data_percentage_filtered", "data.frame")

      RUtilpol::output_comment("All records were filtered out by pollen sum")

      util_check_data_table(data_percentage_filtered)
    } else {
      data_percentage_filtered <- data_ages_unique_age
    }

    # Filter out RECORDS based on age limits  -----

    if (
      filter_by_age_limit == TRUE
    ) {
      RUtilpol::output_heading(
        msg = "Filtering records by age limits"
      )

      RUtilpol::stop_if_not(
        any(
          purrr::map_lgl(data_percentage_filtered$young_age, is.na),
          purrr::map_lgl(data_percentage_filtered$old_age, is.na)
        ) %>%
          isFALSE(),
        false_msg = paste(
          "There are some missing data for 'young_age' and 'old_age',",
          "which are needed for selected filtering.",
          msg
        ),
        true_msg = paste("All data have a age criterium")
      )

      data_age_filtered <-
        data_percentage_filtered %>%
        dplyr::mutate(
          fullfil_test = purrr::pmap_lgl(
            .l = list(levels, young_age, old_age),
            .f = ~ proc_detect_age_limits(
              data_source = ..1,
              age_limit_young = ..2,
              age_limit_old = ..3,
              test_quantiles = use_age_quantiles
            )
          )
        ) %>%
        dplyr::filter(fullfil_test == TRUE) %>%
        dplyr::select(-fullfil_test)

      RUtilpol::check_if_loaded(
        file_name = "data_age_filtered",
        env = current_env
      )

      RUtilpol::check_class("data_age_filtered", "data.frame")

      RUtilpol::output_comment("All records were filtered out by age limits")

      util_check_data_table(data_age_filtered)
    } else {
      data_age_filtered <- data_percentage_filtered
    }

    # Filter out LEVELS by the last control point  -----

    if (
      filter_by_extrapolation == TRUE
    ) {
      RUtilpol::output_heading(
        msg = "Filtering out levels beyond last chron.control point (extrapolation)"
      )

      data_extrapolation_filtered <-
        data_age_filtered %>%
        dplyr::mutate(
          valid_id = purrr::map2(
            .x = levels,
            .y = chron_control_limits,
            .f = ~ proc_get_sampleid_extrapol(
              data_level = .x,
              data_chron_control_limits = .y,
              maximum_age_extrapolation = maximum_age_extrapolation,
              test_quantiles = use_age_quantiles
            )
          )
        ) %>%
        proc_subset_all_data_by_id(
          data_source = .,
          variable_vec = variable_vec
        )

      RUtilpol::check_if_loaded(
        file_name = "data_extrapolation_filtered",
        env = current_env
      )

      RUtilpol::check_class("data_extrapolation_filtered", "data.frame")

      RUtilpol::output_comment("All levels beyond last chron.control point were filtered out")

      util_check_data_table(data_extrapolation_filtered)
    } else {
      data_extrapolation_filtered <- data_age_filtered
    }

    # Filter out LEVELS beyond age limit  -----

    if (
      filter_by_interest_region == TRUE
    ) {
      RUtilpol::output_heading(
        msg = "Filtering out levels beyond age limits"
      )

      RUtilpol::stop_if_not(
        any(
          purrr::map_lgl(
            data_extrapolation_filtered$end_of_interest_period,
            is.na
          )
        ) %>%
          isFALSE(),
        false_msg = paste(
          "There are some missing data for 'end_of_interest_period',",
          "which are needed for selected filtering.",
          msg
        ),
        true_msg = paste("All data have a age criterium")
      )

      data_age_limit_filtered <-
        data_extrapolation_filtered %>%
        dplyr::mutate(
          valid_id = purrr::map2(
            .x = levels,
            .y = end_of_interest_period,
            .f = ~ proc_get_sampleid_age_lim(
              data_source = .x,
              age_limit = .y,
              test_quantiles = use_age_quantiles, # [config_criteria]
              bookend = use_bookend_level # [config_criteria]
            )
          )
        ) %>%
        proc_subset_all_data_by_id(
          data_source = .,
          variable_vec = variable_vec
        )

      RUtilpol::check_if_loaded(
        file_name = "data_age_limit_filtered",
        env = current_env
      )

      RUtilpol::check_class("data_age_limit_filtered", "data.frame")

      RUtilpol::output_comment("All levels beyond age limits were filtered out")

      util_check_data_table(data_age_limit_filtered)
    } else {
      data_age_limit_filtered <- data_extrapolation_filtered
    }

    # Filters out RECORDS based on N of levels   -----

    if (
      filter_by_number_of_levels == TRUE
    ) {
      RUtilpol::output_heading(
        msg = "Filtering out records by number of levels"
      )

      data_n_leves_filtered <-
        data_age_limit_filtered %>%
        dplyr::mutate(
          n_sample_counts = purrr::map_dbl(levels, nrow)
        ) %>%
        dplyr::filter(
          n_sample_counts >= ifelse(use_bookend_level,
            # + 1 if there is "bookend" level
            (min_n_levels + 1),
            min_n_levels
          )
        )

      RUtilpol::check_if_loaded(
        file_name = "data_n_leves_filtered",
        env = current_env
      )

      RUtilpol::check_class("data_n_leves_filtered", "data.frame")

      RUtilpol::check_col_names("data_n_leves_filtered", "n_sample_counts")

      RUtilpol::output_comment("All records were filtered out based on number of levels")

      util_check_data_table(data_n_leves_filtered)
    } else {
      data_n_leves_filtered <- data_age_limit_filtered
    }

    data_filtered <-
      data_n_leves_filtered %>%
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
      )

    prefered_columns <-
      c(
        "dataset_id", "handle", "siteid", "sitename",
        "long", "lat", "altitude",
        "depositionalenvironment",
        "region", "country", "harmonisation_region",
        "levels", "n_sample_counts", "age_min", "age_max", "age_uncertainty",
        "chron_control_format", "n_chron_control", "chron_control_limits",
        "age_type", "curve_name", "postbomb_curve_name",
        "raw_counts", "counts_harmonised", "pollen_percentage",
        "young_age", "old_age", "end_of_interest_period",
        "source_of_data", "data_publicity",
        "doi"
      )

    present_columns <-
      names(data_filtered)

    fin_columns <-
      present_columns[present_columns %in% prefered_columns]

    data_filtered_res <-
      data_filtered %>%
      dplyr::relocate(fin_columns)

    RUtilpol::check_if_loaded(
      file_name = "data_filtered_res",
      env = current_env
    )

    RUtilpol::check_class("data_filtered_res", "data.frame")

    RUtilpol::output_comment("All records and levels were filtered out based on user's preferences")

    util_check_data_table(data_filtered_res)

    return(data_filtered_res)
  }
