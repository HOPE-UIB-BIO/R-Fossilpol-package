#' @title Format count and depth data
#' @param data_source Data.frame with `raw_counts` and `sample_depth`
#' @description `sample_depth` Data.frame will be sorted by `depth` and then
#' `raw_counts` data.frame levels will be subset and ordered by the `sample_id`
#' @export
proc_prepare_raw_count_levels <-
  function(data_source) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", c("raw_counts", "sample_depth"))

    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    #--------------------------------------------------#
    # 1. Fixing sample_id name -----
    #--------------------------------------------------#

    RUtilpol::output_heading(
      msg = "Fixing issue with sample_id type"
    )

    data_sample_id_fix <-
      data_source %>%
      dplyr::mutate(
        raw_counts = purrr::map(
          .x = raw_counts,
          .f = ~ .x %>%
            tibble::as_tibble()
        ),
        raw_counts = purrr::map(
          .x = raw_counts,
          .f = ~ RUtilpol::rename_column(
            data_source = .x,
            old_name = "sample.id",
            new_name = "sample_id"
          )
        ),
        sample_depth = purrr::map(
          .x = sample_depth,
          .f = ~ .x %>%
            tibble::as_tibble()
        ),
        sample_depth = purrr::map(
          .x = sample_depth,
          .f = ~ RUtilpol::rename_column(
            data_source = .x,
            old_name = "sample.id",
            new_name = "sample_id"
          )
        )
      )

    RUtilpol::check_col_names("data_sample_id_fix", c("raw_counts", "sample_depth"))

    # test if all data has been fixed
    RUtilpol::stop_if_not(
      purrr::map_lgl(
        .x = data_sample_id_fix$raw_counts,
        .f = ~ any(names(.x) %in% "sample_id")
      ) %>%
        all(),
      false_msg = "Some datasets does not have 'sample_id'",
      true_msg = "All datasets have 'sample_id'"
    )

    #--------------------------------------------------#
    # 2. Fixing sample_id type ----
    #--------------------------------------------------#

    RUtilpol::output_comment(
      msg = "Making all 'sample_id' as 'character'"
    )

    # !!! this take some time !!!!
    data_sample_id <-
      data_sample_id_fix %>%
      dplyr::mutate(
        sample_depth = purrr::map(
          .x = sample_depth,
          .f = ~ .x %>%
            dplyr::mutate(sample_id = as.character(sample_id))
        ),
        raw_counts = purrr::map(
          .x = raw_counts,
          .f = ~ .x %>%
            dplyr::mutate(sample_id = as.character(sample_id))
        )
      )

    RUtilpol::check_if_loaded(
      file_name = "data_sample_id",
      env = current_env
    )

    RUtilpol::check_class("data_sample_id", "data.frame")

    RUtilpol::check_col_names("data_sample_id", c("raw_counts", "sample_depth"))

    RUtilpol::output_comment("All issues with 'sample_id' were fixed")


    #--------------------------------------------------#
    # 3. Sorting the levels ----
    #--------------------------------------------------#

    RUtilpol::output_comment(
      msg = "Sorting levels by depth"
    )

    # sort sample_depth by depth and drop NA
    data_sample_depth_sort <-
      data_sample_id %>%
      dplyr::mutate(
        sample_depth = purrr::map(
          .x = sample_depth,
          .f = ~ .x %>%
            dplyr::arrange(depth) %>%
            tidyr::drop_na()
        )
      )

    RUtilpol::check_col_names("data_sample_depth_sort", "sample_depth")

    # make a vector of sample_ids
    data_raw_counts_valid_id <-
      data_sample_depth_sort %>%
      dplyr::mutate(
        valid_id = purrr::map(
          .x = sample_depth,
          .f = ~ as.character(.x$sample_id)
        )
      )

    RUtilpol::check_col_names("data_raw_counts_valid_id", "valid_id")

    # subset
    data_raw_counts_filtered <-
      data_raw_counts_valid_id %>%
      proc_subset_all_data_by_id(
        data_source = .,
        variable_vec = "raw_counts"
      )

    # test if all datasets have same number of levels between counts and depth
    RUtilpol::stop_if_not(
      data_raw_counts_filtered %>%
        dplyr::mutate(
          n_counts = purrr::map_dbl(raw_counts, nrow),
          n_depth = purrr::map_dbl(sample_depth, nrow),
          same_n = n_counts == n_depth
        ) %>%
        purrr::pluck("same_n") %>%
        all(),
      false_msg = "Some datasets still do not have the same number of levels in counts and depth",
      true_msg = "All datasets have the same number of levels in counts and depth columns"
    )

    #--------------------------------------------------#
    # 4. Replace NA with zeros  ----
    #--------------------------------------------------#

    RUtilpol::output_comment(
      msg = "Replacing 'NA' with '0' for all taxa"
    )

    suppressWarnings(
      data_raw_counts_all_numeric <-
        data_raw_counts_filtered %>%
        dplyr::mutate(
          raw_counts = purrr::map(
            .x = raw_counts,
            .f = ~ .x %>%
              dplyr::mutate(
                dplyr::across(
                  !dplyr::contains("sample_id"),
                  ~ as.numeric(.x)
                )
              )
          )
        )
    )

    data_raw_counts_no_NA <-
      data_raw_counts_all_numeric %>%
      dplyr::mutate(
        raw_counts = purrr::map(
          .x = raw_counts,
          .f = ~ .x %>%
            dplyr::mutate(
              dplyr::across(
                tidyselect::where(is.numeric),
                ~ tidyr::replace_na(.x, 0)
              )
            )
        )
      )

    RUtilpol::check_col_names("data_raw_counts_no_NA", "raw_counts")

    #--------------------------------------------------#
    # 5. Sum taxa together and drop empty rows and columns  ----
    #--------------------------------------------------#

    RUtilpol::output_comment(
      msg = "Filtering out 'empty' taxa and levels"
    )

    data_raw_counts_no_empty_taxa <-
      data_raw_counts_no_NA %>%
      dplyr::mutate(
        raw_counts = purrr::map(
          .x = raw_counts,
          .f = ~ {
            counts <-
              .x %>%
              tidyr::pivot_longer(
                cols = -sample_id,
                names_to = "taxa",
                values_to = "val"
              ) %>%
              dplyr::group_by(sample_id, taxa) %>%
              dplyr::summarise(
                .groups = "drop",
                sum = sum(val)
              ) %>%
              tidyr::pivot_wider(
                names_from = "taxa",
                values_from = "sum"
              ) %>%
              tibble::column_to_rownames("sample_id")

            counts[rowSums(counts) > 0, colSums(counts) > 0, drop = FALSE] %>%
              tibble::rownames_to_column("sample_id") %>%
              dplyr::relocate(sample_id) %>%
              tibble::as_tibble() %>%
              return()
          }
        )
      )

    RUtilpol::check_col_names("data_raw_counts_no_empty_taxa", "raw_counts")

    # get the valid sample id
    #   This is important because they could have dropped from the empt rows
    data_raw_counts_no_empty_row_valid_id <-
      data_raw_counts_no_empty_taxa %>%
      dplyr::mutate(
        valid_id = purrr::map(
          .x = raw_counts,
          .f = ~ as.character(.x$sample_id)
        )
      )

    # subset sample_depth
    data_raw_counts_res <-
      data_raw_counts_no_empty_row_valid_id %>%
      proc_subset_all_data_by_id(
        data_source = .,
        variable_vec = "sample_depth"
      )

    RUtilpol::check_if_loaded(
      file_name = "data_raw_counts_res",
      env = current_env
    )

    RUtilpol::check_class("data_raw_counts_res", "data.frame")

    RUtilpol::check_col_names("data_raw_counts_res", "sample_depth")

    RUtilpol::output_comment("All datasets hav been prepared")

    util_check_data_table(data_raw_counts_res)

    return(data_raw_counts_res)
  }
