#' @title Check and adjust samples of depth-age levels and raw count data
#' @param data_source Data.frame with `sample_depth` and `raw_counts` information
#' @export
proc_neo_check_final_assembly <-
  function(data_source) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names(
      "data_source",
      c("sample_depth", "raw_counts")
    )

    # subset the samples of count data to result in same number of depth-ages levels
    sample_counts_filtered <-
      data_source %>%
      dplyr::mutate(
        sample_depth = purrr::map2(
          .x = sample_depth,
          .y = raw_counts,
          .f = ~ .x %>%
            dplyr::filter(sample_id %in% .y$sample_id)
        )
      ) %>%
      dplyr::mutate(
        # update the general number of levels
        n_sample_counts = purrr::map_dbl(raw_counts, nrow),
        n_sample_depth = purrr::map_dbl(sample_depth, nrow)
      )

    RUtilpol::check_col_names(
      "sample_counts_filtered",
      c("sample_depth", "n_sample_counts", "n_sample_depth")
    )

    # test for number of levels in sample_depth and row_counts
    RUtilpol::stop_if_not(
      all(sample_counts_filtered$n_sample_depth ==
        sample_counts_filtered$n_sample_counts),
      false_msg = "Depth-age dataset and count dataset have different number of levels",
      true_msg = "Depth-age dataset and count dataset have been checked"
    )

    # prepare final table only selecting records with both sample_depth and
    #   raw_counts
    sample_counts_checked <-
      sample_counts_filtered %>%
      dplyr::select(
        !dplyr::any_of(
          c("samples", "n_sample_depth")
        )
      ) %>%
      dplyr::filter(
        !purrr::map_lgl(
          .x = sample_depth,
          .f = ~ all(is.na(.x))
        )
      ) %>%
      dplyr::filter(
        !purrr::map_lgl(
          .x = raw_counts,
          .f = ~ all(is.na(.x))
        )
      )

    util_check_data_table(sample_counts_checked)

    neotoma_processed <-
      sample_counts_checked %>%
      dplyr::mutate(
        source_of_data = "Neotoma",
        data_publicity = "open",
        pollen_percentage = FALSE
      ) %>%
      # add missing information and check data
      util_check_data_assembly(
        data_source = .
      )


    return(neotoma_processed)
  }
