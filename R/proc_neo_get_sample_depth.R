#' @title Get sample depth data from samples
#' @param data_source Data.frame with samples saved as nested information
#' @param min_n_levels Minimal number of levels for records to be included
#' @export
proc_neo_get_sample_depth <-
  function(data_source,
           min_n_levels = 1) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "samples")

    RUtilpol::check_class("min_n_levels", "numeric")

    # extract level depth (sample_depth) information
    data_sample_depth <-
      data_source %>%
      dplyr::mutate(
        sample_depth = purrr::map(
          .progress = "Extracting sample depth",
          .x = samples,
          .f = ~ .x %>%
            dplyr::select(
              dplyr::all_of(
                c("sample_id", "depth")
              )
            ) %>%
            dplyr::arrange(depth) # arrange by depth
        )
      ) %>%
      dplyr::mutate(
        # calculate number of levels
        n_sample_depth = purrr::map_dbl(sample_depth, nrow)
      )

    data_sample_depth_filtered <-
      proc_filter_by_min(
        data_sample_depth,
        "n_sample_depth",
        min_n_levels
      )

    RUtilpol::check_col_names("data_sample_depth_filtered", c("sample_depth", "n_sample_depth"))

    # save
    return(data_sample_depth_filtered)
  }
