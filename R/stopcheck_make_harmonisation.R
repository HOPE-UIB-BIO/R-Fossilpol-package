#' @title Create file with list of all taxa to be harmonised
#' @param data_source Data.frame with the `raw_counts` data
#' @param dir Path to save file
#' @return Data.frame with all taxa
stopcheck_make_harmonisation <-
  function(data_source,
           dir) {
    util_check_class("data_source", "data.frame")

    util_check_class("dir", "character")

    util_check_col_names("data_source", "raw_counts")

    taxon_list <-
      tibble::tibble(
        taxon_name = purrr::map(data_source$raw_counts, names) %>%
          unlist() %>%
          unique() %>%
          sort(),
        level_1 = NA
      ) %>%
      dplyr::filter(taxon_name != "sample_id")

    return(taxon_list)
  }
