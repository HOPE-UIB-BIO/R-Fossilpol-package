#' @title Create file with list of all taxa to be harmonised
#' @param data_source Data.frame with the `raw_counts` data
#' @param dir Path to save file
#' @return Data.frame with all taxa
#' @keywords internal
stopcheck_make_harmonisation <-
  function(data_source,
           dir) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_class("dir", "character")

    RUtilpol::check_col_names("data_source", "raw_counts")

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
