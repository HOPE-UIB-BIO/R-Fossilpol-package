#' @title Extract all ecological groups from dataset
#' @param data_source Data.frame with samples saved as nested information
proc_neo_get_samples_eco_group <-
  function(data_source) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "samples")

    # extract all ecological groups
    eco_group_data <-
      data_source %>%
      tidyr::unnest(samples) %>%
      tidyr::unnest(sample_detail) %>%
      dplyr::distinct(ecologicalgroup) %>%
      dplyr::arrange(ecologicalgroup)

    return(eco_group_data)
  }
