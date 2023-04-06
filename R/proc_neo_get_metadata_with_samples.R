#' @title Obtain meta and sample information from Neotoma download
#' @param neotoma_download List of lists with Neotoma data
#' @param long_min Limit for the smallest longitude
#' @param long_max Limit for the largest longitude
#' @param lat_min Limit for the smallest latitude
#' @param lat_max Limit for the largest latitude
#' @param alt_min Limit for the smallest altitude
#' @param alt_max Limit for the largest altitude
#' @export
proc_neo_get_metadata_with_samples <- function(neotoma_download,
                                               long_min = NA,
                                               long_max = NA,
                                               lat_min = NA,
                                               lat_max = NA,
                                               alt_min = NA,
                                               alt_max = NA) {
  RUtilpol::check_class("neotoma_download", "list")

  RUtilpol::check_class("long_min", c("numeric", "logical"))

  RUtilpol::check_class("long_max", c("numeric", "logical"))

  RUtilpol::check_class("lat_min", c("numeric", "logical"))

  RUtilpol::check_class("lat_max", c("numeric", "logical"))

  RUtilpol::check_class("alt_min", c("numeric", "logical"))

  RUtilpol::check_class("alt_max", c("numeric", "logical"))

  current_frame <- sys.nframe()
  current_env <- sys.frame(which = current_frame)

  # Create table for all sites
  neotoma_sites_meta_data <-
    proc_neo_get_metadata(neotoma_download)

  neotoma_sites_meta_data_filtered <-
    proc_filter_by_geography(
      neotoma_sites_meta_data,
      long_min,
      long_max,
      lat_min,
      lat_max,
      alt_min,
      alt_max
    )

  RUtilpol::check_if_loaded(
    file_name = "neotoma_sites_meta_data_filtered",
    env = current_env
  )

  RUtilpol::check_class("neotoma_sites_meta_data_filtered", "data.frame")

  RUtilpol::output_comment("Neotoma meta information extracted")

  RUtilpol::stop_if_not(
    nrow(neotoma_sites_meta_data_filtered) > 0,
    false_msg = paste(
      "There are 0 records based on the selected Criteria",
      "Please change the geographical criteria."
    ),
    true_msg = paste(
      "Extracted meta information for",
      nrow(neotoma_sites_meta_data_filtered),
      "records"
    )
  )

  # Extract sample data for all sites
  neotoma_sample_data <-
    proc_neo_get_samples(neotoma_download)

  # join into one table and select important columns
  neotoma_meta_samples <-
    dplyr::inner_join(
      neotoma_sites_meta_data_filtered,
      neotoma_sample_data,
      by = "dataset_id"
    ) %>%
    dplyr::select(
      dataset_id, handle, siteid, sitename,
      long, lat, altitude,
      depositionalenvironment,
      samples,
      doi
    )

  RUtilpol::stop_if_not(
    nrow(neotoma_meta_samples) > 0,
    false_msg = paste(
      "There are 0 records with both meta data and sample data",
      "Please change the criteria."
    ),
    true_msg = paste(
      "Both meta data and sample data data were extracted for",
      nrow(neotoma_meta_samples),
      "records"
    )
  )

  return(neotoma_meta_samples)
}
