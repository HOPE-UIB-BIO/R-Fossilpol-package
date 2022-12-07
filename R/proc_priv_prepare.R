#' @title Prepare other datasets
#' @param data_source Data.frame containing other data assembly
#' @param data_storage_path Path to the data storage folder
#' @param min_n_levels Minimal number of levels for sequences to be included
#' @param long_min Limit for the smallest longitude
#' @param long_max Limit for the largest longitude
#' @param lat_min Limit for the smallest latitude
#' @param lat_max Limit for the largest latitude
#' @param alt_min Limit for the smallest altitude
#' @param alt_max Limit for the largest altitude
#' @description Data is checked for names. Filtered by depositional 
#' environments, geographical location, and number of samples
#' @export
proc_priv_prepare <- function(data_source,
                              data_storage_path,
                              min_n_levels = 1,
                              long_min = NA,
                              long_max = NA,
                              lat_min = NA,
                              lat_max = NA,
                              alt_min = NA,
                              alt_max = NA) {
  RUtilpol::check_class("data_storage_path", "character")

  RUtilpol::check_class("min_n_levels", "numeric")

  RUtilpol::check_class("long_min", c("numeric", "logical"))

  RUtilpol::check_class("long_max", c("numeric", "logical"))

  RUtilpol::check_class("lat_min", c("numeric", "logical"))

  RUtilpol::check_class("lat_max", c("numeric", "logical"))

  RUtilpol::check_class("alt_min", c("numeric", "logical"))

  RUtilpol::check_class("alt_max", c("numeric", "logical"))

  # check all names
  other_dat_checked <-
    util_check_data_assembly(data_source)

  RUtilpol::output_comment(
    "Filtering by number of geography"
  )

  # filter only data within geographical criteria
  other_dat_filtered_geo <-
    proc_filter_by_geography(
      other_dat_checked,
      long_min,
      long_max,
      lat_min,
      lat_max
    )

  RUtilpol::output_comment(
    "Filtering by number of samples"
  )

  # filter by number of samples
  other_dat_filtered_samples <-
    proc_filter_by_min(
      other_dat_filtered_geo,
      "n_sample_counts",
      min_n_levels
    )

  # filter by depositional environment
  other_dat_filtered_dep_env <-
    proc_priv_filter_by_dep_env(
      other_dat_filtered_samples,
      data_storage_path
    )

  return(other_dat_filtered_dep_env)
}
