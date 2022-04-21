#' @title Prepare private datasets
#' @param data_source Data.frame containing private data assembly
#' @param data_storage_path Path to the data storage folder
#' @param min_n_levels Minimal number of levels for sequences to be included
#' @param long_min Limit for the smallest longitude 
#' @param long_max Limit for the largest longitude 
#' @param lat_min Limit for the smallest latitude 
#' @param lat_max Limit for the largest latitude 
#' @param alt_min Limit for the smallest altitude
#' @param alt_max Limit for the largest altitude
#' @description Data is checked for names. Filtered by depositional environments, 
#' geographical location, and number of samples
#' @export
proc_priv_prepare <- 
  function(
    data_source,
    data_storage_path,
    min_n_levels = 1,
    long_min = NA,
    long_max = NA,
    lat_min = NA,
    lat_max = NA,
    alt_min = NA,
    alt_max = NA) {
    
    util_check_class("data_storage_path", "character")
    
    util_check_class("min_n_levels", "numeric")
    
    util_check_class("long_min", c("numeric", "logical"))
    
    util_check_class("long_max", c("numeric", "logical"))
    
    util_check_class("lat_min", c("numeric", "logical"))
    
    util_check_class("lat_max", c("numeric", "logical"))
    
    util_check_class("alt_min", c("numeric", "logical"))
    
    util_check_class("alt_max", c("numeric", "logical"))
    
    # check all names
    private_dat_checked <- 
      util_check_data_assembly(data_source)
    
    util_output_comment(
      "Filtering by number of geography")
    
    # filter only data within geographical criteria 
    private_dat_filtered_geo <-
      proc_filter_by_geography(
        private_dat_checked,
        long_min, 
        long_max, 
        lat_min, 
        lat_max ) 
    
    util_output_comment(
      "Filtering by number of samples")
    
    # filter by number of samples
    private_dat_filtered_samples <-
      proc_filter_by_min(
        private_dat_filtered_geo,
        "n_sample_counts",
        min_n_levels)
    
    # filter by depositional environment
    private_dat_filtered_dep_env <- 
      proc_priv_filter_by_dep_env(
        private_dat_filtered_samples,
        data_storage_path)

    return(private_dat_filtered_dep_env)
    
  }