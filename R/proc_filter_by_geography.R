#' @title Filter data by geographical information
#' @param data_source Data.frame with geographical information to be filtered out
#' @param long_min Limit for the smallest longitude 
#' @param long_max Limit for the largest longitude 
#' @param lat_min Limit for the smallest latitude 
#' @param lat_max Limit for the largest latitude 
#' @param alt_min Limit for the smallest altitude
#' @param alt_max Limit for the largest altitude
proc_filter_by_geography <- 
  function(data_source,
           long_min = NA,
           long_max = NA,
           lat_min = NA,
           lat_max = NA,
           alt_min = NA,
           alt_max = NA) {
    
    util_check_class("data_source", "data.frame")
    
    util_check_class("long_min", c("numeric", "logical"))
    
    util_check_class("long_max", c("numeric", "logical"))
    
    util_check_class("lat_min", c("numeric", "logical"))
    
    util_check_class("lat_max", c("numeric", "logical"))
    
    util_check_class("alt_min", c("numeric", "logical"))
    
    util_check_class("alt_max", c("numeric", "logical"))
    
    data_filter <- data_source
    
    # if there is specification of criterium, use it to filter out by
    if ("long" %in% names(data_source)) {
      
      if (!is.na(long_max)) {
        data_filter <-
          data_filter %>% 
          dplyr::filter(
            long <= long_max) #[config_criteria]
      } 
      
      if (!is.na(long_min)) {
        data_filter <-
          data_filter %>% 
          dplyr::filter(
            long >= long_min) #[config_criteria]
      } 
    }  
    
    if ("lat" %in% names(data_source)) {
      
      if (!is.na(lat_max)) {
        data_filter <-
          data_filter %>% 
          dplyr::filter(
            lat <= lat_max) #[config_criteria]
      } 
      
      if (!is.na(lat_min)) {
        data_filter <-
          data_filter %>% 
          dplyr::filter(
            lat >= lat_min) #[config_criteria]
      } 
    }  
    
    if ("altitude" %in% names(data_source)) {
      
      if (!is.na(alt_max)) {
        data_filter <-
          data_filter %>% 
          dplyr::filter(
            altitude <= alt_max) #[config_criteria]
      } 
      
      if (!is.na(alt_min)) {
        data_filter <-
          data_filter %>% 
          dplyr::filter(
            altitude >= alt_min) #[config_criteria]
      } 
    }  
    
    util_check_data_table(
      data_filter,
      msg = "Alter the geographical criteria")
    
    return(data_filter)
    
  }
