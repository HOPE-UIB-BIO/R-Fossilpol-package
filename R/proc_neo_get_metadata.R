#' @title Obtain meta information from Neotoma download
#' @param neotoma_download List of lists with Neotoma data
#' @return Data.frame with meta information such as site names, coordinates, doi, etc 
proc_neo_get_metadata <- 
  function(neotoma_download) {
  
    util_check_class("neotoma_download", "list")
    
    util_output_message(
      msg = "Extracting Neotoma meta information")
    
    # get all dataset ids
    datasets_ids <- 
      proc_neo_get_dataset_id(neotoma_download)
    
    # get all site data
    neotoma_download_sites <- 
      proc_neo_get_sites(neotoma_download)
    
   util_stop_if_not(
      length(datasets_ids) == length(neotoma_download_sites),
      true_msg = "All sites prepared",
      false_msg = "There is different number of sites than dataset IDs.")
    
    # extract metadata into a data.frame
    neotoma_sites_site_data <- 
      tibble::tibble(
        
        # dataset id
        dataset_id = datasets_ids,
        
        # site id
        siteid = neotoma_download_sites %>%
          purrr::map_chr(
            .f = ~ util_extract_var_safe("siteid", .x)),  
        
        # site name
        sitename = neotoma_download_sites %>%
          purrr::map_chr(
            .f = ~ util_extract_var_safe("sitename", .x)),
        
        # collection handle
        handle = neotoma_download_sites %>%
          purrr::map("collectionunit") %>% 
          purrr::map_chr(
            .f = ~ util_extract_var_safe("handle", .x)),
        
        # full coordinates
        coord = neotoma_download_sites %>%
          purrr::map_chr(
            .f = ~ util_extract_var_safe("geography", .x)) %>% 
          stringr::str_replace(., '.*\\[', "") %>% 
          stringr::str_replace(., "\\].*",""),
        
        # latitude
        lat = stringr::str_replace(coord, '.*\\,', "") %>% 
          as.double(),
        
        # longitude
        long = stringr::str_replace(coord, '\\,.*', "") %>% 
          as.double(),
        
        # altitude
        altitude = neotoma_download_sites %>%
          purrr::map_dbl(
            .f = ~ util_extract_var_safe("altitude", .x)),
        
        # depositional environment
        depositionalenvironment = neotoma_download_sites %>%
          purrr::map("collectionunit") %>% 
          purrr::map_chr(
            .f = ~ util_extract_var_safe("depositionalenvironment", .x)),
        
        doi = neotoma_download_sites %>%
          purrr::map("collectionunit") %>% 
          purrr::map("dataset") %>% 
          purrr::map_chr(
            .f = ~ util_extract_var_safe("doi", .x) %>% 
              unlist())
      ) %>% 
      dplyr::select(!dplyr::any_of("coord")) # do not include full coords
    
    return(neotoma_sites_site_data)
    
  }
