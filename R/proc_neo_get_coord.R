#' @title Get coordinates of sequences from Neotoma list
#' @param datasets List of lists with Neotoma datasets 
proc_neo_get_coord <- 
  function(datasets) {
    
    util_check_class("datasets", "list")
    
    purrr::map_dfr(
      .x = datasets,
      .f = ~ {
        
        # get all dataset_ids
        datase_id_table <- 
          .x$site$datasets %>% 
          purrr::map(
            .f = ~ {
              tibble::tibble(
                dsid = .x$datasetid, 
                dstype = .x$datasettype) 
            }) %>% 
          dplyr::bind_rows()
        
        # full coordinates string
        coord_string <- 
          .x$site$geography
        
        # extract just coordinates
        coord <- 
          stringr::str_replace(coord_string, '.*\\[', "") %>% 
          stringr::str_replace(., "\\].*","")
        
        tibble::tibble(
          # dataset_ids
          datase_id_table,
          # Latitude
          lat =   stringr::str_replace(coord, '.*\\,', "") %>% 
            as.double(),
          # Longitude
          long = stringr::str_replace(coord, '\\,.*', "") %>% 
            as.double()
        ) %>% 
          return()
      }
    )
  }
