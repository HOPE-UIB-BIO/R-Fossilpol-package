#' @title Delete all temporary files for age-depth modelling
#' @param dir Path to the data storage folder
#' @return NULL
util_clean_chron_temp <- 
  function(dir){
    
    util_check_class("dir", "character")
    
    confirm <- 
      util_confirm(
        msg = "Do you want to clean all temporary files from age-depth modelling?")
    
    if(
      confirm == TRUE 
    ) {
      
      temp_path <- "/Data/Processed/Chronology/Temporary_output/"
      
      # load the batches
      pres_batch <- 
        list.files(
          path = paste0(dir, temp_path),
          pattern = "batch.") %>% 
        stringr::str_replace(., ".rds", "")
      
      purrr::walk(
        .x = pres_batch,
        .f = ~ file.remove(
          paste0(dir, temp_path, .x, ".rds")
        ))
      
      suppressMessages(
        suppressWarnings(
          try(
            file.remove(
              paste0(dir, temp_path, "chron_result_batch.rds")),
            silent = TRUE)
        )
      )
    }
  }
