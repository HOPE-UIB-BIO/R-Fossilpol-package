#' @title Clean the names of the columns 
#' @param data_source Data frame with names to be fixed
#' @param additional_patterns User defined patterns so be used for name changes
#' @return Data frame with new names
#' @description This is a wrapper function for a `proc_clean_names` function.
#' Clean names function first, change special characters to text an then 
#' use janitor package to clean the rest.
#' @export
proc_clean_column_names <- 
  function(data_source, additional_patterns = NULL) {
    
    util_check_class("data_source", "data.frame")
    
    util_check_class("additional_patterns", c("NULL", "character"))
    
    col_names_fresh <- 
      data_source %>% 
      dplyr::select(
        !dplyr::any_of("sample_id")) %>% 
      names()
    
    data_new <- data_source
    
    col_names_clean <- 
      proc_clean_names(col_names_fresh, 
                       additional_patterns = additional_patterns)
    
    names(data_new) <- 
      c("sample_id",col_names_clean)
    
    return(data_new)
  } 

