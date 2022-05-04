#' @title Change name of a columns id data.frame
#' @param data_source Data.frame to fix
#' @param old_name Character. Old name of column 
#' @param new_name Character. New name of column 
#' @return Data.frame with new name
#' @description Detect if table has a column with `old_name` and replace
#' it with `new_name`
#' @export
proc_rename_column <- 
  function(data_source, 
           old_name,
           new_name) {
    
    util_check_class("data_source", "data.frame")
    
    util_check_class("old_name", "character")
    
    util_check_class("new_name", "character")
    
    # if there is an old_name in the data
    if (any(names(data_source) %in% old_name)) {
      
      data_new <-
        data_source %>% 
        dplyr::rename(!!new_name := dplyr::all_of(old_name))
      
      return(data_new)
      
    } else {
      return(data_source)
      
    }
    
  } 

