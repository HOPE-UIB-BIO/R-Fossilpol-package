#' @title Helper function to extract data safely
#' @param var Name of the variable
#' @param dataset list of lists 
util_extract_var_safe <-
  function(var, dataset){
    
    util_check_class("var", "character")
    
    util_check_class("dataset", "list")
    
    ifelse(var %in% names(dataset),
           util_replace_null_with_na(dataset[[var]]),
           NA)
  }

  
