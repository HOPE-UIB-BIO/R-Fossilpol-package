#' @title Clean the names of the columns 
#' @param data_source Vector with names (characters)
#' @param additional_patterns User defined patterns to be used for name changes
#' @return Vector with new names
#' @description First, change special characters to text and then 
#' use janitor package to clean the rest.
#' @export
proc_clean_names <- 
  function(data_source,
           additional_patterns = NULL) {
    
    util_check_class("data_source", "character")
    
    util_check_class("additional_patterns", c("NULL", "character"))
    
    names_clean <- 
      data_source %>% 
      stringr::str_replace_all(
        .,
        c( "\\-type" = "_type",
           "\\-t" = "_t",
          "\u2264" = "_smaller_than_",
          "\u2265" = "_bigger_than_",
          "\u003c" = "_smaller_than_",
          "\u003e" = "_bigger_than_",
          "<" = "_smaller_than_",
          ">" = "_bigger_than_",
          "\\+" = "_plus_",
          "\\-" = "_minus_",
          "\\-" = "_",
          "\u00EB" = "e",
          "\u00EB" = "e",
          "\u00E4" = "a",
          "\u00B5" = "mu_")) 
    
    if(is.null(additional_patterns) == FALSE) {
      names_clean <- 
        names_clean %>% 
        stringr::str_replace_all(
          ., 
          additional_patterns)
    }
    
    names_clean_fin <-
      janitor::make_clean_names(names_clean)
    
    return(names_clean_fin)
  } 

