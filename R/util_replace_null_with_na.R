#' @title Replace NULL with N
#' @param x Any R object
#' @description Helper function. If `x` is `NULL` then replace with `NA`
#' @export
util_replace_null_with_na <- 
  function(x){
    
    ifelse(is.null(x) == TRUE, NA, x) %>% 
      return()
  }
