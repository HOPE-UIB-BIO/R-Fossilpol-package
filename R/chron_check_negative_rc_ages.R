#' @title Check if there are any radiocarbon ages with negative values  
#' @param data_source Chronology control table
#' @param postbomb_age Threshold old for use of the postbomb curve
#' @param rc_control_types Vector with all RC types 
#' @return Logical
#' @description  Detect radiocarbon ages with negative values 
chron_check_negative_rc_ages <- 
  function(data_source, 
           postbomb_age = 199,
           rc_control_types = NULL){
    
    util_check_class("data_source", "data.frame")
    
    util_check_col_names("data_source", c("chroncontroltype", "chroncontrolage"))
   
    util_check_class("postbomb_age", "numeric")
    
    util_check_class("rc_control_types", c("character", "NULL"))
    
    # default is FALSE
    need_to_fix <- FALSE
    
    y <- 
      data_source %>% 
      dplyr::filter(chroncontroltype %in% 
                      rc_control_types) %>% 
      dplyr::filter(chroncontrolage < postbomb_age)
    
    
    if (nrow(y) > 0){
      need_to_fix <- TRUE
    }
    
    return(need_to_fix)
  }
