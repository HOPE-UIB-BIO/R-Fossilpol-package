#' @title Extract pollen data from nested Neotoma data and filter it 
#' @param data_source Data.frame with nested `sample_detail`
#' @param sel_var_element Character with selected variable element
#' @param sel_eco_group Vector with selected ecological groups 
#' @return Data.frame for each of the dataset with pollen counts
#' @description Will create data set with count data that should 
#' contain pollen types within `sel_eco_group`. The default provide 
#' the counts for the pollen data. 
proc_neo_extract_counts <- 
  function(data_source,
           sel_var_element = "pollen", 
           sel_eco_group) {
    
    current_frame <- sys.nframe()
    
    current_env <- sys.frame(which = current_frame)
    
    util_check_class("data_source", "data.frame")
    
    util_check_col_names("data_source", "sample_detail")
    
    util_check_class("sel_var_element", "character")
    
    util_check_class("sel_eco_group", "character")
    
    counts <- 
      data_source %>% 
      tidyr::unnest(sample_detail) %>% 
      dplyr::filter(elementtype %in% sel_var_element) %>%
      dplyr::filter(ecologicalgroup %in% sel_eco_group) %>%
      dplyr::group_by(sample_id, variablename) %>% 
      dplyr::summarise(.groups = "drop", value = sum(value)) %>% 
      dplyr::select(sample_id, variablename, value) %>% 
      tidyr::pivot_wider(names_from = variablename, values_from = value) 
    
    return(counts)
    
  }
