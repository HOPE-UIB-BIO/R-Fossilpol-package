#' @title Add chronologies to the meta table
#' @param neotoma_meta_samples Data.frame, which contains the meta information
#' @param chroncontrol_tables Chronology tables extracted form Neotoma
#' @export
proc_neo_add_chronologies <- 
  function(neotoma_meta_samples, chroncontrol_tables) {
    
    util_check_class("neotoma_meta_samples", "data.frame")
   
    util_check_col_names("neotoma_meta_samples", "dataset_id")
   
    util_check_class("chroncontrol_tables", "data.frame")
      
    util_check_col_names(
      "chroncontrol_tables",
      c(
        "dataset_id",
        "chron_control",
        "n_chron_control"
      ))
    
    # combine chroncontrol summary to meta data file
    neotoma_sites_meta_chron_control <- 
      dplyr::inner_join(
        neotoma_meta_samples,
        chroncontrol_tables,
        by = "dataset_id")
    
    util_check_data_table(neotoma_sites_meta_chron_control)
    
    return(neotoma_sites_meta_chron_control)
    
  }
