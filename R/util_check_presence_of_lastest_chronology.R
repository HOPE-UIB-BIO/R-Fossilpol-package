#' @title Check the presence of previous results of the chronology estimation
#' @param dir Path to the data storage folder
#' @return Named vector with a summary of presence of previous chronologies and
#' predicted ages
util_check_presence_of_lastest_chronology <-
  function(dir) {
    
    util_check_class("dir", "character")
    
    # look for most recent re-calibrated data
    latest_chron_file <-
      util_check_the_latest_file(
        file_name = "chron_mod_output",
        dir = paste0(
          dir, "/Data/Processed/Chronology/Models_full"))
    
    is_latest_chron_present <- !is.na(latest_chron_file)
    
    # look for the chron info file
    latest_pred_ages_file <-
      util_check_the_latest_file(
        file_name = "chron_predicted_ages",
        dir = paste0(
          dir, "/Data/Processed/Chronology/Predicted_ages"))
    
    is_latest_pred_ages_present <- !is.na(latest_pred_ages_file)
    
    return(
      paste(
        is_latest_chron_present,
        is_latest_pred_ages_present, 
        sep = "-") %>% 
        purrr::set_names(
          nm = c(
            "is_latest_chron_present-is_latest_pred_ages_present"))
    )
    
  }
