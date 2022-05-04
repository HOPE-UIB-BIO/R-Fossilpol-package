#' @title The current chronology setting and state of previous results 
#' @param dir Path to the data storage folder
#' @param calc_AD_models_denovo Setting to define if age-depth models should be
#' re-calibrated 'de novo'
#' @param predict_ages_denovo Setting to define if ages should be predicted from
#' last models 'de novo'
#' @return Named list containing the summary of the presence of previous results and current settings
#' @export
chron_get_current_state <-
  function(dir,
           calc_AD_models_denovo,
           predict_ages_denovo) {
    
    util_check_class("dir", "character")
    
    util_check_class("calc_AD_models_denovo", "logical")
    
    util_check_class("predict_ages_denovo", "logical")
    
    latest_ad_present <-
      util_check_presence_of_lastest_chronology(
        dir = dir)
    
    setting_state <-
      paste(
        calc_AD_models_denovo,
        predict_ages_denovo,
        sep = "-") %>% 
      purrr::set_names(
        nm = c("calc_AD_models_denovo-predict_ages_denovo"))
    
    util_output_comment(
      msg = paste(
        "Current setting:", "\n",
        "Re-calibrate age-depth models 'de novo' = ", calc_AD_models_denovo, "\n",
        "Predict ages for all levels 'de novo' = ", predict_ages_denovo))
    
    if(setting_state == "FALSE-FALSE" & latest_ad_present == "FALSE-FALSE" |
       setting_state == "FALSE-TRUE" & stringr::str_detect(latest_ad_present, "FALSE-*") |
       setting_state == "TRUE-TRUE" & stringr::str_detect(latest_ad_present, "FALSE-*")) {
      
      util_output_comment(
        msg = paste(
          "The selected setting does not align with the presence of previous data.","\n",
          "Please review the setting.",
          "The setting recommended for the first-time run of the Workflow is:", "\n",
          "'calc_AD_models_denovo' == TRUE\n",
          "'predict_ages_denovo' == TRUE\n"))
      
      calc_AD_models_denovo <- util_confirm_global_setting("calc_AD_models_denovo")
      
      predict_ages_denovo <- util_confirm_global_setting("predict_ages_denovo")
      
      setting_state <-
        paste(
          calc_AD_models_denovo,
          predict_ages_denovo,
          sep = "-") %>% 
        purrr::set_names(
          nm = c("calc_AD_models_denovo-predict_ages_denovo")
        )
    }
    
    return(
      list(
        latest_ad_present,
        setting_state) %>% 
        purrr::set_names(
          nm = c(
            "latest_ad_present",
            "setting_state"
          )
        )
      )
    
    
  }
