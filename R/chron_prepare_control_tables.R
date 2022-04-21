#' @title Format chronology control tables and filter out tables with specific 
#' number of control points
#' @param data_source Data.frame containing `chron_control` and `curve_name`
#' @param chron_control_types Named list with `chroncontrol_included_types` and 
#' `radiocarbon_control_types`
#' @param default_thickness Value to use if thickness is not present
#' @param default_error Value to use if error is not present
#' @param max_age_error Maximum error value to accept
#' @param guess_depth Maximum depth to be accepted "Guess" as valid type
#' @param min_n_of_control_points Minimal number of chronology control points
chron_prepare_control_tables <-
  function(data_source,
           chron_control_types,
           default_thickness, 
           default_error,
           max_age_error, 
           guess_depth,
           min_n_of_control_points = 2) {
    
    util_check_class("data_source", "data.frame")
    
    util_check_col_names(
      "data_source",
      c("chron_control",
        "curve_name")
    )
    
    util_check_class("chron_control_types", "list")
    
    util_check_class("default_thickness", "numeric")  
    
    util_check_class("default_error", "numeric")  
    
    if(missing(max_age_error)){
      max_age_error <- Inf
    }
    
    util_check_class("max_age_error", "numeric")  
    
    if(missing(guess_depth)){
      guess_depth <- -Inf
    }
    
    util_check_class("guess_depth", "numeric")  
    
    util_check_class("min_n_of_control_points", "numeric")
    
    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)
    
    assertthat::assert_that(
      min_n_of_control_points >= 2,
      msg = "Minimal 'min_n_of_control_points' must be 2")
    
    # Prepare the chron control table by checking and adjusting various properties. 
    #   For example, duplication of depth missing values, calibration curve 
    #   specifics, etc
    # !!! this takes some time !!!
    data_formated <-
      data_source %>% 
      dplyr::mutate(
        chron_control_format = purrr::map2(
          .x = chron_control,
          .y = curve_name,
          .f = ~ chron_format_table(
            data_source = .x,
            curve_name = .y,
            use_default_thickness = default_thickness, 
            use_default_error = default_error, 
            max_error = max_age_error, 
            valid_control_types = chron_control_types$chroncontrol_included_types, 
            control_types_to_calibrate = chron_control_types$radiocarbon_control_types,
            guess_depth = guess_depth 
          ))) %>% 
      # update the number of control points
      dplyr::mutate(n_chron_control = purrr::map_dbl(chron_control_format, nrow))
    
    util_check_if_loaded(
      file_name = "data_formated",
      env = current_env)
    
    util_check_class("data_formated", "data.frame")
    
    util_check_col_names(
      "data_formated", 
      c("chron_control_format", "n_chron_control"))
    
    util_output_comment("Chronology control tables were formatted")
    
    util_check_data_table(data_formated)
    
    # pre-filter by number of control points
    data_subset <-
      proc_filter_by_min(
        data_source = data_formated,
        var_name = "n_chron_control",
        min_n = min_n_of_control_points)
    
    util_check_data_table(data_subset)
    
    util_check_col_names(
      "data_subset", 
      c("chron_control_format", "n_chron_control"))
    
    return(data_subset)    
  }