#' @title Transform the chronology control into Bchron format and filter 
#' the points
#' @param data_source Chronology control tables
#' @param curve_name Type of calibration curve assign to the whole site
#' @param use_default_thickness Value to use if thickness is not present
#' @param use_default_error Value to use if error is not present
#' @param max_error Maximum error value to accept
#' @param valid_control_types List of valid control types
#' @param control_types_to_calibrate List of control types to use calibration curve
#' @param guess_depth Maximum depth to be accepted "Guess" as valid type
#' @return Data.frame with chronology control table
#' @description Will automatically prepare the chron control table checking 
#' and adjusting various properties. For example, duplication of depth 
#' missing values, calibration curve specifics, etc
chron_format_table <- 
  function(data_source,
           curve_name,
           use_default_thickness, 
           use_default_error,
           max_error,
           valid_control_types,
           control_types_to_calibrate,
           guess_depth) {
    
    util_check_class("data_source", "data.frame")
    
    util_check_col_names(
      "data_source", 
      c("chroncontroltype",
        "chroncontrolage",
        "agelimitolder",
        "agelimityounger",
        "depth"))
    
    util_check_class("curve_name", "character")
    
    util_check_class("use_default_thickness", "numeric")  
    
    # make sure that there is a column name 'thickness'
    if (any(names(data_source) == "thickness") == FALSE) {
      data_source <- 
        data_source %>% 
        dplyr::mutate(
          thickness = use_default_thickness)
    }
    
    util_check_col_names("data_source", "thickness")
    
    
    # make sure that there is a column name 'chroncontrolid'
    if (any(names(data_source) == "chroncontrolid") == FALSE) {
      data_source <- 
        data_source %>% 
        dplyr::mutate(
          chroncontrolid = use_default_thickness)
    }
    
    util_check_col_names("data_source", "chroncontrolid")
    
    
    util_check_class("use_default_error", "numeric")  
    
    if(missing(max_error)){
      max_error <- Inf
    }
    
    util_check_class("max_error", "numeric")  
    
    # if there is not a selected types, use all of them
    if(missing(valid_control_types)){
      valid_control_types<-
        data_source$chroncontroltype %>% 
        unique()
    }
    
    util_check_class("valid_control_types", "character")  
    
    assertthat::assert_that(
      length(valid_control_types) > 0,
      msg = "There are no 'chroncontroltype' in the 'data_source'"
    )
    
    util_check_class("control_types_to_calibrate", "character")  
    
    if(missing(guess_depth)){
      guess_depth <- -Inf
    }
    
    util_check_class("guess_depth", "numeric")  
    
    table_res <- 
      data_source %>% 
      #filter out hron.control points, without depth value
      dplyr::filter(!is.na(depth)) %>%
      # sort table by depth
      dplyr::arrange(depth) %>%
      dplyr::mutate(
        chroncontrolid = tidyr::replace_na(chroncontrolid, "no_value")) %>% 
      # replace missing `thickness` with a default one
      dplyr::mutate(
        thickness = tidyr::replace_na(thickness, use_default_thickness)) %>% 
      #calculate the mean error
      dplyr::mutate(
        error = round((agelimitolder - agelimityounger)/2)) %>% 
      # replace missing `error` with a default one
      dplyr::mutate(
        error = tidyr::replace_na(error, use_default_error)) %>%       
      # arbitrary adjustem of error to 1 year (as 0 is not possible to use)
      dplyr::mutate(
        error = replace(error, error == 0, 1)) %>% 
      # adjust error to be only positive integer
      dplyr::mutate(
        error = round(abs(error))) %>% 
      # filter out chron.control points, which has bigger error than specified
      #   by `max_error`
      dplyr::filter(error <= max_error) %>%  
      # if values for either `agelimityounger` or `agelimitolder` are misisng, 
      # calculate them using `chroncontrolage` and `error`
      dplyr::mutate(
        agelimityounger = ifelse(is.na(agelimityounger),
                                 chroncontrolage - error,
                                 agelimityounger),
        agelimitolder = ifelse(is.na(agelimitolder),
                               chroncontrolage + error,
                               agelimitolder)) %>% 
      # calculate mean age of each chron.control point
      dplyr::mutate(
        mean_age = cbind(agelimityounger, agelimitolder) %>% 
          rowMeans() %>% 
          round) %>%
      # if values `chroncontrolage` are missing `replace the with `mean_age`.
      #   `mean_age` is always present as it is calculated for all points
      dplyr::mutate(
        chroncontrolage = ifelse(is.na(chroncontrolage), mean_age, chroncontrolage)) %>%
      # filter out point, which were not able to calculate `chroncontrolage` 
      dplyr::filter(!is.na(chroncontrolage)) %>%
      # filter out points which are older that 46 ka yr. This is limitation of 
      #   Bchron  
      dplyr::filter(!(chroncontrolage > 46e3)) %>%
      # only include the selected chron.control point types
      dplyr::filter(
        chroncontroltype %in% valid_control_types) %>% 
      # add specific calibration curve based on the type of chron.control point
      dplyr::mutate(
        cal_curves = ifelse(chroncontroltype %in% control_types_to_calibrate, 
                            ifelse(curve_name == "intcal20",
                                   "intcal20",
                                   ifelse(curve_name == "SHCal20",
                                          "shcal20",
                                          "calmixed")),
                            "normal")) %>%
      # Adjustment to calibration curves and their age limitations. 
      #   Young chron.control point, which should be calibrated 
      #   ('intcal20', 'shcal20', 'calmixed') are turn back to normal as Bchron 
      #   is not able to use such limits of calibration curves. 
      #   It is more appropriate to add postbomb curve using:
      #   `chron_add_postbomb_curve')
      dplyr::mutate(
        cal_curves = ifelse((chroncontrolage < 95  &     
                               cal_curves == "intcal20"), 
                            "normal",
                            cal_curves)) %>%
      dplyr::mutate(
        cal_curves = ifelse((chroncontrolage < 118  &     
                               cal_curves == "shcal20"), 
                            "normal",
                            cal_curves)) %>%
      dplyr::mutate(
        cal_curves = ifelse((chroncontrolage < 107   &     
                               cal_curves == "calmixed"), 
                            "normal",
                            cal_curves)) %>%
      # Filter out 'Guess' chron.control types unless it is used as "core top",
      #   which is specified by low depth (`guess_depth`)
      dplyr::filter(!(chroncontroltype == "Guess" & depth > guess_depth)) %>% 
      # only include unique chron.control points
      dplyr::distinct(depth, chroncontrolage, .keep_all = TRUE) %>% 
      # select only the needed variables
      dplyr::select(
        dplyr::any_of(
          c("chroncontrolid",
            "thickness",
            "depth",
            "chroncontrolage",
            "error",
            "chroncontroltype",
            "cal_curves"))) %>% 
      # make sure it it is a tibble
      tibble::as_tibble()
    
    # sub-routine to prevent duplicated depth values. Bchron does not cope well
    #   with duplicted depth values
    if(any(duplicated(table_res$depth)) == TRUE){
      # repeat until none of the value is duplicated. For each loop, check if
      #   any values is duplicated and add very small value to it
      repeat{
        table_res <- 
          table_res %>% 
          dplyr::mutate(is_duplicated = duplicated(depth)) %>% 
          dplyr::arrange(-is_duplicated, depth) %>% 
          dplyr::mutate(order = as.double(dplyr::row_number())) %>%
          dplyr::mutate(depth = ifelse(is_duplicated == TRUE,
                                       depth + (order * 0.1),
                                       depth)) %>% 
          dplyr::select(-c(is_duplicated, order)) %>% 
          dplyr::arrange(depth)
        
        
        if(all(!duplicated(table_res$depth)) == TRUE) break
        
      }
    }
    
    util_check_col_names(
      "table_res",
      c("chroncontrolid",
        "depth",
        "thickness",
        "chroncontrolage",
        "error",
        "chroncontroltype",
        "cal_curves"
        ))
    
    return(table_res)
  }
