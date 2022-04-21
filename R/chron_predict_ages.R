#' @title Predict ages for each level based on chronology age-depth model
#' @param data_source Bchron output
#' @param sample_data Data.frame with the `sample_id` and `depth` of levels
#' @param target_age_quantile Quantile to calculate upper and lower quantile
#' between. Must be between 0 and 1.
#' @return List of two: ages and age_position
#' @description Predict ages for each level based on Bchron including the 
#' 95th quantile. In addition, produce the age uncertainties.  
#' @export
chron_predict_ages <- 
  function(data_source, 
           sample_data, 
           target_age_quantile = 0.95) {
    
    util_check_class("data_source", "BchronologyRun")
    
    util_check_class("sample_data", "data.frame")
    
    util_check_col_names("sample_data", c("sample_id", "depth"))
    
    util_check_class("target_age_quantile", "numeric")

    assertthat::assert_that(
      target_age_quantile < 1 & target_age_quantile > 0,
      msg = "'target_age_quantile' must be > 0 and < 1")
    
    sample_id <- sample_data$sample_id
    bchron_model <- data_source
    depth <- sample_data$depth
    
    age_position <- 
      predict(bchron_model, newPositions = depth)
    
    default_age_quantiles <-  c(0.25, 0.33, 0.5, 0.666, 0.75, 0.95)
    
    if (!target_age_quantile %in% default_age_quantiles){
      default_age_quantiles <- c(default_age_quantiles, target_age_quantile)
    }
    
    quantile_limits <- (1 - default_age_quantiles)/2
    
    quantile_limits_lower <- 0 + quantile_limits
    quantile_limits_upper <- (1 - quantile_limits)
    
    prob_vector_full <-
      c(quantile_limits_lower, quantile_limits_upper, 0.5) %>% 
      sort()
    
    predicted_quantiles <- 
      apply(
        age_position, 2,
        stats::quantile,
        probs = prob_vector_full)
    
    colnames(age_position) <- sample_id
    
    age_position <- 
      age_position %>% 
      as.data.frame() %>% 
      dplyr::mutate_all(., as.integer) %>% 
      as.matrix()
    
    ages <-  
      tibble::tibble(
        sample_id = sample_id,
        depth = depth, 
        age = as.vector(predicted_quantiles["50%", ]))
    
    for(i in seq_along(prob_vector_full)){
      
      current_frame <- sys.nframe()
      current_env <- sys.frame(which = current_frame)
      
      temp_data <- 
        tibble::tibble(
          sample_id = sample_id,
          !!row.names(predicted_quantiles)[i] := as.vector(
            predicted_quantiles[row.names(predicted_quantiles)[i], ]))
      
      ages <- 
        dplyr::left_join(
          ages,
          temp_data,
          by = "sample_id")
      
      rm(temp_data, envir = current_env)
      
    }
    
    target_age_quantile_lim <- 
      (1 - target_age_quantile)/2
    upper_quantile_name <- 
      ((0 + target_age_quantile_lim)*100) %>% 
      as.character() %>% 
      paste0(.,"%")
    lower_quantile_name <- 
      ((1 - target_age_quantile_lim)*100) %>% 
      as.character() %>% 
      paste0(.,"%")
    
    ages_proccesed <- 
      ages %>% 
      dplyr::mutate(
        lower = get(lower_quantile_name),
        upper = get(upper_quantile_name)) %>% 
      dplyr::select(sample_id, depth, age, upper, lower, dplyr::everything()) %>% 
      janitor::clean_names()
    
    util_check_col_names(
      "ages_proccesed",
      c("sample_id", "depth", "age", "lower", "lower"))
    
    Bchron_age <- 
      list(
        ages = ages_proccesed,
        age_position = age_position)
    
    return(Bchron_age)
    
  }
