#' @title Extract data from TIF file and add as points
#' @param data_source  Data.frame containing `long` and `lat`
#' @param tif_file_name Name of a shape file to use
#' @param fill_na Logical. If TRUE, function will search for the most common value
#'  in the increasing distance of 'distance_step'
#' @param na_as_value Use for values of tiff file, which should be treated as 
#'  NA for the purposes of looking for additional value.  
#' @param distance_step Numeric. Distance in meters giving radius to increase the
#'  search radius
#' @param n_max_step Numeric. Maximum number of step increases.
#' @return Data.frame with additional columns
#' @description Extract data from tif file and add to points provided based 
#'  on the data `lat` and `long`. If `fill_na` == TRUE, function will search for 
#'  the most common value in the increasing distance of `distance_step`. 
#'  Function will stop search if all values are found.  

#' @export
geo_assign_tif <- 
  function(data_source, 
           tif_file_name = "",
           fill_na = FALSE,
           na_as_value = NULL,
           distance_step = 500,
           n_max_step = 10){
    
    util_check_class("data_source", "data.frame")
    
    util_check_col_names("data_source",  c("lat", "long"))
    
    util_check_class("tif_file_name", "character")
    
    util_check_class("fill_na", "logical")
    
    if(fill_na == TRUE){
      
      util_check_class("distance_step", "numeric")
      
      util_check_class("n_max_step", "numeric")
      
      assertthat::assert_that(
        round(n_max_step) == n_max_step,
        msg = "'n_max_step' must be an integer")  
    } else {
      assertthat::assert_that(
        is.null(na_as_value), 
        msg = "'fill_na' must be TRUE for 'na_as_value' to be set (not null)")
    }
    
    data_source_coord <- 
      data_source %>% 
      dplyr::select(lat, long)
    
    # Create a raster stack of your raster file
    raster_object <- raster::raster(tif_file_name)
    
    # Read point data, and convert them into spatial points data frame.
    point_df <- 
      data_source_coord %>% 
      as.data.frame()
    
    sp::coordinates(point_df) = ~ long + lat
    
    # Extract raster value by points
    raster_value <- raster::extract(raster_object, point_df)
    
    # replace the selected value with NA
    if(is.null(na_as_value) == FALSE & fill_na == TRUE){
      raster_value[raster_value == na_as_value] <- NA
    }
    
    # search for the closest value for NAs
    if(any(is.na(raster_value)) & fill_na == TRUE){
      
      util_output_comment(
        msg = "Searching for the closest value for NAs")
      
      for(i in 1:n_max_step){
        
        buffer_value <- distance_step * i
        
        cat(
          paste("distance =", buffer_value, "m"), "\n")
        
        point_df_sub <- 
          data_source_coord[is.na(raster_value), ] %>% 
          as.data.frame()
        
        sp::coordinates(point_df_sub) = ~ long + lat
        
        raster_value_est <- 
          raster::extract(raster_object,
                          point_df_sub,
                          buffer = buffer_value,
                          small = TRUE)
        
        # replace the selected value with NA
        if(is.null(na_as_value) == FALSE){
          raster_value_est <- 
            purrr::map(
              .x = raster_value_est,
              .f = ~ {
                .x[.x == na_as_value] <- NA
                
                return(.x)
              })
        }
        
        raster_value_sub <- 
          purrr::map_dbl(
            .x = raster_value_est,
            .f = ~ {
              res <- table(.x) %>% 
                sort(., decreasing = TRUE) %>%
                names() %>% 
                purrr::pluck(1) %>% 
                as.double()
              
              ifelse(is.null(res) == TRUE, NA_real_, res) %>% 
                return()
            }) 
        
        raster_value[is.na(raster_value)] <- raster_value_sub
        
        if(all(!is.na(raster_value))) break
      }
      
    }
    
    data_with_values <- 
      data_source %>% 
      dplyr::mutate(
        raster_values = raster_value)
    
    util_check_col_names("data_with_values", "raster_values")
    
    return(data_with_values)
  }
