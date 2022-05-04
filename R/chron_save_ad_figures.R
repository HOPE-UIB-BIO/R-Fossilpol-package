#' @title Save all age-depth models by regions
#' @param dir Path to the data_storage folder
#' @param date Date to be used for making folders
#' @param text_size Text size
#' @param line_size Line size
#' @param image_width Width of image
#' @param image_height Height of image
#' @param image_units Units to measure image
#' @param image_format Format of figure
#' @return NULL
#' @description Load the most recent data, create a folder with current data 
#' and save all plots in it by regions 
#' @export
chron_save_ad_figures <- 
  function(dir,
           date,
           text_size,
           line_size,
           image_width,
           image_height,
           image_units = c("in", "cm", "mm", "px"),
           image_format = ".pdf") {
    
    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)
    
    util_check_class("dir", "character")
    
    util_check_class("date", "Date")
    
    util_check_class("text_size", "numeric")
    
    util_check_class("line_size", "numeric")
    
    util_check_class("image_width", "numeric")
    
    util_check_class("image_height", "numeric")
    
    util_check_vector_values("image_units", c("in", "cm", "mm", "px"))
    
    image_units <- match.arg(image_units)
    
    util_check_class("image_units", "character")
    
    util_check_class("image_format", "character")
    
    # load the old outputs
    chron_output <-
      util_load_latest_file(
        file_name = "chron_mod_output",
        dir =  paste0(
          dir, "/Data/Processed/Chronology/Models_full"))
    
    util_check_if_loaded(
      file_name = "chron_output",
      env = current_env)
    
    util_check_class("chron_output", "data.frame")
    
    util_check_col_names("chron_output", c("dataset_id", "bchron_mod"))
    
    # load the processed data
    data_regions <-
      util_load_latest_file(
        file_name = "data_merged",
        dir = paste0(
          dir, "/Data/Processed/Data_merged")) %>% 
      dplyr::select(
        dplyr::all_of(
          c("dataset_id", "region")))
    
    util_check_if_loaded(
      file_name = "data_regions",
      env = current_env)
    
    util_check_class("data_regions", "data.frame")
    
    util_check_col_names("data_regions", c("dataset_id", "region"))
    
    # merge them together
    data_to_plot <-
      dplyr::inner_join(
        chron_output,
        data_regions,
        by = "dataset_id")
    
    all_present_regions <-
      data_to_plot %>% 
      dplyr::distinct(region) %>% 
      purrr::pluck("region")
    
    fig_dir <- paste0(dir, "/Outputs/Figures/Chronology")
    
    # make the folders with current date
    util_make_fig_dir(
      dir = fig_dir,
      region_vector = all_present_regions,
      sel_date = date
    )
    
    most_recent_folder <-
      util_check_the_latest_file(
        file_name = NA, 
        dir = fig_dir, 
        folder = TRUE)
    
    # loop for regions, subset the data, save the data to corresponding folders
    for (i in seq_along(all_present_regions)) {
      
      current_inside_frame <- sys.nframe()
      current_inside_env <- sys.frame(which = current_inside_frame)
      
      util_output_comment(paste("Processing", all_present_regions[i]))
      
      save_dir <-
        paste0(
          fig_dir, "/", most_recent_folder, "/" , all_present_regions[i], "/")
      
      data_sub <-
        data_to_plot %>% 
        dplyr::filter(region == all_present_regions[i])
      
      if (nrow(data_sub) > 0) {
        
        cat(" - detected sites", "\n")
        
        purrr::map2(
          .x = data_sub$bchron_mod,
          .y = data_sub$dataset_id,
          .f = ~ chron_ggsave_bchron_output(
            data_source = .x,
            dataset_id = .y,
            dir = save_dir,
            text_size = text_size,
            line_size = line_size,
            image_width = image_width,
            image_height = image_height,
            image_units = image_units,
            image_format = image_format))
      }
      
      rm(data_sub, envir = current_inside_env)
      
    }
  }
