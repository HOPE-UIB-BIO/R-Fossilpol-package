#' @title Merge data from Neotoma and Private source  
#' @param data_storage_path Path to the data storage folder
#' @param private_data Logical value if to include private source of data
#' @export
proc_get_merged_dataset <- 
  function(data_storage_path, private_data = FALSE){
    
    util_check_class("data_storage_path", "character")
    
    util_check_class("private_data", "logical")
    
    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)
    
    util_output_message(
      msg = "Loading datasets")
    
    # Load processed Neotoma sequences
    neotoma_processed <- 
      util_load_latest_file(
        file_name = "neotoma_processed",
        dir = paste0(data_storage_path, "/Data/Processed/Neotoma_processed"))
    
    util_check_if_loaded(
      file_name = "neotoma_processed",
      env = current_env)
    
    util_check_class("neotoma_processed", "data.frame")
    
    if(private_data == TRUE){
      
      # load processed private sequences
      private_processed <- 
        util_load_latest_file(
          file_name = "private_data_prepared",
          dir = paste0(data_storage_path, "/Data/Processed/Private"))   
      
      util_check_if_loaded(
        file_name = "private_processed",
        env = current_env)
      
      util_check_class("private_processed", "data.frame")
      
      # make sure that dataset_id are not duplicated
      neotoma_filtered <- 
        neotoma_processed %>% 
        dplyr::filter(!dataset_id %in% private_processed$dataset_id)
      
      # merge file together
      data_full <-
        dplyr::bind_rows(
          neotoma_filtered,  
          private_processed) %>% 
        dplyr::mutate(
          # mark everything as public, if it is not private
          data_publicity = ifelse(data_publicity != "private",
                                  "public", "private"),
          # detect if data is in pollen percentages
          pollen_percentage = ifelse(is.na(pollen_percentage), 
                                     FALSE, 
                                     pollen_percentage),
          # mark everything as private if not stated otherwise
          source_of_data = ifelse(is.na(source_of_data),
                                  "private",
                                  source_of_data))
      
      util_check_if_loaded(
        file_name = "data_full",
        env = current_env)
      
      util_check_class("data_full", "data.frame")
      
      util_check_col_names(
        "data_full",
        c("data_publicity", "pollen_percentage", "source_of_data")
      )
      
      util_output_comment("Neotoma data was loanded and merged with Private")
      
    } else {
      data_full <- neotoma_processed
      
      util_check_if_loaded(
        file_name = "data_full",
        env = current_env)
      
      util_check_class("data_full", "data.frame")
      
      util_output_comment("Neotoma data was loaded")
    
    }
    
    util_check_data_table(data_full)
    
    return(data_full)
  
  }
