#' @title Check presence of table and create new one if needed
#' @param data_source  Data.frame containing the data to use
#' @param file_name Name of the file to load/create
#' @param dir Path to the file storage
#' @param sel_method Method to select appropriate function to create a table
#' @param msg Message to output as a guide to alter the table
#' @param stop_session Logical. If true, the session will be stopped
#' @description Function check the presence of the selected `file_name` in
#' `dir`. If there is a file, function will load the table and return it.
#' If there is not a file, function will create the table using function 
#' selected based on `sel_method` and stop script
#' @return Object from `file_name` from `dir` 
stopcheck_table <- 
  function(
    data_source,
    file_name,
    dir,
    sel_method = c("default", "chron_control", "harm_table", "duplicates", "age_limits"),
    msg = "",
    stop_session = TRUE){
    
    util_check_class("data_source", "data.frame")
    
    util_check_class("file_name", "character")
    
    util_check_class("dir", "character")
    
    util_check_class("sel_method", "character")
    
    util_check_vector_values(
      "sel_method", 
      c("default", 
        "chron_control", 
        "harm_table",
        "duplicates", 
        "age_limits"))
    
    sel_method <- match.arg(sel_method)
    
    util_check_class("msg", "character")
    
    current_frame <- sys.nframe()
    parent_frame <-  sys.parent()
    
    current_env <- sys.frame(which = current_frame)
    parent_env <- sys.frame(which = parent_frame)
    
    # get the name of the file (or NA if missig)
    selection_file_name <-
      util_check_the_latest_file(
        file_name = file_name,
        dir = dir)
    
    # choose function based on the 'method'
    sel_function <- 
      switch(sel_method,
             "default"       = "stopcheck_make_default",
             "chron_control" = "stopcheck_make_chron_types",
             "harm_table"    = "stopcheck_make_harmonisation",
             "duplicates"    = "stopcheck_make_potent_dupl",
             "age_limits"    = "stopcheck_make_age_limits")
    
    # create a function call
    fc_command <- 
      paste0("sel_file <- ",
             sel_function,
             "(data_source = data_source, dir = dir)")
    
    # evaluate function (assign the table)
    eval(parse(text = fc_command), envir = current_env)
    
    if(is.na(selection_file_name)){
      
      util_output_comment(
        msg = "Did not detect any previous file, continue to create new one")
      
      assign(file_name, 
             sel_file,
             envir = current_env)
      
      # Save
      util_save_if_latests(
        file_name = file_name,
        dir = dir,
        prefered_format = "csv",
        compress = FALSE)
      
      # remove from global env
      rm(list = file_name, envir = current_env)
      
      # Output message
      usethis::ui_info(
        paste0(
          "Please open file '", file_name, "' in folder ", dir,".",
          msg,
          " Then proceed and re-run this whole script"))
      
      # open the folder
      util_open_dir(dir)
      
      if(stop_session == TRUE) {
        # stop session
        util_stop_quietly()  
      } else {
        return(NA_real_)
      }
      
    } else {
      
      # load file (silently)
      final_file <-
        readr::read_csv(
          paste0(dir,"/",selection_file_name),
          show_col_types = FALSE)
      
      data_list <-
        sel_file %>% 
        purrr::pluck(1) %>% 
        as.character() %>% 
        unique() %>% 
        sort()
      
      saved_list <-
        final_file %>% 
        purrr::pluck(1) %>% 
        as.character() %>% 
        unique() %>% 
        sort()
      
      
      if( all(data_list %in% saved_list)) {
        return(final_file)
      } else {
        
        usethis::ui_todo(
          paste(
            "The current data has more than data than previously saved."))
        
        # open custom menu to select confirmation of rewrite
        rewrite_confirm <-
          util_confirm(
            msg =  paste("Do you want to rewrite the previous saved table?"))
        
        if(rewrite_confirm == TRUE) {
          
          # assign to global environment 
          #   (`util_save_if_latests` has to be in global env.)
          assign(file_name, 
                 sel_file,
                 envir = current_env)
          
          # Save
          util_save_if_latests(
            file_name = file_name,
            dir = dir,
            prefered_format = "csv",
            compress = FALSE)
          
          # remove from global env
          rm(list = file_name, envir = current_env)
          
          # Output message
          usethis::ui_info(
            paste0(
              "The table has been replaced, please proceed and re-run this whole script"))
          
          
        } else {
          paste(
            "The current script cannot continue until all data are presnt in the table.",
            "Please alter/delete the lastet files in:",
            paste0(
              dir, "/Data/Input/Depositional_environment/Neotoma"),
            "and re-run this script")
          
          # open the folder
          util_open_dir(dir)
          
        }
        
        if(stop_session == TRUE) {
          # stop session
          util_stop_quietly()  
        } else {
          return(NA_real_)
        }
        
        
        
      }
    }
  }

