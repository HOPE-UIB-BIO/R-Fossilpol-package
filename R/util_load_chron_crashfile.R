#' @title Load last Crash file or create dummy if not present
#' @param dir Path to the data storage folder
util_load_chron_crashfile <- 
  function(dir) {
    
   util_check_class("dir", "character")
    
    # set path to Crash file
    crash_file_path <-
      paste0(dir, "/Data/Input/Chronology_setting/Bchron_crash/")
    
    # make empty file if needed
   util_make_chron_crashfile(crash_file_path)
    
    # load crash file         
    crash_file <-
      readr::read_csv(
        paste0(crash_file_path, "Crash_file.csv"),
        show_col_types = FALSE)
    
   util_check_class("crash_file", "data.frame")
    
    return(crash_file)
    
  }
