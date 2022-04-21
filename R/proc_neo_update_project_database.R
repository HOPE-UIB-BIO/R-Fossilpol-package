#' @title Update the project database with information from Neotoma
#' @param neotoma_sites_authors Data.frame with extracted information about 
#' authors form Neotoma
#' @param neotoma_meta_samples Data.frame with datasets presnet in data c
#' compilation
#' @param dir Path to the data storage folder
#' @export
proc_neo_update_project_database <- 
  function(neotoma_sites_authors,
           neotoma_meta_samples,
           dir) {
    
    util_check_class("neotoma_sites_authors", "data.frame")
    
    util_check_class("neotoma_meta_samples", "data.frame")
    
    util_check_col_names(
      "neotoma_sites_authors",
      c("dataset_id",
        "author_id",
        "first_name",
        "last_name",
        "email"))
    
    util_check_class("dir", "character")
    
    util_output_comment(
      msg = "Adding author information to the project database")
    
    # load dataset database
    project_dataset_database <- 
      readr::read_rds(
        paste0(
          dir, 
          "/Data/Personal_database_storage/project_dataset_database.rds"))
    
    # add data
    db_add_data(project_dataset_database) <- neotoma_meta_samples
    
    db_add_data(project_dataset_database) <- neotoma_sites_authors
    
    # save  database
    readr::write_rds(
      project_dataset_database, 
      paste0(
        dir, 
        "/Data/Personal_database_storage",
        "/project_dataset_database.rds"),
      compress = "gz")
    
    print(project_dataset_database)
    
  }
