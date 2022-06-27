#' @title Import all datasets from selected directory
#' @param dir_files A directory path to the folder with all Excel sheets
#' @param dir Path to data storage folder
#' @param suffix A character, which would add to all new IDs
#' (dataset, authors, affiliations, etc).
#' @description The function will source all datasets from selected `dir_files`
#' folder and will compare it with all datasets in project database (object
#' of class `proj_db_class`) from `dir`. It will automatically
#' update the database if needed and use information from the database in
#' case that the dataset/author/affiliation is already present.
#' @return Data.frame with all data from the folder formatted in a
#' Neotoma-processed style data.
#' @export
import_datasets_from_folder <-
  function(dir_files,
           dir,
           suffix = "private") {

    # safety tests
    util_check_class("dir_files", "character")
    util_check_class("dir", "character")
    util_check_class("suffix", "character")

    # Get the vector of all files in focal dir_files
    sites_list <-
      list.files(
        path = dir_files,
        pattern = ".xlsx"
      ) %>%
      sort()

    n_sites <- length(sites_list)

    # Check if there are some files
    util_stop_if_not(
      n_sites >= 1,
      true_msg = paste(
        "There are", n_sites, "xlsx files detected."
      ),
      false_msg = paste("There are no 'xlsx' file in", dir_files)
    )

    # for each file
    for (i in seq_along(sites_list)) {
      # load project dataset database
      try(
        project_db <-
          readr::read_rds(
            paste0(
              dir,
              "/Data/Personal_database_storage/project_dataset_database.rds"
            )
          )
      )

      util_check_class("project_db", "proj_db_class")

      util_output_comment(
        msg = paste0("Extracting file ", i, " out of ", n_sites)
      )

      # select one site
      selected_file <- sites_list[i]

      cat(paste("Extracting file", selected_file), "\n")

      # apply custom function to extract site
      site_result <-
        extract_dataset_from_excel(
          selected_file = selected_file,
          dir = dir_files,
          suffix = suffix,
          project_db = project_db
        )

      # save single site
      if (
        i == 1
      ) {
        result_table <-
          site_result %>%
          purrr::pluck("data")
      } else {
        result_table <-
          dplyr::bind_rows(
            result_table,
            site_result %>%
              purrr::pluck("data")
          )
      }

      # save database
      readr::write_rds(
        site_result %>%
          purrr::pluck("db"),
        paste0(
          dir,
          "/Data/Personal_database_storage/project_dataset_database.rds"
        ),
        compress = "gz"
      )
    }
    return(result_table)
  }
