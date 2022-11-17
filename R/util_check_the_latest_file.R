#' @title Detect the newest version of the selected file
#' @param file_name Name of the object to save in quotes
#' @param dir Directory path
#' @param folder `TRUE` if looking for folder names
#' @return Object name of the most recent file
#' @description look into the folder and find the version of the file
#' with the most recent name
#' @export
util_check_the_latest_file <-
  function(file_name,
           dir,
           folder = FALSE) {
    RUtilpol::check_class("file_name", c("character", "logical"))

    RUtilpol::check_class("dir", "character")

    RUtilpol::check_class("folder", "logical")

    file_full_list <- list.files(dir)

    if (
      folder == FALSE
    ) {
      file_list_selected_files <-
        subset(
          file_full_list,
          stringr::str_detect(file_full_list, file_name)
        )

      if (
        length(file_list_selected_files) == 0
      ) {
        newest_file <- NA
      } else {
        file_list_selected_files <-
          stringr::str_sort(file_list_selected_files, decreasing = TRUE)

        newest_file <- file_list_selected_files[1]
      }
    } else {
      file_list_selected_files <-
        stringr::str_sort(file_full_list, decreasing = TRUE)

      newest_file <- file_list_selected_files[1]
    }

    return(newest_file)
  }
