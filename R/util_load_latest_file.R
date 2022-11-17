#' @title Detect the newest version of the selected file and load it
#' @param file_name Name of the object to save in quotes
#' @param dir Directory path
#' @return Object of latest version of the `file_name`
#' @description Look into the folder `dir` and find the version of the file with
#'  the most recent date and load it
#' @export
util_load_latest_file <-
  function(file_name,
           dir) {
    RUtilpol::check_class("file_name", "character")

    RUtilpol::check_class("dir", "character")

    file_last_name <-
      util_check_the_latest_file(
        file_name = file_name,
        dir = dir
      )

    data_object <-
      readr::read_rds(
        paste0(dir, "/", file_last_name)
      )

    if (
      any(names(data_object) == "data")
    ) {
      usethis::ui_done(
        paste(
          "Automatically loaded file", file_name,
          "with date", data_object$setting$date
        )
      )

      data_object <- data_object$data
    } else {
      usethis::ui_done(
        paste("Automatically loaded file", file_name)
      )
    }

    return(data_object)
  }
