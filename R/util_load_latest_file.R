#' @title Detect the newest version of the selected file and load it
#' @param file_name Name of the object to save in quotes
#' @param dir Directory path
#' @return Object of latest version of the `file_name`
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
#' Look into the folder `dir` and find the version of the file with
#'  the most recent date and load it
util_load_latest_file <-
  function(file_name,
           dir) {
    lifecycle::deprecate_warn(
      "0.0.2", "util_load_latest_file()", "RUtilpol::get_latest_file()"
    )
    RUtilpol::check_class("file_name", "character")

    RUtilpol::check_class("dir", "character")

    file_last_name <-
      RUtilpol::get_latest_file_name(
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
