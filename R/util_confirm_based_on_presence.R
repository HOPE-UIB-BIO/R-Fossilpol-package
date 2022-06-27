#' @title Confirm choice based on the presence of previous data
#' @param dir Path to the folder, where data should be stored
#' @param file_name Name of the file to look upon
#' @param msg Message to be placed with the confirmation question
#' @export
util_confirm_based_on_presence <-
  function(dir, file_name, msg = "") {
    util_check_class("dir", "character")

    util_check_class("file_name", "character")

    util_check_class("msg", "character")

    # pre-set to TRUE
    confirm <- TRUE

    # Check if there is any previous file
    previous_file <-
      list.files(
        path = dir
      ) %>%
      stringr::str_detect(., file_name)

    # If there any file
    if (
      any(previous_file)
    ) {
      confirm <-
        util_confirm(
          msg = msg
        )
    }

    return(confirm)
  }
