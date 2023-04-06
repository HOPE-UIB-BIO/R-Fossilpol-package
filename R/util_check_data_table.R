#' @title Check the result of filtration process on table
#' @description Function will check the number of rows of the table and return
#' warning message `msg` if there are no records
#' @param data_source Data.frame of records
#' @param msg Message to be appended to the end of warning
#' @keywords internal
util_check_data_table <-
  function(data_source, msg = "") {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::stop_if_not(
      nrow(data_source) > 0,
      false_msg = paste(
        "There is 0 records based on the selected Criteria.",
        "Please change the criteria.", "\n",
        msg
      ),
      true_msg = paste(
        "Number of records prepared:",
        nrow(data_source)
      )
    )
  }
