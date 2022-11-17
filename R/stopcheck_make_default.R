#' @title Create file with list of all present data
#' @param data_source Data.frame with the data
#' @param dir Path to save file
#' @return Data.frame including all present data and `include` == FALSE
#' @keywords internal
stopcheck_make_default <-
  function(data_source,
           dir) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_class("dir", "character")

    data_selection <-
      data_source %>%
      dplyr::mutate(include = FALSE)

    RUtilpol::check_col_names("data_selection", "include")

    return(data_selection)
  }
