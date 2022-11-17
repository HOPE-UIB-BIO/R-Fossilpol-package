#' @title Create file with list of potential duplicates
#' @param data_source Data.frame with the potential duplicates
#' @param dir Path to save file
#' @return Data.frame including all potential duplicates
stopcheck_make_potent_dupl <-
  function(data_source,
           dir) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_class("dir", "character")

    potential_duplicates <-
      data_source %>%
      dplyr::mutate(
        pair_n = dplyr::row_number(),
        delete = 0
      )

    RUtilpol::check_col_names(
      "potential_duplicates",
      c("pair_n", "delete")
    )

    return(potential_duplicates)
  }
