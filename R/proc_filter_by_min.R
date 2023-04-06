#' @title Filter records by minimal value
#' @param data_source Data.frame to be filtered
#' @param var_name Name of the variable to be compared to
#' @param min_n Criterion to be evaluated against
#' @keywords internal
proc_filter_by_min <-
  function(data_source, var_name, min_n) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_class("var_name", "character")

    RUtilpol::check_class("min_n", "numeric")

    RUtilpol::check_col_names("data_source", eval(var_name))

    data_source_filtered <-
      data_source %>%
      dplyr::filter(
        get(var_name) >= max(1, min_n) # use minimal of 1
      )

    util_check_data_table(data_source_filtered)

    return(data_source_filtered)
  }
