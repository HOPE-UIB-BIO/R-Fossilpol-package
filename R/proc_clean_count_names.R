#' @title Clean names for count datasets
#' @param data_source Data.frame containing `raw_counts` column which contains
#' raw counts
#' @param additional_patterns User defined patterns to be used for name changes
#' @export
proc_clean_count_names <-
  function(data_source, additional_patterns = NULL) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "raw_counts")

    RUtilpol::check_class("additional_patterns", c("NULL", "character"))

    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    RUtilpol::output_heading(
      msg = "Cleaning taxon names"
    )

    data_clean_names <-
      data_full_filtered %>%
      dplyr::mutate(
        raw_counts = purrr::map(
          .x = raw_counts,
          .f = ~ proc_clean_column_names(
            data_source = .x,
            additional_patterns = additional_patterns
          )
        )
      )

    RUtilpol::check_if_loaded(
      file_name = "data_clean_names",
      env = current_env
    )

    RUtilpol::check_class("data_clean_names", "data.frame")

    RUtilpol::check_col_names("data_clean_names", "raw_counts")

    return(data_clean_names)
  }
