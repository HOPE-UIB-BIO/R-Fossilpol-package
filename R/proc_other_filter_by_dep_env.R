#' @title Filter Other sequences by the selected depositional environments
#' @param data_source Data.frame with sequences and their depositional environments
#' @param data_storage_path Path to the data storage folder
#' @description The function will obtain the all depositional environments from
#' Neotoma. Next, it will filter the sequences based on the selected
#' depositional environments.
#' @keywords internal
proc_other_filter_by_dep_env <- function(data_source, data_storage_path) {
  RUtilpol::check_class("data_source", "data.frame")

  RUtilpol::check_col_names("data_source", "depositionalenvironment")

  RUtilpol::check_class("data_storage_path", "character")

  RUtilpol::output_heading(
    msg = "Starting preparation of depositional environment selection"
  )

  # select all depositional environments presented in the data
  dep_env_sel_data <-
    data_source %>%
    dplyr::distinct(depositionalenvironment) %>%
    dplyr::arrange(depositionalenvironment)

  data_dep_env_filtered <-
    proc_filter_by_dep_env(
      data_source,
      dep_env_sel_data,
      data_storage_path,
      dir_spec = "Other"
    )

  return(data_dep_env_filtered)
}
