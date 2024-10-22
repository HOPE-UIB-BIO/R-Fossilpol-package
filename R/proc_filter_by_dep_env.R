#' @title Filter data by the depositional environments
#'
#' @param data_source Data.frame with records to be filtered
#' @param selection_data Data.frame with depositional environments presented in the data
#' @param data_storage_path Path to the data storage folder
#' @param dir_spec Specification of the folder name
#' @description Use "stop-check" (see `stopcheck_table` function) to load/create
#'  the table defined by the user. Next, it will filter the records in
#'  `data_source` based on the selected depositional environments.
#' @keywords internal
proc_filter_by_dep_env <-
  function(data_source, selection_data, data_storage_path, dir_spec) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "depositionalenvironment")

    RUtilpol::check_class("selection_data", "data.frame")

    RUtilpol::check_col_names("selection_data", "depositionalenvironment")

    RUtilpol::check_class("data_storage_path", "character")

    RUtilpol::check_class("dir_spec", "character")

    folder_dir <-
      paste0(
        data_storage_path, "/Data/Input/Depositional_environment/", dir_spec, "/"
      )

    # load/create table with depositional environments
    depositional_environment_selection <-
      stopcheck_table(
        data_source = selection_data,
        file_name = "depositional_environment_selection",
        dir = folder_dir,
        sel_method = "default",
        msg = "Select which depositional environment should be kept ('include' = TRUE/FALSE)."
      )

    # create a list of valid dep.env
    dep_envt_types_transform_filter <-
      depositional_environment_selection %>%
      dplyr::filter(include == TRUE) %>%
      purrr::pluck("depositionalenvironment")


    RUtilpol::open_dir_if_not(
      length(dep_envt_types_transform_filter) > 0,
      dir = folder_dir,
      msg = paste(
        "There is no depositional environment included.",
        "Please review the latest file in:",
        folder_dir
      )
    )

    RUtilpol::output_comment(
      msg = paste(
        "There has been", length(dep_envt_types_transform_filter),
        "selected depositional environments:",
        RUtilpol::paste_as_vector(dep_envt_types_transform_filter)
      )
    )

    # Filter sites based on relevant environmental types
    data_filtered_by_dep_env <-
      data_source %>%
      dplyr::filter(
        depositionalenvironment %in% dep_envt_types_transform_filter
      )

    util_check_data_table(data_filtered_by_dep_env)

    return(data_filtered_by_dep_env)
  }
