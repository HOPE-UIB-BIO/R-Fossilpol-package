#' @title Filter Neotoma sequences by the selected depositional environments
#' @param data_source Data.frame with sequences and their depositional environments
#' @param data_storage_path Path to the data storage folder
#' @description The function will obtain all the depositional environments from
#' Neotoma. Next, it will filter the sequences based on the selected
#' depositional environments.
#' @export
proc_neo_filter_by_dep_env <-
  function(data_source, data_storage_path) {
    util_check_class("data_source", "data.frame")

    util_check_col_names("data_source", "depositionalenvironment")

    util_check_class("data_storage_path", "character")

    util_output_message(
      msg = "Starting preparation of depositional environment selection"
    )

    # Get depositional environments from Neotoma
    neotoma_dep_envt_types <-
      proc_neo_get_dep_env()

    # Get list of all depositional environments for all selected sequences
    #   and add all the hierarchical information
    dep_env_sel_data <-
      data_source %>%
      dplyr::distinct(depositionalenvironment) %>%
      dplyr::arrange(depositionalenvironment) %>%
      dplyr::left_join(
        neotoma_dep_envt_types,
        by = c("depositionalenvironment" = "dep_env")
      ) %>%
      dplyr::select(!dplyr::contains("_id")) # do not include any of *_id columns

    data_dep_env_filtered <-
      proc_filter_by_dep_env(
        data_source,
        dep_env_sel_data,
        data_storage_path,
        dir_spec = "Neotoma"
      )

    return(data_dep_env_filtered)
  }
