#' @title Obtain the table of depositional environments from Neotoma
#' @keywords internal
proc_neo_get_dep_env <-
  function() {
    # Get relevant tables with information from Neotoma API
    dep_envt_types_dwn <-
      httr::GET("https://api.neotomadb.org/v2.0/dbtables/table?table=depenvttypes",
        query = list(
          limit = 99999,
          offset = 0
        )
      )

    RUtilpol::stop_if_not(
      dep_envt_types_dwn$status_code == 200, # status 200 = success
      false_msg = "List of all depositional environments was NOT downloaded from Neotoma",
      true_msg = "List of all depositional environments was downloaded from Neotoma"
    )

    # extract the content of the download
    dep_envt_types_dwn <- httr::content(dep_envt_types_dwn)$data

    # extract all types of dep.env. into table
    dep_envt_types <-
      tibble::tibble(
        depenvtid = dep_envt_types_dwn$data %>%
          purrr::map_int("depenvtid"),
        depenvt = dep_envt_types_dwn$data %>%
          purrr::map_chr("depenvt"),
        depenvthigherid = dep_envt_types_dwn$data %>%
          purrr::map_int("depenvthigherid")
      ) %>%
      dplyr::rename(
        dep_env_id = depenvtid,
        dep_env = depenvt,
        higher_id = depenvthigherid
      )

    # Prepare DepEnvtTypes - rebuild table to filter information
    # This is needed as dep.env. is constructed in hierarchical structure
    dep_envt_types_transform <-
      dep_envt_types %>%
      dplyr::left_join(
        dep_envt_types,
        by = c("higher_id" = "dep_env_id"),
        suffix = c("", "_level_1")
      ) %>%
      dplyr::left_join(
        dep_envt_types,
        by = c("higher_id_level_1" = "dep_env_id"),
        suffix = c("", "_level_2")
      ) %>%
      dplyr::left_join(
        dep_envt_types,
        by = c("higher_id_level_2" = "dep_env_id"),
        suffix = c("", "_level_3")
      ) %>%
      dplyr::left_join(
        dep_envt_types,
        by = c("higher_id_level_3" = "dep_env_id"),
        suffix = c("", "_level_4")
      )

    return(dep_envt_types_transform)
  }
