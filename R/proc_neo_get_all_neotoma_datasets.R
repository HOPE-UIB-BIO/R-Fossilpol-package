#' @title Download the list of all Neotoma datasets
#' @description Download all datasets from Neotoma and subset only those for
#' selected `dataset_type` and within selected geographical limits
#' @param dataset_type Character. Type of dataset to use
#' @param long_min Numeric. Limit for the smallest longitude
#' @param long_max Numeric. Limit for the largest longitude
#' @param lat_min Numeric. Limit for the smallest latitude
#' @param lat_max Numeric. Limit for the largest latitude
#' @param use_api_directly Logical. Use directly Neotoma API.
#' If false, the function will use {neotoma2} package (currently slower).
#' @param loc Optional. Used only if `use_api_directly` is set to `FALSE`.
#' Argument is passed to `neotoma2::get_sites`. See details.
#' @details
#' The function is extracting the datasets_id of all datasets with the
#' `dataset_type`. If `use_api_directly` set to `FALSE`, the function will call
#'  `neotoma2::get_sites` function, which allows aditional possibilities
#' of selection (e.g. polygon). See {neotoma2} documentation
#' @seealso [neotoma2::get_sites()]
#' @export
proc_neo_get_all_neotoma_datasets <- function(dataset_type,
                                              long_min,
                                              long_max,
                                              lat_min,
                                              lat_max,
                                              use_api_directly = TRUE,
                                              loc) {
  current_frame <- sys.nframe()
  current_env <- sys.frame(which = current_frame)

  RUtilpol::check_class("dataset_type", "character")

  # if `loc` is not specified by the user, use the traditional geo box
  if (
    is.null(loc) == TRUE
  ) {
    RUtilpol::check_class("long_min", "numeric")

    RUtilpol::check_class("long_max", "numeric")

    RUtilpol::check_class("lat_min", "numeric")

    RUtilpol::check_class("lat_max", "numeric")

    loc <-
      c(
        long_min,
        lat_min,
        long_max,
        lat_max
      )
  }

  if (
    isTRUE(use_api_directly)
  ) {
    # api paths
    api_path_datasets <- "https://api.neotomadb.org/v2.0/data/datasets/"

    # request all data of selected type from Neotoma 2.0
    http_results <-
      httr::GET(
        api_path_datasets,
        query = list(
          datasettype = dataset_type, # [config_criteria]
          limit = 99999,
          offset = 0
        )
      )

    # Extract all data
    all_datasets <- httr::content(http_results)$data

    # Create a table with dataset_id, and coordinates
    sel_sites_df <-
      proc_neo_get_coord(all_datasets)
  } else {
    # use neotoma2 to get the selected sites
    sel_sites <-
      neotoma2::get_sites(
        loc = loc,
        limit = 99999999
      )

    # Transform into a data.frame
    sel_sites_df <-
      neotoma2::as.data.frame(sel_sites) %>%
      dplyr::rename(
        altitude = elev
      )
  }

  RUtilpol::output_comment("List of Neotoma sites was successfully obtained.")

  # Filter all records by the geographical limits
  sel_sites_filtered <-
    proc_filter_by_geography(
      sel_sites_df,
      long_min,
      long_max,
      lat_min,
      lat_max
    )

  RUtilpol::check_class("sel_sites_filtered", "data.frame")

  if (
    isFALSE(use_api_directly)
  ) {
    sel_datasets <-
      neotoma2::get_datasets(
        sel_sites, # use the ofirginal list od sites
        all_data = TRUE,
        verbose = FALSE
      ) %>%
      # filter to only include those seletced by altitude
      neotoma2::filter(siteid %in% sel_sites_filtered$siteid) %>%
      # filter to only include
      neotoma2::filter(datasettype %in% dataset_type) %>%
      # turn into datasets
      neotoma2::datasets() %>%
      neotoma2::as.data.frame() %>%
      # rename the column for back compatibility
      dplyr::rename(
        dsid = datasetid
      )
  } else {
    sel_datasets <-
      sel_sites_filtered
  }

  RUtilpol::check_if_loaded(
    file_name = "sel_datasets",
    env = current_env
  )

  RUtilpol::check_class("sel_datasets", "data.frame")

  RUtilpol::output_comment(
    "List of Neotoma records was successfully obtained."
  )

  util_check_data_table(sel_datasets)

  return(sel_datasets)
}
