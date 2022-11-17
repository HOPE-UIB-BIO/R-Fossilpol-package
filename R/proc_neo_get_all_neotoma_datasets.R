#' @title Download the list of all Neotoma datasets
#' @description Download all datasets from Neotoma and subset only those for
#' selected `dataset_type` and within selected geographical limits
#' @param dataset_type Type of dataset to use
#' @param long_min Limit for the smallest longitude
#' @param long_max Limit for the largest longitude
#' @param lat_min Limit for the smallest latitude
#' @param lat_max Limit for the largest latitude
#' @param loc Argument passed to `neotoma2::get_sites`. See details.
#' @details
#' The function is calling `neotoma2::get_sites` function to extract the
#' datasets_id. This allow aditional possibilities of selection (e.g. polygon).
#' @seealso [neotoma2::get_sites()]
#' @export
proc_neo_get_all_neotoma_datasets <-
  function(dataset_type,
           long_min,
           long_max,
           lat_min,
           lat_max,
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

    # use neotoma2 to get the selected sites
    sel_sites <-
      neotoma2::get_sites(loc = geo_box)

    # Transform into a data.frame
    sel_sites_df <-
      as.data.frame(sel_sites) %>%
      dplyr::rename(
        altitude = elev
      )

    # Filter all sequences by the geographical limits
    sel_sites_filtered <-
      proc_filter_by_geography(
        sel_sites_df,
        long_min,
        long_max,
        lat_min,
        lat_max
      )

    RUtilpol::check_if_loaded(
      file_name = "sel_sites_filtered",
      env = current_env
    )

    RUtilpol::check_class("sel_sites_filtered", "data.frame")

    RUtilpol::output_comment("List of Neotoma sites was successfully obtained.")

    sel_datasets <-
      neotoma2::get_datasets(
        sel_sites, # use the ofirginal list od sites
        all_data = TRUE,
        verbose = FALSE
      ) %>%
      # filter to only include those seletced by altitude
      neotoma2::filter(siteid %in% sel_sites_filtered$siteid) %>%
      # filter to only include 
      neotoma2::filter(datasettype %in% dataset_type)  %>% 
      # turn into datasets
      neotoma2::datasets() %>%
      as.data.frame()  %>% 
      # rename the column for back compatibility
      dplyr::rename(
        dsid = datasetid
      )

    RUtilpol::check_if_loaded(
      file_name = "sel_datasets",
      env = current_env
    )

    RUtilpol::check_class("sel_datasets", "data.frame")

    RUtilpol::output_comment("List of Neotoma sequences was successfully obtained.")

    util_check_data_table(sel_datasets)

    return(sel_datasets)
  }
