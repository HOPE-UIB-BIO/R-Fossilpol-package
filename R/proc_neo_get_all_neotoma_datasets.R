#' @title Download the list of all Neotoma datasets
#' @description Download all datasets from Neotoma and subset only those for
#' selected `dataset_type` and within selected geographical limits
#' @param dataset_type Type of dataset to use
#' @param long_min Limit for the smallest longitude
#' @param long_max Limit for the largest longitude
#' @param lat_min Limit for the smallest latitude
#' @param lat_max Limit for the largest latitude
#' @export
proc_neo_get_all_neotoma_datasets <-
  function(dataset_type,
           long_min,
           long_max,
           lat_min,
           lat_max) {
    util_check_class("dataset_type", "character")

    util_check_class("long_min", "numeric")

    util_check_class("long_max", "numeric")

    util_check_class("lat_min", "numeric")

    util_check_class("lat_max", "numeric")

    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    # api paths
    rawdatasets <- "https://api.neotomadb.org/v2.0/data/datasets/"

    # request all data of selected type from Neotoma 2.0
    pollends <-
      httr::GET(
        rawdatasets,
        query = list(
          datasettype = dataset_type, # [config_criteria]
          limit = 99999,
          offset = 0
        )
      )

    # Extract all data
    datasets <- httr::content(pollends)$data

    # Create a table with dataset_id, and coordinates
    allds <-
      proc_neo_get_coord(datasets)

    # Filter all sequences by the geographical limits
    allds_filtered <-
      proc_filter_by_geography(
        allds,
        long_min,
        long_max,
        lat_min,
        lat_max
      )

    util_check_if_loaded(
      file_name = "allds_filtered",
      env = current_env
    )

    util_check_class("allds_filtered", "data.frame")

    util_output_comment("List of Neotoma sequences was successfully obtained.")

    util_check_data_table(allds_filtered)

    return(allds_filtered)
  }
