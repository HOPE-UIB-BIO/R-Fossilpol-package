#' @title Merge data from Neotoma and Other source
#' @param data_storage_path Path to the data storage folder
#' @param other_data Logical value if to include other source of data
#' @export
proc_get_merged_dataset <- function(data_storage_path, other_data = FALSE) {
  RUtilpol::check_class("data_storage_path", "character")

  RUtilpol::check_class("other_data", "logical")

  current_frame <- sys.nframe()
  current_env <- sys.frame(which = current_frame)

  RUtilpol::output_heading(
    msg = "Loading datasets"
  )

  # Load processed Neotoma sequences
  neotoma_processed <-
    RUtilpol::get_latest_file(
      file_name = "neotoma_processed",
      dir = paste0(data_storage_path, "/Data/Processed/Neotoma_processed")
    )


  RUtilpol::check_if_loaded(
    file_name = "neotoma_processed",
    env = current_env
  )

  RUtilpol::check_class("neotoma_processed", "data.frame")

  if (
    other_data == TRUE
  ) {

    # load processed other sequences
    other_processed <-
      RUtilpol::get_latest_file(
        file_name = "other_data_prepared",
        dir = paste0(data_storage_path, "/Data/Processed/Other")
      )

    RUtilpol::check_if_loaded(
      file_name = "other_processed",
      env = current_env
    )

    RUtilpol::check_class("other_processed", "data.frame")

    # make sure that dataset_id are not duplicated
    neotoma_filtered <-
      neotoma_processed %>%
      dplyr::filter(!dataset_id %in% other_processed$dataset_id)


    # merge file together
    data_full <-
      dplyr::bind_rows(
        neotoma_filtered,
        other_processed
      ) %>%
      dplyr::mutate(
        # mark everything as public, if it is not other
        data_publicity = ifelse(data_publicity != "other",
          "public", "other"
        ),
        # detect if data is in pollen percentages
        pollen_percentage = ifelse(is.na(pollen_percentage),
          FALSE,
          pollen_percentage
        ),
        # mark everything as other if not stated otherwise
        source_of_data = ifelse(is.na(source_of_data),
          "other",
          source_of_data
        )
      )

    RUtilpol::check_if_loaded(
      file_name = "data_full",
      env = current_env
    )

    RUtilpol::check_class("data_full", "data.frame")

    RUtilpol::check_col_names(
      "data_full",
      c("data_publicity", "pollen_percentage", "source_of_data")
    )

    RUtilpol::output_comment("Neotoma data was loanded and merged with Other")
  } else {
    data_full <- neotoma_processed

    RUtilpol::check_if_loaded(
      file_name = "data_full",
      env = current_env
    )

    RUtilpol::check_class("data_full", "data.frame")

    RUtilpol::output_comment("Neotoma data was loaded")
  }

  util_check_data_table(data_full)

  return(data_full)
}
