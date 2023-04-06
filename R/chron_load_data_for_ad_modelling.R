#' @title Load chronology control tables and filter our those present in Crash
#' File
#' @param dir Path to the data storage folder
#' @export
chron_load_data_for_ad_modelling <-
  function(dir) {
    RUtilpol::check_class("dir", "character")

    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    chron_tables_prepared <-
      RUtilpol::get_latest_file(
        file_name = "chron_tables_prepared",
        dir = paste0(
          dir, "/Data/Processed/Chronology/Chron_tables_prepared"
        )
      )

    RUtilpol::check_if_loaded(
      file_name = "chron_tables_prepared",
      env = current_env
    )

    RUtilpol::check_class("chron_tables_prepared", "data.frame")

    crash_file <-
      util_load_chron_crashfile(dir)

    if (
      nrow(crash_file) > 0
    ) {
      RUtilpol::output_comment(
        paste(
          "Filtering out records from 'Crash File'", "\n",
          "The following sites are curently filtered out:", "\n",
          RUtilpol::paste_as_vector(crash_file$dataset_id)
        )
      )

      # subset the data filtering out dataset_id, which causes crash
      datasets_to_run <-
        chron_tables_prepared %>%
        dplyr::filter(!dataset_id %in% crash_file$dataset_id)
    } else {
      datasets_to_run <- chron_tables_prepared
    }

    util_check_data_table(
      datasets_to_run,
      "There are no dataset to be recalibrated"
    )

    return(datasets_to_run)
  }
