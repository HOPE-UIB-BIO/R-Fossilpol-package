#' @title Recalibrate age-depth models for individual records
#' @param data_source_chron
#' Data.frame containing `dataset_id` and `chron_control_format`
#' @param n_iterations Numeric. The number of iterations used by Bchron
#' @param n_burn
#' Numeric. The number of starting iterations to discard used by Bchron
#' @param n_thin
#' Numeric. The step size for every iteration to keep beyond
#' the burnin used by Bchron
#' @param time_to_stop
#' Numeric. Time to wait until saving the record's `dataset_id` into
#' `Crash_file`as unsuccessfully, due to freeze of calculation
#' @param dir Character. Path to the data storage folder
chron_recalibrate_individual <- function(data_source_chron,
                                         n_iterations = 10e3,
                                         n_burn = 2e3,
                                         n_thin = 8,
                                         time_to_stop = 600,
                                         dir) {

  # individually compute the age-depth model for each site,
  #   which did not manage to re-calibrate AD model.
  # For each file, temporarily add its dataset_id into `Crash_file` until it is
  #   successfully re-calibrated

  RUtilpol::check_class("data_source_chron", "data.frame")

  RUtilpol::check_col_names(
    "data_source_chron",
    c(
      "dataset_id",
      "chron_control_format"
    )
  )

  RUtilpol::check_class("n_iterations", "numeric")

  RUtilpol::check_class("n_burn", "numeric")

  RUtilpol::check_class("n_thin", "numeric")

  RUtilpol::check_class("dir", "character")

  dir <- RUtilpol::add_slash_to_path(dir)

  crash_file <- util_load_chron_crashfile(dir)

  # set path to Crash file
  crash_file_path <-
    paste0(dir, "/Data/Input/Chronology_setting/Bchron_crash/")

  # subset
  data_to_run <-
    data_source_chron %>%
    dplyr::filter(!dataset_id %in% crash_file$dataset_id) %>%
    dplyr::mutate(
      row_n = dplyr::row_number()
    )

  n_ds <- nrow(data_to_run)

  RUtilpol::output_comment(
    msg = paste(
      "Estimation will be done for", n_ds, "records"
    )
  )

  # compute for each Site in data_to_run
  purrr::pwalk(
    .l = list(
      data_to_run$row_n, # ..1
      data_to_run$dataset_id, # ..2
      data_to_run$chron_control_format # ..3
    ),
    .f = ~ {
      current_frame <- sys.nframe()
      current_env <- sys.frame(which = current_frame)

      RUtilpol::output_comment(
        msg = paste0(
          "dataset: ", ..2, ". Number ", ..1, " out of ",
          n_ds
        )
      )

      # update the list
      crash_dataset_list <-
        c(crash_file$dataset_id, ..2) %>%
        unique()

      # create new crash file including the current file
      crash_file <-
        tibble::tibble(
          dataset_id = crash_dataset_list
        )

      # save crash file
      readr::write_csv(
        crash_file,
        paste0(crash_file_path, "Crash_file.csv")
      )

      try(
        expr = {
          # try to run Bchron within `time_to_stop`  time window
          RUtilpol::do_in_time(
            expr = {
              result <-
                chron_run_bchron(
                  data_source = ..3,
                  n_iterations = n_iterations,
                  n_burn = n_burn,
                  n_thin = n_thin
                )
            },
            timeout = time_to_stop,
            envir = current_env
          )
        }
      )

      # if chron fail, insert NA value
      if (
        !exists("result", envir = current_env)
      ) {
        return()
      }

      # remove the site from crash list
      crash_file <-
        crash_file %>%
        dplyr::filter(dataset_id != ..2)

      # save crash file
      readr::write_csv(
        crash_file,
        paste0(crash_file_path, "Crash_file.csv")
      )

      RUtilpol::save_latest_file(
        object_to_save = list(
          chron_control_table = data_source_chron %>%
            dplyr::filter(dataset_id == ..2),
          ad_model = result
        ),
        file_name = ..2,
        dir = paste0(dir, "Data/Processed/Chronology/Models_full/"),
        prefered_format = "rds",
        use_sha = TRUE,
        verbose = FALSE
      )

      RUtilpol::output_comment(
        msg = paste0("dataset: ", ..2, ". Number ", ..1, " - Done")
      )
    }
  )
  return()
}
