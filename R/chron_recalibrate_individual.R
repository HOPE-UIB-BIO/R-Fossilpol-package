#' @title Recalibrate age-depth models for individual sequences
#' @param data_source_chron  Data.frame containing `dataset_id` and `chron_control_format`
#' @param n_iterations Numeric. The number of iterations used by Bchron
#' @param n_burn 
#' Numeric. The number of starting iterations to discard used by Bchron
#' @param n_thin 
#' Numeric. The step size for every iteration to keep beyond
#' the burnin used by Bchron
#' @param time_to_stop 
#' Numeric. Time to wait until saving the sequence's `dataset_id` into
#' `Crash_file`as unsuccessfully, due to freeze of calculation
#' @param dir Character. Path to the data storage folder
chron_recalibrate_individual <-
  function(data_source_chron,
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

    crash_file <- util_load_chron_crashfile(dir)

    # set path to Crash file
    crash_file_path <-
      paste0(dir, "/Data/Input/Chronology_setting/Bchron_crash/")

    # subset
    broken_sites <-
      data_source_chron %>%
      dplyr::filter(!dataset_id %in% crash_file$dataset_id) 

    broken_sites_number <- nrow(broken_sites)

    RUtilpol::output_comment(
      msg = paste(
        "Additional calculation will be done for", broken_sites_number,
        "sequences"
      )
    )

    # compute for each Site in broken_sites
    broken_sites_done <-
      broken_sites %>%
      dplyr::mutate(
        row_n = dplyr::row_number(),
        bchron_mod = purrr::pmap(
          .l = list(row_n, dataset_id, chron_control_format),
          .f = ~ {
            current_frame <- sys.nframe()
            current_env <- sys.frame(which = current_frame)

            RUtilpol::output_comment(
              msg = paste0(
                "dataset: ", ..2, ". Number ", ..1, " out of ",
                broken_sites_number
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

            try(expr = {
              # try to run Bchron within `time_to_stop`  time window
              R.utils::withTimeout(
                {
                  result <-
                    chron_run_bchron(
                      data_source = ..3,
                      n_iterations = n_iterations,
                      n_burn = n_burn,
                      n_thin = n_thin
                    )
                },
                timeout = time_to_stop,
                onTimeout = "silent"
              )
            })

            # if chron fail, insert NA value
            if (
              !exists("result", envir = current_env)
            ) {
              result <- NA
            }

            # save result and delete the object
            broken_sites_res <- result
            rm(result, envir = current_env)

            # remove the site from crash list
            crash_file <-
              crash_file %>%
              dplyr::filter(dataset_id != ..2)

            # save crash file
            readr::write_csv(
              crash_file,
              paste0(crash_file_path, "Crash_file.csv")
            )

            RUtilpol::output_comment(
              msg = paste0("dataset: ", ..2, ". Number ", ..1, " - Done")
            )

            return(broken_sites_res)
          }
        )
      ) %>%
      dplyr::select(-row_n)

    RUtilpol::check_col_names("broken_sites_done", "bchron_mod")

    return(broken_sites_done)
  }
