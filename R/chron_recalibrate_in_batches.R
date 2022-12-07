#' @title Recalibrate the selected sequences in defined batches using parallel
#' computation
#' @param data_source_chron
#' Data.frame containing `dataset_id` and `chron_control_format`
#' @param batch_size
#' Numeric. Number of individual sequences re-calibrate in a single
#' batch using parallel computation
#' @param dir Character. Path to the data storage folder
#' @param n_iterations Numeric. The number of iterations used by Bchron
#' @param n_burn
#' Numeric. The number of starting iterations to discard used by Bchron
#' @param n_thin
#' Numeric. The step size for every iteration to keep beyond
#' the burnin used by Bchron
#' @param number_of_cores
#' Numeric. Number of CPU cores to use in parallel computation
#' @param set_seed Numeric. User-defined seed for randomisations
#' @param maximum_number_of_loops
#' Numeric. Number of tries each batch should be considered
#' before skipping it
#' @param time_per_sequence Time (in sec) dedicated for each sequence to estimate
#' age-depth model. If it takes computer longer that selected value, estimation
#' is considered as unsuccessful and skipped. The time value is multiplied by
#' `iteration_multiplier` as more iteration required more time. Time for whole
#' batch is calculated as `time_per_sequence` multiplied by 
#' `iteration_multiplier` multiplied by the number of sequences per batch 
#' (which is estimated based on `number_of_cores`)
#' @return Vector with names of sequences which failed to estimate
chron_recalibrate_in_batches <- function(data_source_chron,
                                         batch_size = 1,
                                         time_per_sequence = 120,
                                         dir,
                                         n_iterations = 10e3,
                                         n_burn = 2e3,
                                         n_thin = 8,
                                         number_of_cores = 1,
                                         set_seed = 1234,
                                         maximum_number_of_loops = 3) {
  RUtilpol::check_class("data_source_chron", "data.frame")

  RUtilpol::check_col_names("data_source_chron", "chron_control_format")

  RUtilpol::check_class("batch_size", "numeric")

  RUtilpol::check_class("time_per_sequence", "numeric")

  RUtilpol::check_class("dir", "character")

  RUtilpol::check_class("n_iterations", "numeric")

  RUtilpol::check_class("n_burn", "numeric")

  RUtilpol::check_class("n_thin", "numeric")

  RUtilpol::check_class("number_of_cores", "numeric")

  RUtilpol::check_class("set_seed", "numeric")

  RUtilpol::check_class("maximum_number_of_loops", "numeric")

  current_frame <- sys.nframe()
  current_env <- sys.frame(which = current_frame)

  dir <- RUtilpol::add_slash_to_path(dir)

  # detect the number of batches needed to split the data into base on the
  #   `batch_size` selected by user
  number_of_batches <-
    ceiling(nrow(data_source_chron) / batch_size)

  # a dummy data.freme to keep track of progress and
  #   number of tries for each batch
  batch_success_table <-
    tibble::tibble(
      batch_number = 1:number_of_batches,
      batch_name = paste0("batch_", formatC(batch_number, width = 3, flag = 0)),
      # list of sequences in each batch
      sequence_list = purrr::map(
        .x = batch_number,
        .f = ~ data_source_chron %>%
          dplyr::slice(
            seq(
              from = batch_size * (.x - 1) + 1,
              to = min(c(batch_size * (.x), nrow(data_to_run))),
              by = 1
            )
          ) %>%
          purrr::pluck("dataset_id")
      ),
      # number of sequences in each batch
      batch_size = purrr::map_dbl(
        .x = sequence_list,
        .f = length
      ),
      # set a value for the process to wait (in seconds) for each batch
      time_to_stop = (batch_size * time_per_sequence) *
        max(
          c(
            (floor(iteration_multiplier / 2)),
            1
          )
        ),
      done = FALSE,
      .rows = number_of_batches
    )

  RUtilpol::check_if_loaded(
    file_name = "batch_success_table",
    env = current_env
  )

  RUtilpol::check_col_names(
    "batch_success_table",
    c(
      "batch_number",
      "batch_name",
      "sequence_list",
      "batch_size",
      "time_to_stop",
      "done"
    )
  )

  # create a loop counter, which starts at 1
  loop_counter <- 1

  # Chronology computation  -----

  # start the loop
  repeat{
    RUtilpol::output_comment(
      paste("Attempt number", loop_counter, "for all batches")
    )

    # For all batches that are not done
    for (i in seq_along(data_source_batch$batch_name)) {
      current_batch_n <- data_source_batch$batch_number[i]

      if (
        isTRUE(data_source_batch$done[i])
      ) {
        next
      }

      RUtilpol::output_comment(
        msg = paste("batch", current_batch_n, "out of", number_of_batches)
      )

      # subset data in the selected batch
      temp_data <-
        data_source_chron %>%
        dplyr::filter(dataset_id %in% data_source_batch$sequence_list[[i]])

      # setup plan for parallel computation
      future::plan(
        strategy = future::multisession(),
        workers = number_of_cores
      )

      # try to run Chronology
      try(
        expr = {
          # compute within time period. If longer than `time_to_stop`,
          #   stop computation
          R.utils::withTimeout(
            expr = {
              bchron_temp_run <-
                furrr::future_map(
                  .x = temp_data$chron_control_format,
                  .f = ~ chron_run_bchron(
                    data_source = .x,
                    n_iterations = n_iterations,
                    n_burn = n_burn,
                    n_thin = n_thin
                  ),
                  .options = furrr::furrr_options(seed = set_seed)
                ) %>%
                rlang::set_names(
                  nm = temp_data$dataset_id
                )

              # assign the test run temporary result
              assign("bchron_temp", bchron_temp_run, envir = current_env)

              # delete test run
              rm(bchron_temp_run, envir = current_env)
            },
            timeout = data_source_batch$time_to_stop[[i]],
            onTimeout = "silent"
          )
        },
        silent = TRUE
      )

      RUtilpol::output_comment(
        msg = paste("batch", current_batch_n, "finished")
      )

      # if temporary result is produced
      if (
        exists("bchron_temp", envir = current_env) == TRUE
      ) {

        # save the batch as individual sequences
        purrr::walk2(
          .x = bchron_temp,
          .y = names(bchron_temp),
          .f = ~ RUtilpol::save_latest_file(
            object_to_save = .x,
            file_name = .y,
            dir = paste0(dir, "Data/Processed/Chronology/Models_full/"),
            prefered_format = "rds",
            use_sha = TRUE,
            verbose = FALSE
          )
        )

        # mark status of batch
        data_source_batch$done[i] <- TRUE

        RUtilpol::output_comment(
          msg = "attempt = successful"
        )

        # remove the temporary result
        rm(bchron_temp, envir = current_env)
      } else {
        RUtilpol::output_comment(
          msg = "attempt = unsuccessful"
        )
      }
    }

    # if all batches are marked as succesfull, break
    if (all(data_source_batch$done) == TRUE) break

    # if cycle runs more times than it is a maximum number, break
    if (loop_counter >= maximum_number_of_loops) break

    # loop is finished, increase loop counter
    loop_counter <- loop_counter + 1
  }

  # Get the succesfull batches  -----

  # detect the number of successful batches
  number_of_successes <-
    sum(batch_success_table$done == TRUE)

  RUtilpol::output_comment(
    msg = paste(
      "Chronology was successfully calculated for", number_of_successes,
      "batches"
    )
  )

  # get list of all sequences which did not estimate
  failed_batches <-
    batch_success_table %>%
    dplyr::filter(done == FALSE)

  if (
    nrow(failed_batches) < 1
  ) {
    return()
  }

  failed_seq <-
    failed_batches %>%
    purrr::pluck("sequence_list") %>%
    unlist()

  return(failed_seq)
}
