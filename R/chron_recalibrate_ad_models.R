#' @title Recalibrate all age-depth models base on chronology control tables
#' @param data_source
#' Data.frame containing `dataset_id` and `chron_control_format`
#' @param batch_size Number of individual sequences re-calibrate in a single
#' batch using parallel computation
#' @param number_of_cores Number of CPU cores to use in parallel computation
#' @param default_iteration The number of iterations used by Bchron
#' @param default_burn
#' The number of starting iterations to discard used by Bchron
#' @param default_thin The step size for every iteration to keep beyond
#' the burnin used by Bchron
#' @param iteration_multiplier Value to be used to multiply `iteration`, `burn`,
#' `thin` to keep the ratio between them the same
#' @param set_seed User-defined seed for randomisations
#' @param dir Path to the data storage folder
#' @param batch_attempts Number of tries each batch should be considered
#' before skipping it
#' @param time_per_sequence
#' Time (in sec) dedicated for each sequence to estimate
#' age-depth model. If it takes computer longer that selected value, estimation
#' is considered as unsuccessful and skipped. The time value is multiplied by
#' `iteration_multiplier` as more iteration required more time. Time for whole
#' batch is calculated as `time_per_sequence` multiplied by
#' `iteration_multiplier` multiplied by the number of sequences per batch
#' (which is estimated based on `number_of_cores`)
#' @export
chron_recalibrate_ad_models <- function(data_source,
                                        batch_size = 1,
                                        number_of_cores = 1,
                                        default_iteration = 10e3,
                                        default_burn = 2e3,
                                        default_thin = 8,
                                        iteration_multiplier = 5,
                                        set_seed = 1234,
                                        batch_attempts = 3,
                                        time_per_sequence = 120,
                                        dir) {
  RUtilpol::check_class("data_source", "data.frame")

  RUtilpol::check_col_names(
    "data_source",
    c(
      "dataset_id",
      "chron_control_format"
    )
  )

  RUtilpol::check_class("batch_size", "numeric")

  RUtilpol::check_class("number_of_cores", "numeric")

  RUtilpol::check_class("default_iteration", "numeric")

  RUtilpol::check_class("default_burn", "numeric")

  RUtilpol::check_class("default_thin", "numeric")

  RUtilpol::check_class("iteration_multiplier", "numeric")

  RUtilpol::check_class("set_seed", "numeric")

  RUtilpol::check_class("batch_attempts", "numeric")

  RUtilpol::check_class("time_per_sequence", "numeric")

  RUtilpol::check_class("dir", "character")

  # add slash if neede
  dir <-
    RUtilpol::add_slash_to_path(dir)

  # common path to the Chronology folder
  path_to_chron <-
    paste0(dir, "Data/Processed/Chronology/")

  n_seq_to_run <-
    nrow(data_source)

  # test if there are any sequences to run AD modelling
  if (
    length(n_seq_to_run) < 1
  ) {
    RUtilpol::output_comment(
      msg = paste(
        "There are no sequences for AD calculation,",
        "re-calibration will be skipped in current run"
      )
    )
    return()
  }

  RUtilpol::output_comment(
    paste(
      n_seq_to_run,
      "age-depth model(s) will be recalibrated"
    )
  )


  # Variables definition for computation  -----

  set.seed(set_seed)
  setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

  n_iterations <- default_iteration * iteration_multiplier
  n_burn <- default_burn * iteration_multiplier
  n_thin <- default_thin * iteration_multiplier

  style_selection <-
    ifelse(
      test = isTRUE(number_of_cores > 1),
      yes = "batches",
      no = "individual"
    )

  # pre-alocate data file
  data_to_run <-
    data_source

  if (
    style_selection == "batches"
  ) {
    RUtilpol::output_comment(
      paste(
        "Several age-depth models will re-calibrated",
        "at the same time using parralel computation in",
        number_of_batches, "batches\n",
        "Each batch will have", batch_attempts, "attempts to calculate.",
        "In case that the whole bach is unsuccessful all",
        batch_attempts, "times,",
        "another subroutine can be used to calculate age-depth models",
        "for each sequence individually."
      )
    )

    failed_seq <-
      chron_recalibrate_in_batches(
        data_source_chron = data_to_run,
        batch_size = batch_size,
        time_per_sequence = time_per_sequence,
        dir = dir,
        n_iterations = n_iterations,
        n_burn = n_burn,
        n_thin = n_thin,
        number_of_cores = number_of_cores,
        set_seed = set_seed,
        maximum_number_of_loops = batch_attempts
      )

    n_seq_failed <- length(failed_seq)

    # if all succesfull
    if (
      n_seq_failed < 1
    ) {
      return()
    }

    data_to_run <-
      data_to_run %>%
      dplyr::filter(dataset_id %in% failed_seq)

    n_seq_to_run <- n_seq_failed
  }

  RUtilpol::output_comment(
    msg = paste(
      "Individual calculation will be done for", n_seq_to_run, "sequences"
    )
  )

  chron_recalibrate_individual(
    data_source_chron = data_to_run,
    n_iterations = n_iterations,
    n_burn = n_burn,
    n_thin = n_thin,
    dir = dir
  )

  return()
}
