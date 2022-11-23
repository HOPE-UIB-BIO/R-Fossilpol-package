#' @title Recalibrate all age-depth models base on chronology control tables
#' @param data_source Data.frame containing `dataset_id` and `chron_control_format`
#' @param batch_size Number of individual sequences re-calibrate in a single
#' batch using parallel computation
#' @param number_of_cores Number of CPU cores to use in parallel computation
#' @param default_iteration The number of iterations used by Bchron
#' @param default_burn The number of starting iterations to discard used by Bchron
#' @param default_thin The step size for every iteration to keep beyond
#' the burnin used by Bchron
#' @param iteration_multiplier Value to be used to multiply `iteration`, `burn`,
#' `thin` to keep the ratio between them the same
#' @param set_seed User-defined seed for randomisations
#' @param dir Path to the data storage folder
#' @param batch_attempts Number of tries each batch should be considered
#' before skipping it
#' @param time_per_sequence Time (in sec) dedicated for each sequence to estimate
#' age-depth model. If it takes computer longer that selected value, estimation
#' is considered as unsuccessful and skipped. The time value is multiplied by
#' `iteration_multiplier` as more iteration required more time. Time for whole
#' batch is calculated as `time_per_sequence` multiplied by `iteration_multiplier`
#' multiplied by the number of sequences per batch (which is estimated based on
#' `number_of_cores`)
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

  RUtilpol::check_class("dir", "character")

  current_frame <- sys.nframe()
  current_env <- sys.frame(which = current_frame)

  # add slash if neede
  dir <-
    RUtilpol::add_slash_to_path(dir)

  # common path to the Chronology folder
  path_to_chron <-
    paste0(dir, "Data/Processed/Chronology/")

  seq_prepared <-
    data_source %>%
    purrr::pluck("dataset_id")

  # get all missing names
  seq_absent <-
    util_get_missing_seq_names(
      dir = paste0(path_to_chron, "Models_full/"),
      name_vector = seq_prepared
    )

  n_seq_to_run <- length(seq_absent)

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

  data_to_run <-
    data_source %>%
    dplyr::filter(dataset_id %in% seq_absent)

  # detect the number of batches needed to split the data into base on the
  #   `batch_size` selected by user
  number_of_batches <-
    ceiling(nrow(data_to_run) / batch_size)

  # a dummy data.freme to keep track of progress and number of tries for each batch
  batch_success_table <-
    tibble::tibble(
      batch_number = 1:number_of_batches,
      batch_name = paste0("batch_", formatC(batch_number, width = 3, flag = 0)),
      # list of sequences in each batch
      sequence_list = purrr::map(
        .x = batch_number,
        .f = ~ data_to_run %>%
          dplyr::slice(
            seq(
              from = batch_size * (.x - 1) + 1,
              to = min(c(batch_size * (.x), nrow(data_to_run))),
              by = 1
            )
          ) %>%
          purrr::pluck("dataset_id")
      ),
      # write sequences as vector
      sequence_vec = purrr::map_chr(
        .x = sequence_list,
        .f = ~ RUtilpol::paste_as_vector(.x, sep = "")
      ),
      # number of sequences in each batch
      batch_size = purrr::map_dbl(
        .x = sequence_list,
        .f = length
      ),
      # set a value for the process to wait (in seconds) for each batch
      time_to_stop = (batch_size * time_per_sequence) * max(c((floor(iteration_multiplier / 2)), 1)),
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

  # open custom menu to select confirmation
  style_selection <-
    switch(utils::menu(
      choices = c("both (batches and then individual) - recommended", "individual"),
      title = cat(
        "User can specify which kind of age-depth modeling wants to do", "\n",
        "\n",
        "batches = Several age-depth models are then created",
        "at the same time using parralel computation.",
        "This is done by splitting the sequences into batches, with each",
        "batch containing a certain number of sequences",
        "Moreover, the “failed” batches are estimated one by one.", "\n",
        "\n",
        "individual = age-depth models for sequences are estimated one by one", "\n",
        "\n"
      )
    ),
    "batches",
    "individual"
    )

  # Variables definition for computation  -----

  set.seed(set_seed)
  setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

  n_iterations <- default_iteration * iteration_multiplier
  n_burn <- default_burn * iteration_multiplier
  n_thin <- default_thin * iteration_multiplier

  temp_path <-
    paste0(path_to_chron, "Temporary_output/")

  if (
    style_selection == "batches"
  ) {
    RUtilpol::output_comment(
      paste(
        "Age-depth re-calibration will be done in", number_of_batches, "batches\n",
        "Each batch will have", batch_attempts, "attempts to calculate.",
        "In case that the whole bach is unsuccessful all", batch_attempts, "times,",
        "another subroutine can be used to calculate age-depth models",
        "for each sequence individually."
      )
    )

    chron_recalibrate_in_batches(
      data_source_chron = data_to_run,
      data_source_batch = batch_success_table,
      dir = dir,
      n_iterations = n_iterations,
      n_burn = n_burn,
      n_thin = n_thin,
      number_of_cores = number_of_cores,
      set_seed = set_seed,
      maximum_number_of_loops = batch_attempts
    )
  }

  # check agan the sequences to run
  # get all missing names
  seq_absent <-
    util_get_missing_seq_names(
      dir = paste0(path_to_chron, "Models_full/"),
      name_vector = seq_prepared
    )

  n_seq_to_run <- length(seq_absent)

  # if all succesfull
  if (
    n_seq_to_run < 1
  ) {
    return()
  }

  RUtilpol::output_comment(
    msg = paste(
      "Individual calculation will be done for", n_seq_to_run, "sequences"
    )
  )

  data_to_run <-
    data_source %>%
    dplyr::filter(dataset_id %in% seq_absent)

  chron_recalibrate_individual(
    data_source_chron = data_to_run,
    data_source_batch = batch_success_table,
    n_iterations = n_iterations,
    n_burn = n_burn,
    n_thin = n_thin,
    dir = dir
  )
}
