#' @title Subset the chronology control tables based on the presence
#' of the previous AD models
#' @param data_source Data.frame with `dataset_id` and `chron_control`
#' @param dir Path to the data storage folder
#' @param rerun_ad Logical. Should age-depth models be re-run 'de novo' for
#' records where we have previous age-depth model result?
#' @param sites_to_rerun Optional vector with `dataset_id`s, which should be
#' re-run disregard of the presence of previous results
#' @export
chron_subset_previous_records <- function(data_source,
                                            dir,
                                            rerun_ad = FALSE,
                                            sites_to_rerun = NULL) {
  RUtilpol::check_class("data_source", "data.frame")

  RUtilpol::check_col_names("data_source", c("dataset_id", "chron_control"))

  RUtilpol::check_class("dir", "character")

  RUtilpol::check_class("rerun_ad", "logical")

  RUtilpol::check_class("sites_to_rerun", c("NULL", "character"))

  current_frame <- sys.nframe()
  current_env <- sys.frame(which = current_frame)

  dir <- RUtilpol::add_slash_to_path(dir)

  # pre-alocate the result as all records
  res <- data_source

  ds_to_run <-
    data_source %>%
    purrr::pluck("dataset_id")

  # check if AD models are already calculated
  # get all missing names
  ds_absent <-
    util_get_missing_ds_names(
      dir = paste0(dir, "Data/Processed/Chronology/Models_full/"),
      name_vector = ds_to_run
    )

  is_previous_ad_present <-
    length(ds_absent) > 0

  # assume that user does (not) want to run AD de novo
  run_ad_denovo_confirm <- rerun_ad

  # if user wants to run it de novo and there is a previous version
  #   of AD models, confirm that action
  if (
    isTRUE(rerun_ad) && isTRUE(is_previous_ad_present)
  ) {
    run_ad_denovo_confirm <-
      util_confirm(
        msg = paste(
          "Detected previous calculation of age-depth models.",
          "Are you sure you want to re-calculate 'de novo'?"
        )
      )
  }

  # if there is not any previous version of AD models
  if (
    isFALSE(run_ad_denovo_confirm) && isFALSE(is_previous_ad_present)
  ) {

    # warn user
    RUtilpol::output_comment(
      msg = paste(
        "Cannot detect previous AD models",
        "Will you re-calibrate all age-depth model 'de novo'"
      )
    )

    # switch to calcultion de novo
    run_ad_denovo_confirm <- TRUE
  }

  # if user does NOT want to create de novo
  if (
    isFALSE(run_ad_denovo_confirm)
  ) {

    # Look into the outputs and filter out all sites,
    #   which have Chronology output

    ds_to_rerun <-
      c(
        ds_absent,
        sites_to_rerun
      ) %>%
      unique()

    res <-
      data_source %>%
      dplyr::filter(dataset_id %in% ds_to_rerun)
  } else {
    RUtilpol::output_comment(
      msg = "AD models will be created 'de novo'"
    )
  }

  util_check_data_table(res)

  return(res)
}
