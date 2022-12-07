#' @title Predict ages for the selected data assembly
#' @param data_source Data.frame with `dataset_id` and `sample_depth`
#' @param dir Path to the data storage folder
#' @param predict_ages_denovo Setting to define if ages should be predicted from
#' last models 'de novo'
#' @param sites_to_rerun Vector with `dataset_id`s of sequences to rerun even
#' if previous results are present
#' @description Load the depth information from the last saved data assembly,
#' then predict the ages using the age-depth models.
#' @return Data with `levels`, which contains `depth` and predicted `age, and
#' `age_uncertainty`, which contains age uncertainty matrix.
#' @export
chron_predict_all_ages <- function(data_source,
                                   dir,
                                   predict_ages_denovo,
                                   sites_to_rerun) {
  RUtilpol::check_class("data_source", "data.frame")

  RUtilpol::check_col_names(
    "data_source",
    c(
      "dataset_id",
      "sample_depth"
    )
  )

  RUtilpol::check_class("dir", "character")

  RUtilpol::check_class("predict_ages_denovo", "logical")

  RUtilpol::check_class("sites_to_rerun", c("character", "NULL"))

  current_frame <- sys.nframe()
  current_env <- sys.frame(which = current_frame)

  dir <- RUtilpol::add_slash_to_path(dir)

  # get list of AD models to use
  ds_with_levels <-
    data_source %>%
    purrr::pluck("dataset_id")

  # set all sequences to run as default
  ds_to_run <- ds_with_levels

  # load the old pred ages
  data_previous_age_prediction <-
    RUtilpol::get_latest_file(
      file_name = "chron_predicted_ages",
      dir = paste0(
        dir, "/Data/Processed/Chronology/Predicted_ages"
      )
    )

  RUtilpol::check_if_loaded(
    file_name = "data_previous_age_prediction",
    env = current_env
  )

  ds_previous_age_prediction <- c()

  is_previous_age_present <-
    isFALSE(
      all(
        is.na(data_previous_age_prediction)
      )
    )

  if (
    isTRUE(is_previous_age_present)
  ) {
    ds_previous_age_prediction <-
      data_previous_age_prediction %>%
      # filter out sequences marked to re-run
      dplyr::filter(!(dataset_id %in% sites_to_rerun)) %>%
      purrr::pluck("dataset_id")
  }

  if (
    isFALSE(predict_ages_denovo)
  ) {
    ds_to_run <-
      ds_with_levels[!ds_with_levels %in% ds_previous_age_prediction]
  }

  # Try to predict
  data_age_predicted <-
    data_source %>%
    dplyr::select(
      dplyr::all_of(
        c("dataset_id", "sample_depth")
      )
    ) %>%
    dplyr::filter(
      dataset_id %in% ds_to_run
    ) %>%
    dplyr::arrange(dataset_id) %>%
    dplyr::mutate(
      chron_predicted_ages = purrr::map2(
        .x = dataset_id,
        .y = sample_depth,
        .f = ~ {
          inner_frame <- sys.nframe()
          inner_env <- sys.frame(which = inner_frame)


          message(
            paste0(
              "dataset_id = ", .x
            )
          )

          # get AD model
          sel_ad_model <-
            RUtilpol::get_latest_file(
              file_name = .x,
              dir = paste0(dir, "Data/Processed/Chronology/Models_full/"),
              verbose = FALSE
            )

          # if model is missing return NA
          if (
            isTRUE(
              all(
                is.na(sel_ad_model)
              )
            )
          ) {
            return(NA_real_)
          }

          # tyr to predict model
          try(
            expr = {
              sel_ad_pred <-
                chron_predict_ages(
                  data_source = sel_ad_model,
                  sample_data = .y
                )
            },
            silent = TRUE
          )

          # if did not predict return NA
          if (
            isFALSE(
              exists("sel_ad_pred", envir = inner_env)
            )) {
            return(NA_real_)
          }

          return(sel_ad_pred)
        }
      )
    )

  RUtilpol::check_if_loaded(
    file_name = "data_age_predicted",
    env = current_env
  )

  RUtilpol::check_class("data_age_predicted", "data.frame")

  RUtilpol::check_col_names("data_age_predicted", "chron_predicted_ages")

  util_check_data_table(data_age_predicted)

  # filter out unsuccessful age prediction and split Bchron output
  #   into 2 columns
  data_age_predicted_summary <-
    data_age_predicted %>%
    dplyr::mutate(
      fail_to_predict_ages = purrr::map_lgl(
        .x = chron_predicted_ages,
        .f = ~ any(is.na(.x))
      )
    ) %>%
    dplyr::filter(fail_to_predict_ages == FALSE) %>%
    dplyr::mutate(
      levels = purrr::map(
        .x = chron_predicted_ages,
        .f = ~ .x$ages
      ),
      age_uncertainty = purrr::map(
        .x = chron_predicted_ages,
        .f = ~ .x$age_position
      )
    ) %>%
    dplyr::select(
      !dplyr::any_of(
        c(
          "fail_to_predict_ages",
          "chron_predicted_ages",
          "sample_depth"
        )
      )
    )

  RUtilpol::check_if_loaded(
    file_name = "data_age_predicted_summary",
    env = current_env
  )

  RUtilpol::check_class("data_age_predicted_summary", "data.frame")

  RUtilpol::check_col_names(
    "data_age_predicted_summary",
    c("levels", "age_uncertainty")
  )

  RUtilpol::output_comment(
    paste(
      "Ages were sucessfully predicted for",
      nrow(data_age_predicted_summary), "out of",
      nrow(data_age_predicted), "sequences"
    )
  )

  # save sites that fail to run
  if (
    nrow(data_age_predicted_summary) != nrow(data_age_predicted)
  ) {
    sites_fail_to_predict <-
      data_age_predicted %>%
      dplyr::filter(!dataset_id %in% data_age_predicted_summary$dataset_id) %>%
      dplyr::distinct(dataset_id, region, n_chron_control, age_type) %>%
      dplyr::arrange(dataset_id)

    if (
      nrow(sites_fail_to_predict) > 0
    ) {
      sites_fail_path <-
        paste0(dir, "/Data/Processed/Chronology/Temporary_output/")

      readr::write_csv(
        sites_fail_to_predict,
        paste0(sites_fail_path, "sites_fail_to_predict_", Sys.Date(), ".csv")
      )

      RUtilpol::output_comment(
        paste(
          "There are several sequences, which fail to predict ages.", "\n",
          "You can see them in:",
          sites_fail_path
        )
      )

      RUtilpol::open_dir(
        dir = sites_fail_path
      )
    }
  }

 RUtilpol::check_col_names(
   "data_age_predicted_summary",
   c("levels", "age_uncertainty")
 )

  res <- data_age_predicted_summary

  # merge predicted data with the previous saved
  if (
    isTRUE(is_previous_age_present)
  ) {
    res <-
      data_previous_age_prediction %>%
      dplyr::filter(!dataset_id %in% data_age_predicted_summary) %>%
      dplyr::bind_rows(
        data_age_predicted_summary
      ) %>%
      dplyr::distinct(dataset_id)
  }

  return(res)
}
