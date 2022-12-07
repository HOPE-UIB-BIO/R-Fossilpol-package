#' @title Predict ages for the selected data assembly
#' @param data_source Data.frame with `dataset_id` and `bchron_mod`
#' @param dir Path to the data storage folder
#' @param date Date to named the files with
#' @description Load the depth information from the last saved data assembly,
#' then predict the ages using the age-depth models in `data_source`.
#' @return Data with `levels`, which contains `depth` and predicted `age, and
#' `age_uncertainty`, which contains age uncertainty matrix.
#' @export
chron_predict_all_ages <- function(data_source,
                                   dir,
                                   date) {
  RUtilpol::check_class("data_source", "data.frame")

  RUtilpol::check_col_names(
    "data_source",
    c(
      "dataset_id",
      "bchron_mod"
    )
  )

  RUtilpol::check_class("dir", "character")

  RUtilpol::check_class("date", "Date")

  current_frame <- sys.nframe()
  current_env <- sys.frame(which = current_frame)

  dir <- RUtilpol::add_slash_to_path(dir)

  # load the processed data
  data_level_depth <-
    RUtilpol::get_latest_file(
      file_name = "data_merged",
      dir = paste0(
        dir, "/Data/Processed/Data_merged"
      )
    ) %>%
    dplyr::select(
      dplyr::all_of(
        c("dataset_id", "sample_depth")
      )
    )

  RUtilpol::check_if_loaded(
    file_name = "data_level_depth",
    env = current_env
  )

  RUtilpol::check_class("data_level_depth", "data.frame")

  # merge them together
  data_work <-
    dplyr::inner_join(
      data_level_depth,
      data_source,
      by = "dataset_id"
    )

  # Try to predict
  data_age_predicted <-
    data_work %>%
    dplyr::arrange(dataset_id) %>%
    dplyr::mutate(chron_predicted_ages = purrr::map2(
      .x = bchron_mod,
      .y = sample_depth,
      .f = purrr::possibly(
        .f = ~ chron_predict_ages(
          data_source = .x,
          sample_data = .y
        ),
        otherwise = NA_real_
      )
    ))

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
        paste0(sites_fail_path, "sites_fail_to_predict_", date, ".csv")
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

  return(data_age_predicted_summary)
}
