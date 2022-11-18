#' @title Save outputs of the chronology estimation as age-depth models and
#' predicted ages
#' @param data_source_predicted_ages Data.frame with `dataset_id` and `bchron_mod`
#' @param data_source_to_predict Data.frame with `dataset_id`, `levels`, and
#' `age_uncertainty`
#' @param current_state Named list congaing the summary of the presence of
#' previous results and current settings
#' @param sites_to_rerun Vector with `dataset_id`s of sequences to rerun even
#' if previous results are present
#' @param dir Path to the data storage folder
#' @export
chron_save_results <-
  function(data_source_predicted_ages,
           data_source_to_predict,
           current_state,
           sites_to_rerun,
           dir) {
    RUtilpol::check_class("data_source_predicted_ages", "data.frame")

    RUtilpol::check_col_names(
      "data_source_predicted_ages",
      c(
        "dataset_id",
        "bchron_mod"
      )
    )

    RUtilpol::check_class("data_source_to_predict", "data.frame")

    RUtilpol::check_col_names(
      "data_source_predicted_ages",
      c(
        "dataset_id",
        "levels",
        "age_uncertainty"
      )
    )

    RUtilpol::check_class("current_state", "list")

    RUtilpol::check_class("sites_to_rerun", c("character", "NULL"))

    RUtilpol::check_class("dir", "character")

    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    if (
      nrow(data_source_predicted_ages) < 0
    ) {
      RUtilpol::output_comment(
        msg = "No successful predicted ages. Not saving"
      )

      return()
    }

    RUtilpol::output_heading(
      msg = "Saving outputs"
    )

    # Chronology outputs  -----

    RUtilpol::output_comment(
      msg = "Saving Chronology outputs. This can take a while."
    )

    if (
      stringr::str_detect(current_state$setting_state, "FALSE-*") &
        stringr::str_detect(current_state$latest_ad_present, "TRUE-*")
    ) {

      # load the old outputs
      previous_models <-
        RUtilpol::get_latest_file(
          file_name = "chron_mod_output",
          dir = paste0(
            dir, "/Data/Processed/Chronology/Models_full"
          )
        ) %>%
        # filter out sequences marked to re-run
        dplyr::filter(!(dataset_id %in% sites_to_rerun))

      RUtilpol::check_if_loaded(
        file_name = "previous_models",
        env = current_env
      )

      RUtilpol::check_class("previous_models", "data.frame")

      previous_models_sub <-
        previous_models %>%
        dplyr::filter(!(dataset_id %in% data_source_predicted_ages$dataset_id))

      data_succesful_predicted <-
        dplyr::bind_rows(
          previous_models_sub,
          data_source_predicted_ages
        )
    } else if (
      stringr::str_detect(current_state$setting_state, ".*-TRUE")
    ) {
      data_succesful_predicted <-
        data_source_predicted_ages %>%
        dplyr::filter(dataset_id %in% data_source_to_predict$dataset_id)
    } else {
      data_succesful_predicted <- data_source_predicted_ages
    }

    models_full <-
      data_succesful_predicted %>%
      dplyr::distinct(dataset_id, .keep_all = TRUE) %>%
      dplyr::arrange(dataset_id) %>%
      dplyr::select(
        dplyr::all_of(
          c(
            "dataset_id",
            "bchron_mod"
          )
        )
      )

    # save
    # do not use the `RUtilpol::save_if_latests` as it cannot handle large files
    readr::write_rds(
      x = models_full,
      file = paste0(
        dir, "/Data/Processed/Chronology/Models_full/",
        paste0("chron_mod_output-", current_date, ".rds")
      ),
      compress = "gz"
    )

    # Predicted ages  -----

    RUtilpol::output_comment(
      msg = "Saving predicted ages"
    )

    if (
      stringr::str_detect(current_state$setting_state, "FALSE") &
        stringr::str_detect(current_state$latest_ad_present, ".*-TRUE")
    ) {

      # load the old pred ages
      previous_age_prediction <-
        RUtilpol::get_latest_file(
          file_name = "chron_predicted_ages",
          dir = paste0(
            dir, "/Data/Processed/Chronology/Predicted_ages"
          )
        )

      RUtilpol::check_if_loaded(
        file_name = "previous_age_prediction",
        env = current_env
      )

      RUtilpol::check_class("previous_age_prediction", "data.frame")

      previous_age_prediction_sub <-
        previous_age_prediction %>%
        dplyr::filter(!(dataset_id %in% data_source_predicted_ages$dataset_id))

      pred_ages_file <-
        dplyr::bind_rows(
          data_source_predicted_ages,
          previous_age_prediction_sub
        )
    } else {
      pred_ages_file <- data_source_predicted_ages
    }

    chron_predicted_ages <-
      pred_ages_file %>%
      dplyr::distinct(dataset_id, .keep_all = TRUE) %>%
      dplyr::arrange(dataset_id) %>%
      dplyr::select(
        !dplyr::any_of("bchron_mod")
      )

    RUtilpol::save_latest_file(
      object_to_save = chron_predicted_ages,
      dir = paste0(
        dir, "/Data/Processed/Chronology/Predicted_ages"
      ),
      prefered_format = "rds",
      use_sha = TRUE
    )

    # clean the files
    util_clean_chron_temp(dir)
  }
