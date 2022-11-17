#' @title Prepare data assembly of sequences to predict aged for based on the
#' presence of previous results and the user-defined settings
#' @param data_source Data.frame containing the `dataset_id`
#' @param dir Path to the data storage folder
#' @param sites_to_rerun Vector with `dataset_id`s of sequences to rerun even
#' if previous results are present
#' @param current_state Named list congaing the summary of the presence of
#' previous results and current settings
#' @export
chron_prepare_ad_to_predict <-
  function(data_source,
           dir,
           sites_to_rerun = c(),
           current_state) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "dataset_id")

    RUtilpol::check_class("dir", "character")

    RUtilpol::check_class("sites_to_rerun", c("character", "NULL"))

    RUtilpol::check_class("current_state", "list")

    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    # Four different possibilities of data preparation for predicting ages based on
    #   user settings (`calc_AD_models_denovo`, `predict_ages_denovo`)

    if (
      current_state$setting_state == "FALSE-FALSE"
    ) {

      # calc DN: F; pred DN F  -----

      # load the data for last Chronology output and age prediction, then select for
      #   prediction only sequences, which does not have Chronology output and age
      #   predictions

      RUtilpol::output_comment(
        msg = paste(
          "Ages will be predicted for sequences, which are not present in previous",
          "version of the Chronology outputs."
        )
      )

      # if there are any previous Bcrhon outputs
      if (
        current_state$latest_ad_present == "TRUE-TRUE"
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

        # merge previous outputs
        previous_outputs <-
          dplyr::inner_join(
            previous_models %>%
              dplyr::select(
                dplyr::all_of("dataset_id")
              ),
            previous_age_prediction %>%
              dplyr::select(
                dplyr::all_of("dataset_id")
              ),
            by = "dataset_id"
          )

        # filter out sequences which have a successful predicted ages
        data_to_predict <-
          data_source %>%
          dplyr::filter(!(dataset_id %in% previous_outputs$dataset_id)) %>%
          dplyr::distinct(dataset_id, .keep_all = TRUE) %>%
          dplyr::arrange(dataset_id)
      } else {
        data_to_predict <-
          data_source %>%
          dplyr::distinct(dataset_id, .keep_all = TRUE) %>%
          dplyr::arrange(dataset_id)
      }
    } else if (
      current_state$setting_state == "FALSE-TRUE"
    ) {

      # calc DN: F; pred DN T  -----

      # load the data for last Chronology output, then filter out sequences from current
      #   Chronology outputs, which have previous Chronology output. Select for prediction
      #   all sequences merged together.

      RUtilpol::output_comment(
        msg = paste(
          "Ages will be predicted for all sequences, as combination of current and",
          "previous Chronology outputs"
        )
      )

      RUtilpol::stop_if_not(
        stringr::str_detect(current_state$latest_ad_present, "TRUE-*"),
        false_msg = "Cannot detect the previous AD models for selected setting",
        true_msg = "Detected AD models for for selected setting"
      )

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

      # filter out sequences which have successful Chronology output
      chron_output_sub <-
        data_source %>%
        dplyr::filter(!(dataset_id %in% previous_models$dataset_id))

      # merge old and new Chronology output to predict
      data_to_predict <-
        dplyr::bind_rows(
          chron_output_sub,
          previous_models
        ) %>%
        dplyr::distinct(dataset_id, .keep_all = TRUE) %>%
        dplyr::arrange(dataset_id)
    } else if (
      current_state$setting_state == "TRUE-TRUE"
    ) {

      # calc DN: T; pred DN T  -----

      # load the data for last Chronology output, then merge current
      #   Chronology outputs with old sequences, which are not present in current run.
      #   Predict for all of them.

      RUtilpol::output_comment(
        msg = paste(
          "Ages will be predicted for all sequences."
        )
      )

      if (
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

        # filter out sequences have new Chronology output
        previous_models_sub <-
          previous_models %>%
          dplyr::filter(!(dataset_id %in% data_source$dataset_id))

        # merge old and new Chronology output to predict
        data_to_predict <-
          dplyr::bind_rows(
            previous_models_sub,
            data_source
          ) %>%
          dplyr::distinct(dataset_id, .keep_all = TRUE) %>%
          dplyr::arrange(dataset_id)
      } else {
        data_to_predict <-
          data_source %>%
          dplyr::distinct(dataset_id, .keep_all = TRUE) %>%
          dplyr::arrange(dataset_id)
      }
    } else if (
      current_state$setting_state == "TRUE-FALSE"
    ) {

      # calc DN: T; pred DN F  -----

      # Use only the current Bchorn result for prediction

      RUtilpol::output_comment(
        msg = paste(
          "Ages will be predicted for current Chronology results."
        )
      )

      data_to_predict <- data_source
    } else {
      data_to_predict <- NA
    }

    return(data_to_predict)
  }
