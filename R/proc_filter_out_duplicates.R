#' @title Filter out duplicated dataset
#' @param data_source Data frame containing the data to be filtered
#' @param data_storage_path Path to the data storage folder
#' @param filter_var Name of a column that indicates which column to be used to
#' compare the data (character)
#' @param n_subgroups number of subgroups to split the data based on the
#' geography (numeric). Sites within subgroup will be tested only if subgroup
#' contains sites form different `filter_var`
#' @param max_degree_distance maximal euclidean distance between two sites to
#' consider them as material for comparison (numeric)
#' @export
proc_filter_out_duplicates <-
  function(data_source,
           data_storage_path,
           filter_var,
           n_subgroups = NULL,
           max_degree_distance = 1) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", c("long", "lat", eval(filter_var)))

    RUtilpol::check_class("data_storage_path", "character")

    RUtilpol::check_class("filter_var", "character")

    if (
      is.null(n_subgroups)
    ) {
      n_subgroups <- max(round(nrow(data_source) / 100), 1) # set 1 group per 100 sequences
    }

    RUtilpol::check_class("n_subgroups", "numeric")

    assertthat::assert_that(
      n_subgroups > 0,
      msg = "'n_subgroups' must be larger than 0"
    )

    RUtilpol::check_class("max_degree_distance", "numeric")

    assertthat::assert_that(
      max_degree_distance > 0,
      msg = "'max_degree_distance' must be larger than 0"
    )

    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    file_dir <-
      paste0(data_storage_path, "/Data/Input/Potential_duplicates/")

    detect_duplicates_confirm <-
      util_confirm_based_on_presence(
        dir = file_dir,
        file_name = "potential_duplicates",
        msg = paste(
          "Detected previous record of duplication detection.",
          "Are you sure you want to detect duplicates again?"
        )
      )

    if (
      detect_duplicates_confirm == TRUE
    ) {
      data_work <- data_source

      # detect duplicates
      potential_duplicates_data <-
        proc_detect_duplicates(
          data_source = data_work,
          source_var = filter_var,
          n_subgroups = n_subgroups,
          # maximal distance to potential duplicate
          maximal_distance = max_degree_distance
        )

      # if there are some duplicates
      if (
        all(is.na(potential_duplicates_data)) == FALSE
      ) {

        # load/create table with potential duplicates
        potential_duplicates <-
          stopcheck_table(
            data_source = potential_duplicates_data,
            file_name = "potential_duplicates",
            dir = file_dir,
            sel_method = "duplicates",
            msg = paste(
              "Select which dataset should be deleted in each pair by flagging",
              "'1' or '2' in the 'delete' column. '0' will leave both datasets in."
            )
          )
      }
    } else {

      # load/create table with potential duplicates
      potential_duplicates <-
        stopcheck_table(
          data_source = tibble::tibble(
            dataset_A = NA_character_,
            dataset_B = NA_character_,
            distance = NA_real_,
            similarity = NA_real_,
            .rows = 0
          ),
          file_name = "potential_duplicates",
          dir = file_dir,
          sel_method = "duplicates",
          msg = paste(
            "Select which dataset should be deleted in each pair by flagging",
            "'1' or '2' in the 'delete' column. '0' will leave both datasets in."
          )
        )
    }

    # if there are some duplicates
    if (
      exists("potential_duplicates", envir = current_env)
    ) {

      # extract vector with for datasets to be excluded as duplicates
      dataset_id_tobe_excluded <-
        potential_duplicates %>%
        dplyr::mutate_at("dataset_A", as.character) %>%
        dplyr::mutate_at("dataset_B", as.character) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          dataset_id = purrr::pmap_chr(
            .l = list(dataset_A, dataset_B, delete),
            .f = ~ {
              if (..3 == 1) {
                return(..1)
              }
              if (..3 == 2) {
                return(..2)
              }
              return(NA)
            }
          )
        ) %>%
        tidyr::drop_na(dataset_id) %>%
        purrr::pluck("dataset_id")
    }

    #----------------------------------------------------------#
    # 5. Filter out unwanted datasets  -----
    #----------------------------------------------------------#

    RUtilpol::output_comment(
      msg = "Filtering out unwanted datasets"
    )

    # if there are no duplicates, create an empty vector
    if (
      !exists("dataset_id_tobe_excluded", envir = current_env)
    ) {
      dataset_id_tobe_excluded <- c()
    }

    data_source_filtered <-
      data_source %>%
      dplyr::filter(!dataset_id %in% dataset_id_tobe_excluded)

    util_check_data_table(data_source_filtered)

    return(data_source_filtered)
  }
