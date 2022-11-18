#' @title Save all the reference tables of the data assembly
#' @param data_source Data.frame with `dataset_id` and other variables
#' @param project_database Project dataset database
#' @param user_sel_variables Vector with variables, which have to be present in the
#' final data assembly
#' @param dir Path to the data storage folder
proc_save_references <-
  function(data_source,
           project_database,
           user_sel_variables = c(),
           dir) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "dataset_id")

    RUtilpol::check_class("project_database", "proj_db_class")

    RUtilpol::check_class(
      "user_sel_variables",
      c(
        "NULL",
        "character"
      )
    )

    RUtilpol::check_class("dir", "character")

    refference_path <-
      paste0(dir, "/Outputs/Tables/Meta_and_references/")

    RUtilpol::output_comment(
      msg = "Saving meta-information about data assembly"
    )

    data_assembly_meta <-
      data_source %>%
      dplyr::select(
        dplyr::any_of(
          c(
            "dataset_id",
            "handle", "siteid", "sitename",
            "long", "lat", "altitude",
            "depositionalenvironment",
            "region", "country", "harmonisation_region",
            "n_sample_counts", "age_min", "age_max",
            "n_chron_control",
            "age_type", "curve_name", "postbomb_curve_name",
            "pollen_percentage",
            "young_age", "old_age", "end_of_interest_period",
            "source_of_data", "data_publicity",
            "doi",
            user_sel_variables
          )
        )
      )

    RUtilpol::save_latest_file(
      object_to_save = data_assembly_meta,
      dir = refference_path,
      prefered_format = "csv"
    )

    # Authors of datasets -----

    RUtilpol::output_comment(
      msg = "Saving authors of data assembly"
    )

    authors_present <-
      dplyr::inner_join(
        data_assembly_meta %>%
          dplyr::select(dataset_id),
        db_Auth_dataset_tab(project_database),
        by = "dataset_id"
      ) %>%
      dplyr::left_join(
        db_Authors(project_database),
        by = "author_id"
      )

    affiliation_present_with_n <-
      authors_present %>%
      dplyr::distinct(author_id) %>%
      dplyr::inner_join(
        db_Auth_aff_tab(project_database),
        by = "author_id"
      ) %>%
      dplyr::distinct(affiliation_id) %>%
      dplyr::inner_join(
        db_Affiliations(project_database),
        by = "affiliation_id"
      ) %>%
      dplyr::mutate(
        afffiliation_number = dplyr::row_number()
      ) %>%
      dplyr::relocate(afffiliation_number)

    RUtilpol::check_col_names("affiliation_present_with_n", "afffiliation_number")

    affiliation_present <-
      db_Auth_aff_tab(project_database) %>%
      dplyr::inner_join(
        affiliation_present_with_n,
        by = "affiliation_id"
      ) %>%
      dplyr::inner_join(
        authors_present %>%
          dplyr::distinct(author_id),
        by = "author_id"
      )

    author_table <-
      authors_present %>%
      dplyr::mutate(
        datasets = purrr::map_chr(
          .x = author_id,
          .f = ~ authors_present %>%
            dplyr::filter(author_id == .x) %>%
            purrr::pluck("dataset_id") %>%
            unique() %>%
            sort() %>%
            RUtilpol::paste_as_vector(.) %>%
            return()
        )
      ) %>%
      dplyr::distinct(author_id, .keep_all = TRUE) %>%
      dplyr::mutate(
        affiliation = purrr::map_chr(
          .x = author_id,
          .f = ~ affiliation_present %>%
            dplyr::filter(author_id == .x) %>%
            purrr::pluck("afffiliation_number") %>%
            unique() %>%
            sort() %>%
            RUtilpol::paste_as_vector(.) %>%
            return()
        )
      ) %>%
      dplyr::select(
        !dplyr::any_of(
          c("dataset_id", "author_id")
        )
      ) %>%
      dplyr::arrange(last_name)

    RUtilpol::check_col_names(
      "author_table", "last_name"
    )

    RUtilpol::save_latest_file(
      object_to_save = author_table,
      dir = refference_path,
      prefered_format = "csv"
    )

    affiliation_table <-
      affiliation_present_with_n %>%
      dplyr::select(
        !dplyr::any_of("affiliation_id")
      )

    RUtilpol::save_latest_file(
      object_to_save = affiliation_table,
      dir = refference_path,
      prefered_format = "csv"
    )

    RUtilpol::output_comment(
      msg = "Saving reproducibility package"
    )

    config_file <-
      paste0(dir, "/R/00_Config_file.R")

    stop_check_tables <-
      tibble::tribble(
        ~table_name, ~file_name, ~path,
        "bchron_crash_file", "Crash_file", "/Data/Input/Chronology_setting/Bchron_crash/",
        "chronology_control_points", "chron_control_point_types", "/Data/Input/Chronology_setting/Chron_control_point_types/",
        "depositional_env_neotoma", "depositional_environment_selection", "/Data/Input/Depositional_environment/Neotoma/",
        "depositional_env_private", "depositional_environment_selection", "/Data/Input/Depositional_environment/Other/",
        "ecological_group", "eco_group", "/Data/Input/Eco_group/",
        "potential_duplicates", "potential_duplicates", "/Data/Input/Potential_duplicates/",
        "potential_pmc", "potential_pmc", "/Data/Input/Chronology_setting/Percentage_radiocarbon/",
        "regional_age_limits", "regional_age_limits", "/Data/Input/Regional_age_limits/"
      ) %>%
      dplyr::mutate(
        path_full = paste0(dir, path),
        latest_file = purrr::map2_chr(
          .x = path_full,
          .y = file_name,
          .f = ~ RUtilpol::get_latest_file_name(
            file_name = .y,
            dir = .x
          )
        ),
        present = purrr::map_lgl(
          .x = latest_file,
          .f = ~ isFALSE(is.na(.x))
        )
      ) %>%
      dplyr::filter(present == TRUE) %>%
      dplyr::mutate(
        file_name_full = paste0(path_full, latest_file)
      ) %>%
      purrr::pluck("file_name_full")

    if (
      "harmonisation_region" %in% names(data_source)
    ) {
      harm_tables <-
        tibble::tibble(
          harmonisation_region = data_source$harmonisation_region %>%
            unique() %>%
            sort()
        ) %>%
        dplyr::mutate(
          path_full = paste0(
            dir, "/Data/Input/Harmonisation_tables/"
          ),
          latest_file = purrr::map2_chr(
            .x = path_full,
            .y = harmonisation_region,
            .f = ~ RUtilpol::get_latest_file_name(
              file_name = .y,
              dir = .x
            )
          ),
          present = purrr::map_lgl(
            .x = latest_file,
            .f = ~ isFALSE(is.na(.x))
          )
        ) %>%
        dplyr::filter(present == TRUE) %>%
        dplyr::mutate(
          file_name_full = paste0(path_full, latest_file)
        ) %>%
        purrr::pluck("file_name_full")
    } else {
      harm_tables <- NULL
    }

    spatial_files <-
      list.files(
        path = paste0(dir, "/Data/Input/Spatial"),
        full.names = TRUE,
        recursive = TRUE
      )

    utils::zip(
      zipfile = paste0(refference_path, "reproducibility_bundle.zip"),
      files = c(config_file, stop_check_tables, harm_tables, spatial_files)
    )


    RUtilpol::open_dir(
      dir = refference_path
    )
  }
