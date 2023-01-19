#' @title Save all the reference tables of the data assembly
#' @param data_source Data.frame with `dataset_id` and other variables
#' @param project_database Project dataset database
#' @param user_sel_variables Vector with variables, which have to be present in
#' the final table with meta data
#' @param selected_outputs Character. Vector with selected outputs.
#' Currently available:
#' \itemize{
#' \item `"meta_table"` - table with all sequences in the final dataset
#' compilation with their meta information
#' \item `"author_table"` - table containing information about the datasets
#' used, the main data contributor, and their contact information
#' \item `"affiliation_table"` - table linking affiliations and their authors.
#' This is currently NOT available for Neotoma data.
#' \item `"graphical_summary"` -
#' \item `"reproducibility_bundle"` - a zip file of the Config file, all
#' "stop-checks" CSV tables, and all shapefiles
#' }
#' @param dir Path to the data storage folder
#' @param image_width,image_height Numeric. Define size properties of figures
#' produced in `graphical_summary`
#' @param image_units Character. The units in which the figure should be
#' measured.
#' @param ... addtition parameters passed to [plot_graphical_summary()]
#' @export
proc_save_references <- function(data_source,
                                 project_database,
                                 user_sel_variables = c(),
                                 selected_outputs = c(
                                   "meta_table",
                                   "author_table",
                                   "affiliation_table",
                                   "graphical_summary",
                                   "reproducibility_bundle"
                                 ),
                                 dir,
                                 image_width = 20,
                                 image_height = 7,
                                 image_units = c("in", "cm", "mm", "px"),
                                 ...) {
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

  RUtilpol::check_class("selected_outputs", "character")

  RUtilpol::check_vector_values(
    "selected_outputs",
    c(
      "meta_table",
      "author_table",
      "affiliation_table",
      "graphical_summary",
      "reproducibility_bundle"
    )
  )

  RUtilpol::check_class("dir", "character")

  dir <- RUtilpol:::add_slash_to_path(dir)

  refference_path <-
    paste0(dir, "Outputs/Meta_and_references/")

  # Meta data -----

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

  if (
    "meta_table" %in% selected_outputs
  ) {
    RUtilpol::output_comment(
      msg = "Saving meta-information about data assembly"
    )

    RUtilpol::save_latest_file(
      object_to_save = data_assembly_meta,
      dir = refference_path,
      prefered_format = "csv"
    )
  }

  # Authors of datasets -----

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

  if (
    "author_table" %in% selected_outputs
  ) {
    RUtilpol::output_comment(
      msg = "Saving authors of data assembly"
    )

    RUtilpol::save_latest_file(
      object_to_save = author_table,
      dir = refference_path,
      prefered_format = "csv"
    )
  }

  # Affiliation table -----

  affiliation_table <-
    affiliation_present_with_n %>%
    dplyr::select(
      !dplyr::any_of("affiliation_id")
    )

  if (
    "affiliation_table" %in% selected_outputs
  ) {
    RUtilpol::output_comment(
      msg = "Saving author's affiiations"
    )

    RUtilpol::save_latest_file(
      object_to_save = affiliation_table,
      dir = refference_path,
      prefered_format = "csv"
    )
  }

  # graphical summary ----

  if (
    "graphical_summary" %in% selected_outputs
  ) {
    RUtilpol::output_comment(
      msg = "Saving graphical summary"
    )

    graphical_summary <-
      plot_graphical_summary(
        data_source = data_source,
        ...
      )

    ggplot2::ggsave(
      filename = paste0(
        refference_path, "graphical_summary_", Sys.Date(), ".pdf"
      ),
      plot = graphical_summary,
      width = image_width,
      height = image_height,
      units = image_units
    )
  }


  # Reproducubility package -----

  if (
    "reproducibility_bundle" %in% selected_outputs
  ) {
    RUtilpol::output_comment(
      msg = "Saving reproducibility package"
    )

    stop_check_tables <-
      tibble::tribble(
        ~table_name, ~file_name, ~path,
        "bchron_crash_file", "Crash_file", "Data/Input/Chronology_setting/Bchron_crash/",
        "chronology_control_points", "chron_control_point_types", "Data/Input/Chronology_setting/Chron_control_point_types/",
        "depositional_env_neotoma", "depositional_environment_selection", "Data/Input/Depositional_environment/Neotoma/",
        "depositional_env_other", "depositional_environment_selection", "Data/Input/Depositional_environment/Other/",
        "ecological_group", "eco_group", "Data/Input/Eco_group/",
        "potential_duplicates", "potential_duplicates", "Data/Input/Potential_duplicates/",
        "potential_pmc", "potential_pmc", "Data/Input/Chronology_setting/Percentage_radiocarbon/",
        "regional_age_limits", "regional_age_limits", "Data/Input/Regional_age_limits/"
      ) %>%
      dplyr::mutate(
        path_full = paste0(dir, path),
        latest_file = purrr::map2_chr(
          .x = path_full,
          .y = file_name,
          .f = ~ RUtilpol::get_latest_file_name(
            file_name = .y,
            dir = .x,
            verbose = FALSE
          )
        ),
        present = purrr::map_lgl(
          .x = latest_file,
          .f = ~ isFALSE(is.na(.x))
        )
      ) %>%
      dplyr::filter(present == TRUE) %>%
      dplyr::mutate(
        file_name_full = paste0(path, latest_file)
      ) %>%
      purrr::pluck("file_name_full")

    harm_tables <- NULL

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
          path = "Data/Input/Harmonisation_tables/",
          path_full = paste0(dir, path),
          latest_file = purrr::map2_chr(
            .x = path_full,
            .y = harmonisation_region,
            .f = ~ RUtilpol::get_latest_file_name(
              file_name = .y,
              dir = .x,
              verbose = FALSE
            )
          ),
          present = purrr::map_lgl(
            .x = latest_file,
            .f = ~ isFALSE(is.na(.x))
          )
        ) %>%
        dplyr::filter(present == TRUE) %>%
        dplyr::mutate(
          file_name_full = paste0(path, latest_file)
        ) %>%
        purrr::pluck("file_name_full")
    }

    spatial_files <-
      list.files(
        path = paste0(dir, "Data/Input/Spatial"),
        full.names = TRUE,
        recursive = TRUE
      ) %>%
      stringr::str_replace(
        .,
        pattern = dir, replacement = ""
      )

    RUtilpol::zip_files(
      zipfile = paste0(refference_path, "reproducibility_bundle.zip"),
      files = c(
        "R/00_Config_file.R",
        stop_check_tables,
        harm_tables,
        spatial_files
      ),
      root = dir,
      include_directories = FALSE,
      mode = "mirror"
    )
  }

  RUtilpol::open_dir(
    dir = refference_path
  )
}
