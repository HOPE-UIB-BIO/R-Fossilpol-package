#' @title Clean names for count datasets
#' @param data_source Data.frame containing `raw_counts` column which contains
#' raw counts
#' @param additional_patterns User defined patterns to be used for name changes
#' @param dir Path to the data_storage folder
#' @export
proc_clean_count_names <-
  function(data_source,
           additional_patterns = NULL,
           dir) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "raw_counts")

    RUtilpol::check_class("additional_patterns", c("NULL", "character"))

    RUtilpol::check_class("dir", "character")

    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    dir <- RUtilpol::add_slash_to_path(dir)

    RUtilpol::output_heading(
      msg = "Saving taxa reference table"
    )

    taxa_ref_table <-
      data_source %>%
      purrr::pluck("raw_counts") %>%
      purrr::map(
        .f = ~ names(.x)
      ) %>%
      unlist() %>%
      unique() %>%
      sort() %>%
      tibble::enframe() %>%
      rlang::set_names(
        nm = c("id", "neotoma_names")
      ) %>%
      dplyr::mutate(
        taxon_name = proc_clean_names(
          data_source = neotoma_names,
          additional_patterns = additional_patterns
        )
      ) %>%
      dplyr::select(-id)

    RUtilpol::save_latest_file(
      object_to_save = taxa_ref_table,
      file_name = "taxa_reference_table",
      dir = paste0(
        dir, "Data/Input/Harmonisation_tables/"
      ),
      prefered_format = "csv",
      verbose = FALSE
    )

    data_clean_names <-
      data_full_filtered %>%
      dplyr::mutate(
        raw_counts = purrr::map(
          .progress = "Cleaning taxon names",
          .x = raw_counts,
          .f = ~ proc_clean_column_names(
            data_source = .x,
            additional_patterns = additional_patterns
          )
        )
      )

    RUtilpol::check_if_loaded(
      file_name = "data_clean_names",
      env = current_env
    )

    RUtilpol::check_class("data_clean_names", "data.frame")

    RUtilpol::check_col_names("data_clean_names", "raw_counts")

    return(data_clean_names)
  }
