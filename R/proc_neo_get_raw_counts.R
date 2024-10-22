#' @title Obtain the counts and filter them by ecological groups
#' @param data_source Data.frame with samples saved as nested information
#' @param sel_var_element Name of the element to selected the raw counts (default
#' is 'pollen')
#' @export
proc_neo_get_raw_counts <-
  function(data_source, sel_var_element = "pollen") {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "samples")

    RUtilpol::check_class("sel_var_element", "character")

    # get ecological groups
    eco_group_data <-
      proc_neo_get_samples_eco_group(data_source)

    RUtilpol::output_comment(
      msg = "Checking ecological groups"
    )

    eco_group_path <-
      paste0(
        data_storage_path, "/Data/Input/Eco_group/"
      )

    # load/create table with ecological groups
    eco_group <-
      stopcheck_table(
        data_source = eco_group_data,
        file_name = "eco_group",
        dir = eco_group_path,
        sel_method = "default",
        msg = "Please select which types of ecological groups should be kept ('include' = TRUE/FALSE)"
      )

    # extract only those flagged as to include
    sel_eco_group <-
      eco_group %>%
      dplyr::filter(include == TRUE) %>%
      purrr::pluck("ecologicalgroup")

    RUtilpol::open_dir_if_not(
      length(sel_eco_group) > 0,
      dir = eco_group_path,
      msg = paste(
        "None of the ecological groups present in the data has been",
        "selected to be included.",
        paste0(
          "Please open file 'eco_group' in folder ",
          eco_group_path, ".",
          " Then proceed and re-run this whole script"
        )
      )
    )

    RUtilpol::output_comment(
      msg = paste(
        "There has been", length(sel_eco_group), "selected",
        "ecological groups:",
        RUtilpol::paste_as_vector(sel_eco_group)
      )
    )

    # extract raw counts
    # !!! this takes time !!!
    sample_counts <-
      data_source %>%
      dplyr::mutate(
        raw_counts = purrr::map(
          .progress = "Extracting raw counts",
          .x = samples,
          .f = ~ proc_neo_extract_counts(
            data_source = .x,
            sel_var_element = sel_var_element,
            sel_eco_group = sel_eco_group
          )
        )
      ) %>%
      dplyr::mutate(
        # calculate number of levels with counts
        n_sample_counts = purrr::map_dbl(raw_counts, nrow)
      )

    util_check_data_table(sample_counts)

    RUtilpol::check_col_names("sample_counts", c("raw_counts", "n_sample_counts"))

    return(sample_counts)
  }
