#' @title Extract sample data from neotoma2 structure
#' @param data_source  List of sites (neotoma2 JSON structure)
#' @param sel_dataset_id Character with selected variable.element
#' @description Create a table with one row, that include `dataset_id`
#' and nested table with all the sample info.
#' @return Data.frame for each of the datasets with nested sample data
#' @keywords internal
proc_neo_extract_samples <-
  function(data_source,
           sel_dataset_id) {
    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    RUtilpol::check_class("data_source", "list")

    RUtilpol::check_class("sel_dataset_id", "character")

    # prepare the table with sample_id and depth
    try(
      sample_table <-
        tibble::tibble(
          sample_id = data_source %>%
            purrr::map_chr("sampleid"),
          depth = data_source %>%
            purrr::map_dbl("depth")
        ),
      silent = TRUE
    )

    # if the sample table exists and it is longer than 0 rows
    if (
      exists("sample_table", envir = current_env)
    ) {
      if (
        nrow(sample_table) > 0
      ) {
        # create a list with sample id replicated X times, where x is the number
        #   of taxa (this is needed for adding the sample id in future)
        sample_id_list <-
          data_source %>%
          purrr::set_names(nm = sample_table$sample_id) %>%
          purrr::map(
            "datum",
            .progress = "Creating a reference list"
          ) %>%
          base::summary() %>%
          as.data.frame() %>%
          tibble::as_tibble() %>%
          dplyr::filter(Var2 == "Length") %>%
          dplyr::mutate(sample_id = Var1) %>%
          dplyr::select(-c(Var1, Var2)) %>%
          dplyr::mutate(
            Freq = purrr::map(
              .x = Freq,
              .f = ~ seq(1:.x)
            )
          ) %>%
          tidyr::unnest(cols = c(Freq)) %>%
          dplyr::select(-Freq)

        # create a nested tibble with one r
        temp_data <-
          data_source %>%
          purrr::map_dfr(
            "datum",
             .progress = "Transforming records into a table") %>%
          dplyr::mutate(
            sample_id = sample_id_list$sample_id
          ) %>%
          tidyr::nest(sample_detail = dplyr::any_of(
            c(
              "taxonid", "units", "value", "element",
              "elementtype", "variablename", "taxongroup",
              "context", "ecologicalgroup"
            )
          )) %>%
          dplyr::inner_join(., sample_table, by = "sample_id") %>%
          dplyr::select(sample_id, depth, sample_detail) %>%
          dplyr::mutate(dataset_id = sel_dataset_id) %>%
          dplyr::distinct(sample_id, depth, .keep_all = TRUE) %>%
          tidyr::nest(samples = c(sample_id, depth, sample_detail))
      } else {
        temp_data <-
          tibble::tibble(
            dataset_id = sel_dataset_id,
            samples = NA
          )
      }
    } else {
      temp_data <-
        tibble::tibble(
          dataset_id = sel_dataset_id,
          samples = NA
        )
    }

    if (
      exists("sample_table", envir = current_env)
    ) {
      rm(sample_table, envir = current_env)
    }

    return(temp_data)
  }
