#' @title Get all harmonisation tables for regions present
#' @param data_source Data.frame with `harmonisation_region` and `raw_counts`
#' @param dir Path to the data storage folder
#' @export
harm_get_all_tables <-
  function(data_source,
           dir) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names(
      "data_source",
      c(
        "harmonisation_region",
        "raw_counts"
      )
    )

    RUtilpol::check_class("dir", "character")

    # create table with all harmonisation tables
    #   If there is a table missing, create the table 'de novo'
    harmonisation_tables <-
      tibble::tibble(
        harmonisation_region = data_source$harmonisation_region %>%
          unique() %>%
          sort()
      ) %>%
      dplyr::mutate(
        harm_table = purrr::map(
          .x = harmonisation_region,
          .f = ~ {

            # subset data for the specific region
            data_sub <-
              data_source %>%
              dplyr::filter(harmonisation_region == .x) %>%
              dplyr::select(raw_counts)

            # load/create harm table
            stopcheck_table(
              data_source = data_sub,
              file_name = .x,
              dir = paste0(
                dir, "/Data/Input/Harmonisation_tables/"
              ),
              sel_method = "harm_table",
              msg = "Harmonise taxa to the same taxonomic level (column 'level_1').",
              stop_session = FALSE
            )
          }
        )
      )

    RUtilpol::check_col_names(
      "harmonisation_tables",
      c("harmonisation_region", "harm_table")
    )

    #  check if all tables are dataframes (loaded properly)
    if (
      purrr::map_lgl(
        .x = harmonisation_tables$harm_table,
        .f = ~ !is.data.frame(.x)
      ) %>%
        any()
    ) {
      RUtilpol::stop_quietly()
    }

    RUtilpol::output_comment(
      "All harmonisation tables are loaded properly"
    )

    return(harmonisation_tables)
  }
