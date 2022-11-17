#' @title Harmonise the taxa to a certain taxonomic level
#' @param data_source Table containing the raw pollen count
#' @param harmonisation_table Table including original names of taxa
#' (`original_name`) as well as harmonised (`harm_name`)
#' @param original_name Character. Name of the column in harmonisation table
#' of original counts
#' @param harm_name Character. Name of the column in harmonisation table
#' to be used.
#' @param exclude_taxa Character. Name of the taxa, which should be omitted.
#' @param pollen_grain_test Logical. Test for difference in sum of total
#' pollen sum before and after harmonisation.
#' @return Data.frame with harmonised counts
#' @export
harmonise_taxa <-
  function(data_source,
           harmonisation_table,
           original_name = "taxon_name",
           harm_name = "level_1",
           exclude_taxa = "delete",
           pollen_grain_test = TRUE) {

    # safety tests
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "sample_id")

    RUtilpol::check_col_names("data_source", "sample_id")

    RUtilpol::check_class("harmonisation_table", "data.frame")

    RUtilpol::check_class("original_name", "character")

    RUtilpol::check_class("harm_name", "character")

    RUtilpol::check_class("exclude_taxa", "character")

    RUtilpol::check_class("pollen_grain_test", "logical")

    assertthat::assert_that(
      any(names(harmonisation_table) %in% original_name),
      msg = "'harmonisation_table' does not contain selected 'original_name'"
    )

    assertthat::assert_that(
      any(names(harmonisation_table) %in% harm_name),
      msg = "'harmonisation_table' does not contain selected 'harm_name'"
    )

    # prepare the harmonisation table
    harmonisation_table_prepared <-
      harmonisation_table %>%
      dplyr::rename(original_name = eval(original_name)) %>%
      dplyr::select(
        dplyr::all_of(
          c("original_name", harm_name)
        )
      )

    # extract all taxon names from harmonisation table
    taxon_names_table <-
      harmonisation_table_prepared$original_name

    # extract all taxon names in data
    taxon_names_data <-
      data_source %>%
      dplyr::select(!dplyr::any_of(c("sample_id"))) %>%
      names()

    # extract missing taxa
    missing_taxa <-
      taxon_names_data[!taxon_names_data %in% taxon_names_table]

    # check if all taxa are present
    assertthat::assert_that(
      all(taxon_names_data %in% taxon_names_table),
      msg = paste(
        "The following taxa are present in the 'data_source'",
        "but not in the 'harmonisation_table':",
        RUtilpol::paste_as_vector(missing_taxa)
      )
    )

    # computation
    counts <-
      data_source %>%
      # replace all missing values with zeros
      dplyr::mutate(
        dplyr::across(
          !dplyr::any_of("sample_id"),
          ~ tidyr::replace_na(., 0)
        )
      ) %>%
      # turn into long format
      tidyr::pivot_longer(
        cols = -sample_id,
        names_to = "original_name",
        values_to = "counts"
      ) %>%
      # link with the harmonisation table
      dplyr::left_join(
        harmonisation_table_prepared,
        by = "original_name"
      ) %>%
      # group by `harm_name`
      dplyr::group_by(sample_id, get(harm_name)) %>%
      # sum all taxa in the same group
      dplyr::summarise(
        .groups = "keep",
        counts = sum(counts)
      ) %>%
      # rename the column
      dplyr::rename(harmonised_counts = `get(harm_name)`) %>%
      # turn back to the wide format
      tidyr::pivot_wider(
        names_from = "harmonised_counts",
        values_from = "counts"
      ) %>%
      dplyr::ungroup() %>%
      # hide
      tibble::column_to_rownames("sample_id") %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(), ~ ifelse(.x < 0, 0, .x)
        )
      ) %>%
      subset(., select = (colSums(.) != 0)) %>%
      tibble::rownames_to_column("sample_id") %>%
      tibble::as_tibble()

    RUtilpol::check_col_names("counts", "sample_id")

    # remove the "exclude" taxa
    counts_without_deleted <-
      counts %>%
      dplyr::select(!dplyr::contains(exclude_taxa))

    # remove the un-harmonized taxa
    counts_without_NA <-
      counts_without_deleted %>%
      dplyr::select(!dplyr::any_of(c("NA")))


    # pollen sum test
    if (
      pollen_grain_test == TRUE
    ) {

      # helper function to count pollen grains
      count_pollen <-
        function(x) {
          current_frame <- sys.nframe()
          current_env <- sys.frame(which = current_frame)

          # try to calculate the pollen sums
          try(
            n_pollen <-
              sum(x %>%
                dplyr::select(!dplyr::any_of(c("sample_id"))),
              na.rm = TRUE
              ) %>%
              round(),
            silent = TRUE
          )

          # replace with NA if not successful
          if (
            !exists("n_pollen", envir = current_env)
          ) {
            n_pollen <- NA
          }

          return(n_pollen)
        }

      n_pollen_original <-
        count_pollen(data_source)

      n_pollen_harm <-
        count_pollen(counts)

      n_pollen_harm_delete <-
        count_pollen(counts_without_deleted)

      n_pollen_harm_NA <-
        count_pollen(counts_without_NA)


      # if any of the pollen sum was not successful
      if (
        any(
          is.na(
            c(n_pollen_original, n_pollen_harm)
          )
        )) {
        usethis::ui_oops(
          " - Cannot calculate the pollen sums"
        )

        # If there is not same number of pollen grains
      } else if (
        n_pollen_original != n_pollen_harm
      ) {
        usethis::ui_warn(
          x = paste0(
            " - ",
            round(n_pollen_original - n_pollen_harm),
            " (",
            round(((n_pollen_original - n_pollen_harm) / n_pollen_original) * 100, 2),
            "%) grains lost during harmonisation"
          )
        )
      }

      # Differences between final number of grains and deleted using
      #  `exclude_taxa`
      if (
        all(
          !is.na(
            c(n_pollen_harm_delete, n_pollen_harm_NA)
          )
        )) {
        if (
          n_pollen_harm_delete != n_pollen_harm_NA
        ) {
          usethis::ui_info(
            x = paste0(
              " - ",
              round(n_pollen_harm_delete - n_pollen_harm_NA),
              " (",
              round(((n_pollen_harm_delete - n_pollen_harm_NA) / n_pollen_harm_delete) * 100, 2),
              "%) grains removed as non-harmonised."
            )
          )
        }
      }

      # Differences between final number of grains and deleted using
      #  `exclude_taxa`
      if (
        all(
          !is.na(
            c(n_pollen_harm_delete, n_pollen_harm)
          )
        )) {
        if (
          n_pollen_harm_delete != n_pollen_harm
        ) {
          usethis::ui_info(
            x = paste0(
              " - ",
              round(n_pollen_harm - n_pollen_harm_delete),
              " (",
              round(((n_pollen_harm - n_pollen_harm_delete) / n_pollen_harm) * 100, 2),
              "%) grains deleted marked as ",
              RUtilpol::paste_as_vector(exclude_taxa)
            )
          )
        }
      }
    }

    return(counts_without_NA)
  }
