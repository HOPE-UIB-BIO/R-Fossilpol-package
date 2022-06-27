#' @title Detect potential chronology control points saved as Percentage
#' modern carbon and let user decide, which should be back-transformed
#' @param data_source Data.frame with `dataset_id`, `chron_control`, and
#' `chron_control_format`
#' @param chron_control_types Named list with `chroncontrol_included_types` and
#' `radiocarbon_control_types`
#' @param dir Path to the data storage folder
chron_fix_pmc <-
  function(data_source, chron_control_types, dir) {
    util_check_class("data_source", "data.frame")

    util_check_col_names(
      "data_source",
      c(
        "dataset_id",
        "chron_control",
        "chron_control_format"
      )
    )

    util_check_class("chron_control_types", "list")

    util_check_class("dir", "character")

    # detected datasets should be adjusted
    potential_pmc_test <-
      data_source %>%
      dplyr::mutate(
        potential_need_to_correct = purrr::map_lgl(
          # need to use non-formated chron as it does not have rounded ages
          .x = chron_control,
          .f = ~ chron_decimal_ages_detect(
            data_source = .x,
            rc_control_types = chron_control_types$radiocarbon_control_types
          )
        )
      )

    util_check_col_names("potential_pmc_test", "potential_need_to_correct")

    # if there is any potential chron control point to have percentage modern carbon
    if (
      any(potential_pmc_test$potential_need_to_correct)
    ) {
      potential_pmc_data <-
        potential_pmc_test %>%
        # dplyr::filter(potential_need_to_correct == TRUE) %>%
        dplyr::mutate(
          #  use only points which are in formatted but with values from unformatted
          chron_control_merge = purrr::map2(
            .x = chron_control_format,
            .y = chron_control,
            .f = ~ .x %>%
              dplyr::distinct(chroncontrolid) %>%
              dplyr::inner_join(.y, by = "chroncontrolid")
          ),
          # mark those points which are potential percentage radiocarbon
          pmc_marked = purrr::map(
            .x = chron_control_merge,
            .f = ~ chron_decimal_ages_mark(
              data_source = .x,
              rc_control_types = chron_control_types$radiocarbon_control_types
            )
          )
        ) %>%
        dplyr::select(dataset_id, pmc_marked) %>%
        tidyr::unnest(pmc_marked) %>%
        # keep only those which are marked as potential percentage radiocarbon
        dplyr::filter(fulfil_criteria == TRUE)

      # if there are some potential percentage radiocarbon
      if (
        nrow(potential_pmc_data) > 0
      ) {
        potential_pmc_path <-
          paste0(
            dir, "/Data/Input/Chronology_setting/Percentage_radiocarbon/"
          )

        # load/create table with chronology control points which are potential
        #   percentage carbon
        potential_pmc <-
          stopcheck_table(
            data_source = potential_pmc_data,
            file_name = "potential_pmc",
            dir = potential_pmc_path,
            sel_method = "default",
            msg = paste(
              "Select which chron control point should be",
              "back-transformed from percentages ('include' = TRUE/FALSE).",
              "This can be decided based on decimal places in 'chroncontrolage'",
              "column with values around 100 and some type of radiocarbon",
              "('chroncontroltype' column)."
            )
          )

        # subset unique values of dataset_id and chroncontrolid for those,
        #   which are marked as to fix
        pmc_to_backtransform <-
          potential_pmc %>%
          dplyr::filter(include == TRUE) %>%
          dplyr::distinct(dataset_id, chroncontrolid)

        util_output_comment(
          msg = paste(
            "There has been", nrow(potential_pmc), "selected",
            "chronology control points to fix pmc:",
            util_paste_as_vector(pmc_to_backtransform$chroncontrolid)
          )
        )

        # add new variable 'need_to_correct' which is present for those point,
        #   which are confirmed by user to back-transform
        potential_pmc_marked <-
          potential_pmc_test %>%
          dplyr::mutate(
            need_to_correct = purrr::pmap_lgl(
              .l = list(potential_need_to_correct, dataset_id, chron_control_format),
              .f = ~ ifelse(
                ..1 == TRUE &
                  ..2 %in% pmc_to_backtransform$dataset_id,
                ifelse(
                  nrow(
                    ..3 %>%
                      dplyr::filter(
                        chroncontrolid %in%
                          pmc_to_backtransform$chroncontrolid
                      )
                  ) > 0,
                  TRUE,
                  FALSE
                ),
                FALSE
              )
            )
          )

        util_check_col_names("potential_pmc_marked", "need_to_correct")

        # back-transform  points which are marked as percentage carbon
        potential_pmc_fixed <-
          potential_pmc_marked %>%
          dplyr::mutate(
            chron_control_format = ifelse(
              need_to_correct == TRUE,
              purrr::map(
                .x = chron_control_format,
                .f = ~ chron_change_pmc_to_ages(
                  data_source = .x
                )
              ),
              chron_control_format
            )
          )

        util_check_col_names("potential_pmc_fixed", "chron_control_format")
      }
    } else {
      potential_pmc_fixed <-
        potential_pmc_test
    }

    potential_pmc_fixed %>%
      dplyr::select(
        !dplyr::any_of(
          c("need_to_correct", "potential_need_to_correct")
        )
      ) %>%
      return()
  }
