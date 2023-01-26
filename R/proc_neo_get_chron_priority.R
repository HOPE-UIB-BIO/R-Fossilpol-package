#' @title Select chronology table based on the preferences
#' @param data_source A list of lists with chronologies from Neotoma
#' @param dataset_name Name of the dataset
#' @param chron_order_table Data.frame with preferred order of chronology types
#' @return Data.frame with the selected chronology
#' @description Look into all chronologies, select the best one
#' by the `chron_order`
#' @keywords internal
proc_neo_get_chron_priority <-
  function(data_source,
           dataset_name,
           chron_order_table) {
    RUtilpol::check_class("data_source", "list")

    RUtilpol::check_class("dataset_name", "character")

    RUtilpol::check_class("chron_order_table", "data.frame")

    RUtilpol::check_col_names("chron_order_table", c("order", "type"))

    # extract the chronology list
    chronology_list <-
      data_source %>%
      purrr::map("chronology")

    # check if there is at least one valid chronology
    if (
      is.null(chronology_list[[1]]$chronologyid) == FALSE
    ) {

      # create table with the chronology type and id and join the chron_order
      chron_table <-
        tibble::tibble(
          type = chronology_list %>%
            purrr::map("chronology") %>%
            purrr::map_chr("modelagetype"),
          chronology_id = chronology_list %>%
            purrr::map_chr("chronologyid")
        ) %>%
        dplyr::mutate(index = dplyr::row_number()) %>%
        dplyr::left_join(
          ., chron_order_table,
          by = "type"
        )

      RUtilpol::check_col_names(
        "chron_table",
        c("type", "chronology_id")
      )

      # add names to chronology list
      names(chronology_list) <- chron_table$chronology_id

      # select the id of best chronology (defined by chron_order)
      selected_chronology_id <-
        chron_table %>%
        dplyr::filter(order == min(order)) %>%
        dplyr::filter(index == max(index)) %>%
        purrr::pluck("chronology_id")

      # save final type
      selected_type <-
        chron_table %>%
        dplyr::filter(chronology_id == selected_chronology_id) %>%
        purrr::pluck("type")

      # select the chronology
      selected_chronology <-
        chronology_list[[selected_chronology_id]]$chroncontrols

      # create a result table and nest it in dataset_id
      result_chron <-
        tibble::tibble(
          depth = purrr::map_dbl(
            .x = selected_chronology,
            .f = ~ RUtilpol::extract_var_from_list("depth", .x)
          ) %>%
            as.numeric(),
          thickness = purrr::map_dbl(
            .x = selected_chronology,
            .f = ~ RUtilpol::extract_var_from_list("thickness", .x)
          ) %>%
            as.numeric(),
          chroncontrolage = purrr::map_dbl(
            .x = selected_chronology,
            .f = ~ RUtilpol::extract_var_from_list("chroncontrolage", .x)
          ) %>%
            as.numeric(),
          agelimitolder = purrr::map_dbl(
            .x = selected_chronology,
            .f = ~ RUtilpol::extract_var_from_list("agelimitolder", .x)
          ) %>%
            as.numeric(),
          agelimityounger = purrr::map_dbl(
            .x = selected_chronology,
            .f = ~ RUtilpol::extract_var_from_list("agelimityounger", .x)
          ) %>%
            as.numeric(),
          chroncontrolid = purrr::map_chr(
            .x = selected_chronology,
            .f = ~ RUtilpol::extract_var_from_list("chroncontrolid", .x)
          ) %>%
            as.character(),
          chroncontroltype = purrr::map_chr(
            .x = selected_chronology,
            .f = ~ RUtilpol::extract_var_from_list("chroncontroltype", .x)
          ) %>%
            as.character(),
          dataset_id = dataset_name,
          age_type = selected_type
        ) %>%
        dplyr::arrange(depth) %>%
        tidyr::nest(chron_control = c(
          depth, thickness, chroncontrolage,
          agelimitolder, agelimityounger, chroncontrolid,
          chroncontroltype
        ))
    } else {
      result_chron <-
        tibble::tibble(
          dataset_id = dataset_name,
          chron_control = NA
        )
    }

    return(result_chron)
  }
