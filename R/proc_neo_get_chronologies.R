#' @title Obtain chronology control tables from Neotoma download
#' @param neotoma_download List of lists with Neotoma data
#' @param chron_order Data.frame with preferred order of chronology types
#' @param min_n_of_control_points Minimal number of chronology control points in
#' selected chronology table
#' @export
proc_neo_get_chronologies <-
  function(neotoma_download, chron_order, min_n_of_control_points) {
    RUtilpol::check_class("neotoma_download", "list")

    RUtilpol::check_class("chron_order", "data.frame")

    RUtilpol::check_col_names("chron_order", c("order", "type"))

    RUtilpol::check_class("min_n_of_control_points", "numeric")

    # extract chronologies for all records
    chronologies <-
      neotoma_download %>%
      purrr::map("site") %>%
      purrr::map("collectionunit") %>%
      purrr::map("chronologies")

    # get relevant chronologies based on the chron_order
    # !!! this will take a while !!!
    chroncontrol_tables <-
      purrr::map(
        .progress = "get relevant chronologies based on the chron_order",
        .x = chronologies,
        .y = names(chronologies),
        .f = ~ proc_neo_get_chron_priority(
          data_source = .x,
          dataset_name = .y,
          chron_order_table = chron_order
        )
      ) %>%
      purrr::list_rbind()

    # drop datasets without chronologies and add n_chron_control
    chroncontrol_tables_clean <-
      chroncontrol_tables %>%
      # filter out records without chronologies
      dplyr::filter(!purrr::map_lgl(chron_control, is.null)) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(
        n_chron_control = purrr::map_dbl(chron_control, nrow)
      ) %>%
      dplyr::filter(n_chron_control >= min_n_of_control_points)

    RUtilpol::check_col_names("chroncontrol_tables_clean", "n_chron_control")

    util_check_data_table(chroncontrol_tables_clean)

    return(chroncontrol_tables_clean)
  }
