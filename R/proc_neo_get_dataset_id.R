#' @title Get all `dataset_id` from Neotoma download
#' @param neotoma_download List of lists with Neotoma data
proc_neo_get_dataset_id <-
  function(neotoma_download) {
    neotoma_download %>%
      purrr::map("site") %>%
      purrr::map("collectionunit") %>%
      purrr::map("dataset") %>%
      purrr::map_dbl("datasetid") %>%
      as.character() %>%
      return()
  }
