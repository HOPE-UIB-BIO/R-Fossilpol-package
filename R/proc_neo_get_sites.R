#' @title Obtain sites information from Neotoma download
#' @param neotoma_download List of lists with Neotoma data
#' @return List of lists with all site information
#' @keywords internal
proc_neo_get_sites <-
  function(neotoma_download) {
    RUtilpol::check_class("neotoma_download", "list")

    neotoma_download %>%
      purrr::map("site") %>%
      return()
  }
