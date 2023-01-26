#' @title Obtain meta information from Neotoma download
#' @param neotoma_download List of lists with Neotoma data
#' @return Data.frame with meta information such as site names, coordinates, doi, etc
#' @keywords internal
proc_neo_get_metadata <-
  function(neotoma_download) {
    RUtilpol::check_class("neotoma_download", "list")

    RUtilpol::output_heading(
      msg = "Extracting Neotoma meta information"
    )

    # get all dataset ids
    datasets_ids <-
      proc_neo_get_dataset_id(neotoma_download)

    # get all site data
    neotoma_download_sites <-
      proc_neo_get_sites(neotoma_download)

    RUtilpol::stop_if_not(
      length(datasets_ids) == length(neotoma_download_sites),
      true_msg = "All sites prepared",
      false_msg = "There is different number of sites than dataset IDs."
    )

    # extract metadata into a data.frame
    neotoma_sites_site_data <-
      tibble::tibble(

        # dataset id
        dataset_id = datasets_ids,

        # site id
        siteid = neotoma_download_sites %>%
          purrr::map_chr(
            .f = ~ RUtilpol::extract_var_from_list("siteid", .x)
          ),

        # site name
        sitename = neotoma_download_sites %>%
          purrr::map_chr(
            .f = ~ RUtilpol::extract_var_from_list("sitename", .x)
          ),

        # collection handle
        handle = neotoma_download_sites %>%
          purrr::map("collectionunit") %>%
          purrr::map_chr(
            .f = ~ RUtilpol::extract_var_from_list("handle", .x)
          ),

        # full coordinates
        coord = neotoma_download_sites %>%
          purrr::map_chr(
            .f = ~ RUtilpol::extract_var_from_list("geography", .x)
          ) %>%
          stringr::str_replace(., ".*\\[", "") %>%
          stringr::str_replace(., "\\].*", ""),

        # latitude
        lat = stringr::str_replace(coord, ".*\\,", "") %>%
          as.double(),

        # longitude
        long = stringr::str_replace(coord, "\\,.*", "") %>%
          as.double(),

        # altitude
        altitude = neotoma_download_sites %>%
          purrr::map_dbl(
            .f = ~ RUtilpol::extract_var_from_list("altitude", .x)
          ),

        # depositional environment
        depositionalenvironment = neotoma_download_sites %>%
          purrr::map("collectionunit") %>%
          purrr::map_chr(
            .f = ~ RUtilpol::extract_var_from_list("depositionalenvironment", .x)
          ),
        doi = neotoma_download_sites %>%
          purrr::map("collectionunit") %>%
          purrr::map("dataset") %>%
          purrr::map_chr(
            .f = ~ RUtilpol::extract_var_from_list("doi", .x) %>%
              unlist()
          )
      ) %>%
      dplyr::select(!dplyr::any_of("coord")) # do not include full coords

    return(neotoma_sites_site_data)
  }
