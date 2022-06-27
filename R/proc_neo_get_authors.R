#' @title Obtain information about authors of datasets
#' @param neotoma_download List of lists with Neotoma data
#' @param dataset_ids List of all dataset_id to extract contact information
#' @param dir Path to the data storage folder
#' @param download_new Logical. Download the full info about authors? Time
#' consuming
#' @export
proc_neo_get_authors <-
  function(neotoma_download, dataset_ids, dir, download_new = TRUE) {
    util_check_class("neotoma_download", "list")

    util_check_class("dataset_ids", "character")

    util_check_class("download_new", "logical")

    util_check_class("dir", "character")

    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    util_output_message(
      msg = "Extracting information about dataset authors"
    )

    # get all site data
    neotoma_download_sites <-
      proc_neo_get_sites(neotoma_download)

    # extract the PI information
    neotoma_sites_meta_pi <-
      purrr::map_dfr(
        .x = dataset_ids,
        .f = purrr::possibly(
          .f = ~ {

            # save the dataset_id
            sel_dataset_id <- .x

            # obtain information about authors
            author_info <-
              neotoma_download_sites[as.character(sel_dataset_id)] %>%
              purrr::map("collectionunit") %>%
              purrr::map("dataset") %>%
              purrr::map("datasetpi") %>%
              purrr::pluck(1)

            # extract number of authors
            n_authors <-
              length(author_info)

            # extract info about each author
            res_table <-
              purrr::map_dfr(
                .x = 1:n_authors,
                .f = ~
                  tibble::tibble(
                    dataset_id = sel_dataset_id,
                    author_id = paste0(author_info[[.x]]$contactid, "_neotoma")
                  ) %>%
                    return()
              )

            return(res_table)
          },
          # create an empty row for author information
          otherwise = ~ tibble::tibble(
            dataset_id = .x,
            author_id = NA
          )
        )
      )


    author_data_path <-
      paste0(
        dir, "/Data/Input/Author_info"
      )

    # Check the presence of previous data
    download_confirm <- download_new

    if (
      download_confirm == TRUE
    ) {
      download_confirm <-
        util_confirm_based_on_presence(
          file_name = "neotoma_author_data",
          dir = author_data_path,
          msg = "Detected previous download of Author information, do you want to re-do it?"
        )
    }

    # if download
    if (
      download_confirm == TRUE
    ) {
      # get all author ids
      all_author_ids <-
        unique(neotoma_sites_meta_pi$author_id) %>%
        stringr::str_replace(., "_neotoma", "") %>%
        as.numeric() %>%
        sort()

      util_output_comment(
        msg = "Start to download Neotoma author data"
      )

      # download all into about authors
      neotoma_author_data <-
        purrr::map_dfr(
          .x = seq_along(all_author_ids),
          .f = ~ {

            # output progress
            cat(paste0(.x, " in ", length(all_author_ids), "\n"))

            res <-
              httr::GET(
                paste0("https://api.neotomadb.org/v2.0/data/contacts/", all_author_ids[.x])
              )

            if (
              res$status_code == 200
            ) {
              author_info <-
                httr::content(res)$data %>%
                purrr::pluck(1)

              tibble::tibble(
                author_id = paste0(all_author_ids[.x], "_neotoma"),
                first_name = util_replace_null_with_na(author_info$givennames),
                last_name = util_replace_null_with_na(author_info$familyname),
                email = util_replace_null_with_na(author_info$email),
                Department = util_replace_null_with_na(author_info$address),
                url = util_replace_null_with_na(author_info$url)
              ) %>%
                return()
            } else {
              tibble::tibble(
                author_id = paste0(author_info$contactid, "_neotoma"),
                first_name = NA,
                last_name = NA,
                email = NA,
                adress = NA,
                url = NA
              ) %>%
                return()
            }
          }
        )
    } else {
      suppressWarnings(
        try(
          neotoma_author_data <-
            util_load_latest_file(
              file_name = "neotoma_author_data",
              dir = author_data_path
            ),
          silent = TRUE
        )
      )
    }


    if (
      exists("neotoma_author_data", envir = current_env)
    ) {
      util_save_if_latests(
        file_name = "neotoma_author_data",
        dir = author_data_path
      )

      neotoma_author_info <-
        neotoma_sites_meta_pi %>%
        dplyr::left_join(
          neotoma_author_data,
          by = "author_id"
        )
    } else {
      neotoma_author_info <- neotoma_sites_meta_pi
    }

    util_stop_if_not(
      nrow(neotoma_author_info) > 0,
      false_msg = "No authors information were extracted",
      true_msg = paste(
        "The information about",
        neotoma_author_info %>%
          dplyr::distinct(author_id) %>%
          nrow(),
        "individual authors were extracted"
      )
    )

    return(neotoma_author_info)
  }
