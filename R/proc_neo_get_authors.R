#' @title Obtain information about authors of datasets
#' @param neotoma_download List of lists with Neotoma data
#' @param dataset_ids List of all dataset_id to extract contact information
#' @param dir Path to the data storage folder
#' @param download_new Logical. Download the full info about authors?
#' @export
proc_neo_get_authors <-
  function(neotoma_download, dataset_ids, dir, download_new = TRUE) {
    RUtilpol::check_class("neotoma_download", "list")

    RUtilpol::check_class("dataset_ids", "character")

    RUtilpol::check_class("download_new", "logical")

    RUtilpol::check_class("dir", "character")

    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)

    dir <- RUtilpol::add_slash_to_path(dir)

    RUtilpol::output_heading(
      msg = "Extracting information about dataset authors"
    )

    # get all site data
    neotoma_download_sites <-
      RFossilpol:::proc_neo_get_sites(neotoma_download)

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
        dir, "Data/Input/Author_info/"
      )

    author_individual_path <-
      paste0(author_data_path, "individual/")

    if (
      isFALSE(
        any(
          "individual" %in% list.files(author_data_path)
        )
      )
    ) {
      dir.create(
        author_individual_path
      )
    }

    # if download
    if (
      isTRUE(download_new)
    ) {
      # get all author ids
      all_author_ids <-
        unique(neotoma_sites_meta_pi$author_id) %>%
        sort()

      # get all missing names
      author_absent <-
        RFossilpol:::util_get_missing_ds_names(
          dir = author_individual_path,
          name_vector = all_author_ids
        )

      if (
        length(author_absent) > 0
      ) {
        RUtilpol::output_comment(
          msg = "Start to download Neotoma author data"
        )

        # download all into about authors
        purrr::walk(
          .x = seq_along(author_absent),
          .f = ~ {

            # output progress
            cat(paste0(.x, " in ", length(author_absent), "\n"))

            res <-
              httr::GET(
                paste0(
                  "https://api.neotomadb.org/v2.0/data/contacts/",
                  author_absent[.x]
                )
              )

            if (
              res$status_code == 200
            ) {
              author_info <-
                httr::content(res)$data %>%
                purrr::pluck(1)

              author_info_table <-
                tibble::tibble(
                  author_id = author_absent[.x],
                  first_name = RUtilpol::replace_null_with_na(
                    author_info$givennames
                  ),
                  last_name = RUtilpol::replace_null_with_na(
                    author_info$familyname
                  ),
                  email = RUtilpol::replace_null_with_na(
                    author_info$email
                  ),
                  Department = RUtilpol::replace_null_with_na(
                    author_info$address
                  ),
                  url = RUtilpol::replace_null_with_na(
                    author_info$url
                  )
                )

              RUtilpol::save_latest_file(
                object_to_save = author_info_table,
                file_name = author_info_table$author_id,
                dir = author_individual_path,
                prefered_format = "rds",
                use_sha = TRUE,
                verbose = FALSE
              )

              rm(author_info, author_info_table)
            }
          }
        )
      }
    } else {
      suppressWarnings(
        try(
          neotoma_author_data <-
            RUtilpol::get_latest_file(
              file_name = "neotoma_author_data",
              dir = author_data_path
            ),
          silent = TRUE
        )
      )
    }

    if (
      isFALSE(exists("neotoma_author_data", envir = current_env))
    ) {
      # load all dataset
      neotoma_author_data <-
        purrr::map_dfr(
          .x = all_author_ids,
          .f = purrr::possibly(
            .f = ~ RUtilpol::get_latest_file(
              file_name = .x,
              dir = author_individual_path,
              verbose = FALSE
            )
          )
        )

      # save if needed
      RUtilpol::save_latest_file(
        object_to_save = neotoma_author_data,
        dir = author_data_path,
        prefered_format = "rds",
        use_sha = TRUE
      )
    }

    neotoma_author_info <-
      neotoma_sites_meta_pi %>%
      dplyr::left_join(
        neotoma_author_data,
        by = "author_id"
      )


    RUtilpol::stop_if_not(
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
