#' @title Download all records from Neotoma
#' @param allds Data.frame with all dataset IDs
#' @param dir Path to the data storage folder
#' @param n_tries Number of tries to download each dataset
#' @return List with all downloaded records
#' @export
proc_neo_download_records <- function(allds,
                                      dir,
                                      n_tries = 10) {
  RUtilpol::check_class("allds", "data.frame")

  RUtilpol::check_col_names("allds", "dsid")

  RUtilpol::check_class("n_tries", "numeric")

  # make a vector with all se dataset_ids
  ds_vector <-
    allds %>%
    purrr::pluck("dsid") %>%
    sort() %>%
    as.character()

  dir <- RUtilpol::add_slash_to_path(dir)

  path_to_indiv_folder <-
    paste0(dir, "individual/")

  if (
    isFALSE(
      any(
        "individual" %in% list.files(dir)
      )
    )
  ) {
    dir.create(
      path_to_indiv_folder
    )
  }

  # get all missing names
  ds_absent <-
    util_get_missing_ds_names(
      dir = path_to_indiv_folder,
      name_vector = ds_vector
    )

  n_ds_to_download <- length(ds_absent)

  # if all records are downloaded
  if (
    n_ds_to_download < 1
  ) {
    usethis::ui_done("All selected records are already downloaded")
  }

  if (
    n_ds_to_download > 0
  ) {
    RUtilpol::output_comment(
      paste(
        n_ds_to_download,
        "record(s) will be downloaded"
      )
    )

    # path to the Neotoma API
    rawdownload <-
      "https://api.neotomadb.org/v2.0/data/downloads"


    # download all records individually
    purrr::walk(
      .progress = "Downloading individual records",
      .x = 1:n_ds_to_download,
      .f = ~ {
        sel_ds_name <- ds_absent[.x]

        # repeat for 'n_tries' or until successfully download the record
        for (i in 1:n_tries) {
          current_frame <- sys.nframe()
          current_env <- sys.frame(which = current_frame)

          # download the record
          res <-
            httr::GET(
              paste0(rawdownload, "/", sel_ds_name)
            )

          # if it was successful download
          if (
            res$status_code == 200 # NOTE: status_code 200 = success
          ) {
            # extract the data
            output <-
              httr::content(res)$data[[1]]

            RUtilpol::save_latest_file(
              object_to_save = output,
              file_name = sel_ds_name,
              dir = path_to_indiv_folder,
              prefered_format = "rds",
              use_sha = TRUE,
              verbose = FALSE
            )

            # break from loop
            break
          } else {
            # save output as nothing
            output <- NULL
          }

          # delete the result and output
          rm(res, output, envir = current_env)
        }
      }
    )

    # check again if all are downloaded
    # extract the records which were not successfully downloaded
    cannot_download <-
      util_get_missing_ds_names(
        dir = path_to_indiv_folder,
        name_vector = ds_vector
      )

    if (
      length(cannot_download) == 0
    ) {
      usethis::ui_done("All selected records were downloaded")
    } else {
      usethis::ui_oops(
        paste(
          length(cannot_download), "out of", nrow(allds),
          "records were NOT downloaded.", "\n",
          "Specifically, dataset IDs:",
          RUtilpol::paste_as_vector(cannot_download)
        )
      )
    }
  }

  ds_present <-
    list.files(
      path_to_indiv_folder
    ) %>%
    RUtilpol::get_clean_name()

  result_list <-
    subset(
      ds_present,
      ds_present %in% ds_vector
    ) %>%
    rlang::set_names() %>%
    purrr::map(
      .progress = "Compiling records together",
      .f = ~ RUtilpol::get_latest_file(
        file_name = .x,
        dir = path_to_indiv_folder,
        verbose = FALSE
      )
    )

  # save the final list as 'neotoma_download' object filtering out
  # unsuccessfull records
  neotoma_download <-
    result_list[!purrr::map_lgl(result_list, is.null)]

  return(neotoma_download)
}
