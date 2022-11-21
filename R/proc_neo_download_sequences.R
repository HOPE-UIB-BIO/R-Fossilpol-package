#' @title Download all sequences from Neotoma
#' @param allds Data.frame with all dataset IDs
#' @param dir Path to the data storage folder
#' @param n_tries Number of tries to download each dataset
#' @return List with all downloaded sequences
#' @export
proc_neo_download_sequences <- function(allds,
                                        dir,
                                        n_tries = 10) {
  RUtilpol::check_class("allds", "data.frame")

  RUtilpol::check_col_names("allds", "dsid")

  RUtilpol::check_class("n_tries", "numeric")

  # helper functions
  get_clean_name <- function(data_source) {

    # detect dates
    data_source_date_apend <-
      paste0(
        ".",
        RUtilpol:::get_date_from_name(data_source),
        ".*"
      )

    # remove dates
    data_source_striped <-
      stringr::str_replace(
        string = data_source,
        pattern = data_source_date_apend,
        ""
      )

    return(data_source_striped)
  }

  get_missing_names <- function(path_to_folder, name_vector) {

    # pre-alocate that all sequences are missing
    ds_absent <- name_vector

    # check which sequences are present
    ds_present <-
      list.files(
        path_to_folder
      )

    if (
      length(ds_present) > 0
    ) {
      ds_present_striped <-
        get_clean_name(ds_present)

      # select data NOT downloaded
      ds_absent <-
        name_vector[!name_vector %in% ds_present_striped]
    }

    return(ds_absent)
  }

  # make a vector with all se dataset_ids
  ds_vector <-
    allds %>%
    purrr::pluck("dsid") %>%
    sort() %>%
    as.character()

  path_to_indiv_folder <-
    paste0(dir, "/individual/")

  # get all missing names
  ds_absent <-
    get_missing_names(
      path_to_folder = path_to_indiv_folder,
      name_vector = ds_vector
    )

  n_seq_to_download <- length(ds_absent)

  # if all sequences are downloaded
  if (
    n_seq_to_download < 1
  ) {
    usethis::ui_done("All selected sequences are downloaded")
    return()
  }

  RUtilpol::output_comment(
    paste(
      n_seq_to_download,
      "sequence(s) will be downloaded"
    )
  )

  # path to the Neotoma API
  rawdownload <-
    "https://api.neotomadb.org/v2.0/data/downloads"


  # download all sequences individually
  purrr::walk(
    .x = 1:n_seq_to_download,
    .f = ~ {
      sel_seq_name <- ds_absent[.x]

      # output progress
      cat(
        paste(
          "downloading", .x, "of", n_seq_to_download
        )
      )

      # repeat for 'n_tries' or until successfully download the sequence
      for (i in 1:n_tries) {
        current_frame <- sys.nframe()
        current_env <- sys.frame(which = current_frame)

        # download the sequence
        res <-
          httr::GET(
            paste0(rawdownload, "/", sel_seq_name)
          )

        # if it was successful download
        if (
          res$status_code == 200 # NOTE: status_code 200 = success
        ) {

          # extract the data
          output <-
            httr::content(res)$data[[1]]

          # output 'success'
          cat(" - success", "\n")

          RUtilpol::save_latest_file(
            object_to_save = output,
            file_name = sel_seq_name,
            dir = path_to_indiv_folder,
            prefered_format = "rds",
            use_sha = TRUE
          )

          # break from loop
          break
        } else {

          # save output as nothing
          output <- NULL

          # output 'success'
          cat(" - fail", "\n")
        }

        # delete the result and output
        rm(res, output, envir = current_env)
      }
    }
  )

  # check again if all are downloaded
  # extract the sequences which were not successfully downloaded
  cannot_download <-
    get_missing_names(
      path_to_folder = path_to_indiv_folder,
      name_vector = ds_vector
    )

  if (
    length(cannot_download) == 0
  ) {
    usethis::ui_done("All selected sequences were downloaded")
  } else {
    usethis::ui_oops(
      paste(
        length(cannot_download), "out of", nrow(allds),
        "sequences were NOT downloaded.", "\n",
        "Specifically, dataset IDs:",
        RUtilpol::paste_as_vector(cannot_download)
      )
    )
  }

  ds_present <-
    list.files(
      path_to_indiv_folder
    ) %>%
    get_clean_name()

  RUtilpol::output_comment(
    paste(
      "Compiling sequences together"
    )
  )

  result_list <-
    subset(
      ds_present,
      ds_present %in% ds_vector
    ) %>%
    rlang::set_names() %>%
    purrr::map(
      .f = ~ RUtilpol::get_latest_file(
        file_name = .x,
        dir = path_to_indiv_folder
      )
    )

  # save the final list as 'neotoma_download' object filtering out
  # unsuccessfull sequences
  neotoma_download <-
    result_list[!purrr::map_lgl(result_list, is.null)]

  return(neotoma_download)
}
