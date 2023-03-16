#' @title Obtain sample information from Neotoma download
#' @param neotoma_download List of lists with Neotoma data
#' @return Data.farme with nested information about individual samples
#' @keywords internal
proc_neo_get_samples <- function(neotoma_download) {
  RUtilpol::check_class("neotoma_download", "list")

  RUtilpol::output_comment(
    msg = "Extracting Neotoma samples"
  )

  neotoma_download_sites <-
    proc_neo_get_sites(neotoma_download)

  # get all samples
  neotoma_samples <-
    neotoma_download_sites %>%
    purrr::map("collectionunit") %>%
    purrr::map("dataset") %>%
    purrr::map("samples")

  # extract samples into a table
  # !!! this takes time !!!
  neotoma_sample_data <-
    purrr::map2( 
      .progress = "extracting samples into a table",
      .x = neotoma_samples,
      .y = names(neotoma_samples),
      .f = ~ proc_neo_extract_samples(
        data_source = .x,
        sel_dataset_id = .y
      )
    ) %>%
    purrr::list_rbind()
  
  # only include records with some data
  neotoma_sample_data_clean <-
    neotoma_sample_data %>%
    tidyr::drop_na(samples)

  RUtilpol::stop_if_not(
    nrow(neotoma_sample_data_clean) > 0,
    false_msg = paste(
      "There are 0 records with sample data based on the current download",
      "Please change the criteria and re-download."
    ),
    true_msg = paste(
      "Sample data were extracted for",
      nrow(neotoma_sample_data_clean),
      "records"
    )
  )

  return(neotoma_sample_data_clean)
}
