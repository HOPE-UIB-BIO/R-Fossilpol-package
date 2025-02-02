
#' @title Get names of missing datasets
#' @param dir Character. Path to the folder with files
#' @param name_vector Vector with names of datasets.
#' @keywords internal
util_get_missing_ds_names <- function(dir, name_vector) {

  # pre-alocate that all records are missing
  ds_absent <- name_vector

  # check which records are present
  ds_present <-
    list.files(
      dir
    )

  if (
    length(ds_present) > 0
  ) {
    ds_present_striped <-
      RUtilpol::get_clean_name(ds_present)

    # select data NOT downloaded
    ds_absent <-
      name_vector[!name_vector %in% ds_present_striped]
  }

  ds_absent %>%
    unique() %>%
    return()
}
