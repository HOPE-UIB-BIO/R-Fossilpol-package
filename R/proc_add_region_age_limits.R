#' @title Define age limits for each region in the data
#' @param data_source Data.frame with `region` variable
#' @param dir Directory of data storage folder
#' @export
proc_add_region_age_limits <-
  function(data_source, dir) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "region")

    RUtilpol::check_class("dir", "character")

    RUtilpol::output_comment(
      msg = "Checking regional age-limit definition"
    )

    region_age_limit_path <-
      paste0(dir, "/Data/Input/Regional_age_limits/")

    # extract the list of regions
    data_regions <-
      data_source %>%
      dplyr::distinct(region)

    # load/create criteria for age limits of period of interest
    regional_age_limits <-
      stopcheck_table(
        data_source = data_regions,
        file_name = "regional_age_limits",
        dir = region_age_limit_path,
        sel_method = "age_limits",
        msg = paste(
          "Adjust age limits for each region.",
          "'young_age' = the youngest age the sequence has to have.",
          "'old_age' = the oldest age the the sequence has to have.",
          "'end_of_interest_period' = levels beyond this age will be omitted."
        )
      )

    RUtilpol::open_dir_if_not(
      c(
        regional_age_limits$young_age,
        regional_age_limits$old_age
      ) %>%
        is.na() %>%
        any() %>%
        isFALSE(),
      dir = region_age_limit_path,
      msg = paste(
        "Some of the regions in the data has",
        "'NA' values among 'young_age' or 'old_age'.",
        paste0(
          "Please open file 'regional_age_limits' in folder ",
          region_age_limit_path, ".",
          " Then proceed and re-run this whole script"
        )
      )
    )

    # merge criteria with the data
    res <-
      dplyr::left_join(
        data_source,
        regional_age_limits,
        by = "region"
      )

    util_check_data_table(res)

    RUtilpol::check_col_names("res", c("young_age", "old_age", "end_of_interest_period"))

    return(res)
  }
