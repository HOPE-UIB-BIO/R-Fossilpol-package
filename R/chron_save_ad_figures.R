#' @title Save all age-depth models by regions
#' @param dir Path to the data_storage folder
#' @param date Date to be used for making folders
#' @param text_size Text size
#' @param line_size Line size
#' @param image_width Width of image
#' @param image_height Height of image
#' @param image_units Units to measure image
#' @param image_format Format of figure
#' @return NULL
#' @description Load the most recent data, create a folder with current data
#' and save all plots in it by regions
#' @export
chron_save_ad_figures <- function(dir,
                                  date,
                                  text_size,
                                  line_size,
                                  image_width,
                                  image_height,
                                  image_units = c("in", "cm", "mm", "px"),
                                  image_format = ".pdf") {
  current_frame <- sys.nframe()
  current_env <- sys.frame(which = current_frame)

  RUtilpol::check_class("dir", "character")

  RUtilpol::check_class("date", "Date")

  RUtilpol::check_class("text_size", "numeric")

  RUtilpol::check_class("line_size", "numeric")

  RUtilpol::check_class("image_width", "numeric")

  RUtilpol::check_class("image_height", "numeric")

  RUtilpol::check_vector_values("image_units", c("in", "cm", "mm", "px"))

  image_units <- match.arg(image_units)

  RUtilpol::check_class("image_units", "character")

  RUtilpol::check_class("image_format", "character")

  dir <- RUtilpol::add_slash_to_path(dir)

  # load the old outputs
  ds_present <-
    list.files(
      paste0(
        dir, "Data/Processed/Chronology/Models_full/"
      )
    )

  RUtilpol::stop_if_not(
    length(ds_present) > 0,
    false_msg = paste("Did not detect any age-depth models")
  )

  RUtilpol::get_latest_file(
    file_name = "chron_mod_output"
  )

  ds_present_unique <-
    RUtilpol::get_clean_name(ds_present) %>%
    unique() %>%
    sort()

  # load the processed data
  data_regions <-
    RUtilpol::get_latest_file(
      file_name = "data_merged",
      dir = paste0(
        dir, "Data/Processed/Data_merged/"
      )
    ) %>%
    dplyr::select(
      dplyr::all_of(
        c("dataset_id", "region")
      )
    )

  RUtilpol::check_if_loaded(
    file_name = "data_regions",
    env = current_env
  )

  RUtilpol::check_class("data_regions", "data.frame")

  RUtilpol::check_col_names("data_regions", c("dataset_id", "region"))

  # merge them together
  data_to_plot <-
    data_regions %>%
    dplyr::filter(
      dataset_id %in% ds_present_unique
    )

  all_present_regions <-
    data_to_plot %>%
    dplyr::distinct(region) %>%
    purrr::pluck("region")

  fig_dir <- paste0(dir, "Outputs/Figures/Chronology/")

  # make the folders with current date
  util_make_fig_dir(
    dir = fig_dir,
    region_vector = all_present_regions,
    sel_date = date
  )

  most_recent_folder <-
    RUtilpol::get_latest_file_name(
      file_name = ".",
      dir = fig_dir,
      folder = TRUE
    )

  # loop for regions, subset the data, save the data to corresponding folders
  for (i in seq_along(all_present_regions)) {
    current_inside_frame <- sys.nframe()
    current_inside_env <- sys.frame(which = current_inside_frame)

    RUtilpol::output_comment(
      paste("Processing", all_present_regions[i])
    )

    save_dir <-
      paste0(
        fig_dir, most_recent_folder, "/", all_present_regions[i], "/"
      )

    data_sub <-
      data_to_plot %>%
      dplyr::filter(region == all_present_regions[i])

    if (
      nrow(data_sub) > 0
    ) {
      cat(" - detected sites", "\n")

      purrr::map(
        .x = data_sub$dataset_id,
        .f = ~ {
          sel_ad_model <-
            RUtilpol::get_latest_file(
              file_name = .x,
              dir = paste0(
                dir, "Data/Processed/Chronology/Models_full/"
              ),
              verbose = FALSE
            )

          chron_ggsave_bchron_output(
            data_source = sel_ad_model,
            dataset_id = .y,
            dir = save_dir,
            text_size = text_size,
            line_size = line_size,
            image_width = image_width,
            image_height = image_height,
            image_units = image_units,
            image_format = image_format
          )
        }
      )
    }

    rm(data_sub, envir = current_inside_env)
  }
}
