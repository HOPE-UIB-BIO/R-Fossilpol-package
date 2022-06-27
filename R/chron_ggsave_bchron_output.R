#' @title Save chronology as ggplot
#' @param data_source Chronology data
#' @param dataset_id dataset_id
#' @param dir Directory path
#' @param text_size Text size
#' @param line_size Line size
#' @param image_width Width of image
#' @param image_height Height of image
#' @param image_units Units to measure image
#' @param image_format Format of figure
chron_ggsave_bchron_output <-
  function(data_source,
           dataset_id,
           dir,
           text_size,
           line_size,
           image_width,
           image_height,
           image_units = c("in", "cm", "mm", "px"),
           image_format = ".pdf") {
    current_frame <- sys.nframe()

    current_env <- sys.frame(which = current_frame)

    util_check_class("data_source", "BchronologyRun")

    util_check_class("dataset_id", "character")

    util_check_class("dir", "character")

    util_check_class("text_size", "numeric")

    util_check_class("line_size", "numeric")

    util_check_class("image_width", "numeric")

    util_check_class("image_height", "numeric")

    util_check_vector_values("image_units", c("in", "cm", "mm", "px"))

    image_units <- match.arg(image_units)

    util_check_class("image_units", "character")

    util_check_class("image_format", "character")

    site_name <- paste0("dataset_", dataset_id, image_format)
    site_title <- paste0("dataset_id = ", dataset_id)

    cat(
      paste(" - - saving file", site_name), "\n"
    )

    Bchron_plot <-
      plot(data_source) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        text = ggplot2::element_text(size = text_size),
        line = ggplot2::element_line(size = line_size)
      ) +
      ggplot2::labs(
        title = site_title,
        x = "Age (cal years BP)",
        y = "Depth (cm)"
      )

    ggplot2::ggsave(
      filename = site_name,
      plot = Bchron_plot,
      path = dir,
      width = image_width,
      height = image_height,
      units = image_units
    )
  }
