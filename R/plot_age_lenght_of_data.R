
#' @title Plot age-limits of all records in dataset compilation
#' @inheritParams plot_map_of_data
#' @param line_width Numeric. Width of a line representing one record
#' @param line_alpha Numeric. Alpha of each line
#' @param line_colour Character. Color of each line. Can be also a name of
#' a variable in `data_source`
#' @export
#' @seealso [plot_graphical_summary()], [plot_map_of_data()],
#' [plot_count_of_data()],
plot_age_lenght_of_data <- function(data_source,
                                    line_width = 1,
                                    line_alpha = 1,
                                    line_colour = "gray30",
                                    text_size = 16,
                                    line_size = 0.1,
                                    legend_position = c(
                                      "none",
                                      "bottom",
                                      "top",
                                      "left",
                                      "right"
                                    )) {
  RUtilpol::check_class("data_source", "data.frame")

  RUtilpol::check_col_names(
    "data_source", c("dataset_id", "age_max", "age_min")
  )

  RUtilpol::check_class("line_width", "numeric")

  RUtilpol::check_class("line_alpha", "numeric")

  assertthat::assert_that(
    line_alpha <= 1 & line_alpha >= 0,
    msg = "'line_alpha' must be between 0 and 1"
  )

  RUtilpol::check_class("line_colour", "character")

  if (
    isFALSE(RUtilpol::is_color(line_colour))
  ) {
    RUtilpol::check_col_names("data_source", line_colour)
  } else {
    RUtilpol::check_color("line_colour")
  }

  RUtilpol::check_class("line_size", "numeric")

  RUtilpol::check_class("text_size", "numeric")

  RUtilpol::check_class("legend_position", "character")

  RUtilpol::check_vector_values(
    "legend_position",
    c(
      "none",
      "bottom",
      "top",
      "left",
      "right"
    )
  )

  legend_position <- match.arg(legend_position)

  p_0 <-
    data_source %>%
    dplyr::mutate(
      age_mean = abs(age_max - age_min),
      dataset_id = as.factor(dataset_id)
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        y = forcats::fct_reorder(dataset_id, -age_mean),
        yend = forcats::fct_reorder(dataset_id, -age_mean)
      )
    ) +
    ggplot2::scale_x_continuous(
      trans = "reverse"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      line = ggplot2::element_line(linewidth = line_size),
      text = ggplot2::element_text(size = text_size),
      legend.position = legend_position,
      legend.text = ggplot2::element_text(size = text_size * 0.5),
      legend.title = ggplot2::element_text(size = text_size * 0.75),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 2),
      color = ggplot2::guide_legend(nrow = 2)
    ) +
    ggplot2::labs(
      y = "Dataset ID",
      x = "Age (cal yr BP)"
    )

  if (
    RUtilpol::is_color(line_colour)
  ) {
    p_1 <-
      p_0 +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = age_max,
          xend = age_min
        ),
        linewidth = line_width,
        color = line_colour,
        alpha = line_alpha
      )
  } else {
    p_1 <-
      p_0 +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = age_max,
          xend = age_min,
          colour = get(line_colour)
        ),
        linewidth = line_width,
        alpha = line_alpha
      ) +
      ggplot2::labs(
        color = line_colour
      )
  }

  return(p_1)
}
