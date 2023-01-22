#' @title A bar plot with number of records in dataset compilation
#' @inheritParams plot_map_of_data
#' @param point_color Character. Color of lollipops. Can be also a name of
#' a variable in `data_source`
#' @param point_size Numeric. Size of a lollipop
#' @param ouline_colour Numeric. Colour of a outline of lollipol
#' @export
#' @seealso [plot_graphical_summary()], [plot_map_of_data()],
#' [plot_age_lenght_of_data()]
plot_count_of_data <- function(data_source,
                               point_color = "gray80",
                               point_size = 10,
                               ouline_colour = "gray30",
                               line_size = 0.1,
                               text_size = 16,
                               legend_position = c(
                                 "none",
                                 "bottom",
                                 "top",
                                 "left",
                                 "right"
                               )) {
  RUtilpol::check_class("data_source", "data.frame")

  RUtilpol::check_class("point_color", "character")

  if (
    isFALSE(RUtilpol::is_color(point_color))
  ) {
    RUtilpol::check_col_names("data_source", point_color)
  } else {
    RUtilpol::check_color("point_color")
  }

  RUtilpol::check_class("point_size", "numeric")

  RUtilpol::check_class("ouline_colour", "character")

  RUtilpol::check_color("ouline_colour")

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

  if (
    isFALSE(RUtilpol::is_color(point_color))
  ) {
    data_to_use <-
      data_source %>%
      dplyr::count(get(point_color)) %>%
      dplyr::rename(
        fill_var = `get(point_color)`
      ) %>%
      dplyr::mutate(
        fill_var_wrap = stringr::str_wrap(
          fill_var,
          width = 20
        )
      ) %>%
      dplyr::mutate(
        fill_var_wrap = ifelse(
          is.na(fill_var_wrap),
          "NA",
          fill_var_wrap
        )
      )
  } else {
    data_to_use <-
      data_source %>%
      dplyr::mutate(
        fill_var = "fill_var"
      ) %>%
      dplyr::count(fill_var)
  }

  p_0 <-
    data_to_use %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = forcats::fct_reorder(fill_var, -n),
        y = n
      )
    ) +
    ggplot2::theme_classic() +
    ggplot2::coord_cartesian(
      ylim = c(0, max(data_to_use$n) * 1.5)
    ) +
    ggplot2::theme(
      line = ggplot2::element_line(linewidth = line_size),
      text = ggplot2::element_text(size = text_size),
      legend.position = legend_position,
      legend.text = ggplot2::element_text(size = text_size * 0.5),
      legend.title = ggplot2::element_text(size = text_size * 0.75),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 2),
      color = ggplot2::guide_legend(nrow = 2)
    ) +
    ggplot2::labs(
      y = "Number of datasets",
      x = ""
    )

  max_value <-
    max(data_to_use$n)

  p_1 <-
    p_0 +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = forcats::fct_reorder(fill_var, -n),
        yend = 0
      ),
      colour = ouline_colour
    ) +
    ggplot2::geom_point(
      size = point_size + 1,
      col = ouline_colour
    )

  if (
    RUtilpol::is_color(point_color)
  ) {
    p_2 <-
      p_1 +
      ggplot2::geom_point(
        col = point_color,
        size = point_size
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = n),
        size = text_size * 0.2
      )
  } else {
    p_2 <-
      p_1 +
      ggplot2::geom_point(
        ggplot2::aes(
          col = fill_var
        ),
        size = point_size
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = n),
        size = text_size * 0.2
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = fill_var_wrap),
        angle = 90,
        hjust = 0,
        nudge_y = max_value * 0.1,
        size = text_size * 0.2
      ) +
      ggplot2::labs(
        color = point_color
      )
  }

  return(p_2)
}
