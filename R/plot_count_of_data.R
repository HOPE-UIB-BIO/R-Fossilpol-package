#' @title A bar plot with number of records in dataset compilation
#' @inheritParams plot_map_of_data
#' @param fill_colour Character. Color of each bar. Can be also a name of
#' a variable in `data_source`
#' @param outline_width Numeric. Width of a outline of each bar
#' @param ouline_colour Numeric. Colour of a outline of each bar
#' @export
#' @seealso [plot_graphical_summary()], [plot_map_of_data()],
#' [plot_age_lenght_of_data()]
plot_count_of_data <- function(data_source,
                               fill_colour = "gray80",
                               outline_width = 0.1,
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

  RUtilpol::check_class("fill_colour", "character")

  if (
    isFALSE(RUtilpol::is_color(fill_colour))
  ) {
    RUtilpol::check_col_names("data_source", fill_colour)
  } else {
    RUtilpol::check_color("fill_colour")
  }

  RUtilpol::check_class("outline_width", "numeric")

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
    isFALSE(RUtilpol::is_color(fill_colour))
  ) {
    data_to_use <-
      data_source %>%
      dplyr::count(get(fill_colour)) %>%
      dplyr::rename(
        fill_var = `get(fill_colour)`
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
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 2),
      color = ggplot2::guide_legend(nrow = 2)
    ) +
    ggplot2::labs(
      y = "Number of datasets",
      x = ""
    )

  if (
    RUtilpol::is_color(fill_colour)
  ) {
    p_1 <-
      p_0 +
      ggplot2::geom_bar(
        fill = fill_colour,
        stat = "identity",
        colour = ouline_colour,
        linewidth = outline_width
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = n),
        vjust = 0,
        nudge_y = 2,
        size = text_size
      )
  } else {
    p_1 <-
      p_0 +
      ggplot2::geom_bar(
        ggplot2::aes(fill = fill_var),
        stat = "identity",
        colour = ouline_colour,
        linewidth = outline_width
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = fill_var_wrap),
        angle = 90,
        hjust = 0,
        nudge_y = 2,
        size = text_size * 0.2
      ) +
      ggplot2::labs(
        fill = fill_colour
      )
  }

  return(p_1)
}
