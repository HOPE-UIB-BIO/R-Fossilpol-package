#' @title Plot map of geographical location of data compilation
#' @inheritParams plot_graphical_summary
#' @param point_size Numeric. Size of a point representing one record
#' @param point_alpha Numeric. Alpha of each point
#' @param point_colour Character. Color of each point. Can be also a name of
#' a variable in `data_source`
#' @param point_colour_accent Optional. Character. If present, each point will
#' be highlighted by such color.
#' @param col_map_fill  Character. A color of a land in map
#' @param col_map_borders Character. A color of a country borders in map.
#' `NA` will produce no border lines
#' @param map_data_margin Numeric. Number of degrees set as a margin aroud data
#' to be ploted
#' @param legend_position Character. If legend present, where it should be
#' placed? Default is "none", which will not display legend.
#' See `legend.position` in [ggplot2::theme()]
#' @export
#' @seealso [plot_graphical_summary()], [plot_age_lenght_of_data()],
#' [plot_count_of_data()],
plot_map_of_data <- function(data_source,
                             point_size = 3,
                             point_alpha = 1,
                             point_colour = "gray30",
                             point_colour_accent = NULL,
                             col_map_fill = "gray90",
                             col_map_borders = NA,
                             map_data_margin = 1,
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
    "data_source", c("long", "lat")
  )

  RUtilpol::check_class("point_size", "numeric")

  RUtilpol::check_class("point_alpha", "numeric")

  assertthat::assert_that(
    point_alpha <= 1 & point_alpha >= 0,
    msg = "'point_alpha' must be between 0 and 1"
  )

  if (
    isFALSE(RUtilpol::is_color(point_colour))
  ) {
    RUtilpol::check_col_names("data_source", point_colour)
  } else {
    RUtilpol::check_color("point_colour")
  }

  RUtilpol::check_class(
    "point_colour_accent",
    c("character", "null")
  )

  if (
    is.character(point_colour_accent)
  ) {
    RUtilpol::check_color("point_colour_accent")
  }

  RUtilpol::check_class("col_map_fill", "character")

  RUtilpol::check_color("col_map_fill")

  RUtilpol::check_class("col_map_borders", c("character", "logical"))

  if (
    is.character(col_map_borders)
  ) {
    RUtilpol::check_color("col_map_borders")
  }

  RUtilpol::check_class("map_data_margin", "numeric")

  RUtilpol::check_class("text_size", "numeric")

  RUtilpol::check_class("line_size", "numeric")

  RUtilpol::check_class("legend_position", "character")

  RUtilpol::check_vector_values(
    "legend_position",
    c(
      "bottom",
      "top",
      "left",
      "right",
      "none"
    )
  )

  legend_position <- match.arg(legend_position)

  x_range <-
    data_source %>%
    purrr::pluck("long") %>%
    range()

  x_lim <-
    c(
      min(x_range) - map_data_margin,
      max(x_range) + map_data_margin
    )

  y_range <-
    data_source %>%
    purrr::pluck("lat") %>%
    range()

  y_lim <-
    c(
      min(y_range) - map_data_margin,
      max(y_range) + map_data_margin
    )

  p_0 <-
    data_source %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = long,
        y = lat
      )
    ) +
    ggplot2::coord_quickmap(
      xlim = x_lim,
      ylim = y_lim
    ) +
    ggplot2::labs(
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      line = ggplot2::element_line(linewidth = line_size),
      legend.position = legend_position,
      legend.text = ggplot2::element_text(size = text_size * 0.5),
      legend.title = ggplot2::element_text(size = text_size * 0.75),
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(ncol = 2),
      color = ggplot2::guide_legend(ncol = 2)
    )

  p_1 <-
    p_0 +
    ggplot2::borders(
      fill = col_map_fill,
      colour = col_map_borders,
      size = line_size
    )

  p_2 <-
    p_1 +
    ggplot2::geom_point(
      size = point_size + 1,
      alpha = point_alpha,
      colour = point_colour_accent
    )

  if (
    RUtilpol::is_color(point_colour)
  ) {
    p_3 <-
      p_2 +
      ggplot2::geom_point(
        size = point_size,
        alpha = point_alpha,
        colour = point_colour
      )
  } else {
    p_3 <-
      p_2 +
      ggplot2::geom_point(
        ggplot2::aes(
          colour = get(point_colour)
        ),
        size = point_size,
        alpha = point_alpha
      ) +
      ggplot2::labs(
        color = point_colour
      )
  }

  p_4 <-
    p_3 +
    ggplot2::geom_point(
      size = max(0.1, point_size / 10),
      alpha = point_alpha,
      colour = point_colour_accent
    )

  return(p_4)
}
