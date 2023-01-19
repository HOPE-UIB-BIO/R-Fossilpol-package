#' @title Plot graphical summary of dataset compilation
#' @param data_source Data.frame to containing the dataset compilation
#' @param grouping_variable
#' Optional name of variable to use to group and color data
#' @param drop_na Logical. If `grouping_variable` selected, shoudl NA values be
#' ommited?
#' @param col_default Character. Name of a default color
#' @param col_light Character. Name of a lighter color
#' @param col_dark Character. Name of a darker color
#' @param point_size Numeric. Size of a point
#' @param line_size Numeric. Line width
#' @param line_width Numeric. Width of a line for axis
#' @param text_size Numeric. Text size for axis
#' @param legend_position Character. If legend present, where it should be
#' placed? Default is "none", which will not display legend.
#' See `legend.position` in [ggplot2::theme()]
#' @param color_palette A vector with cuntom color palette. Only used when
#' `grouping_variable` is selected
#' @description Create 2-3 plots of graphical summary of data compilation.
#' When `grouping_variable` is specified, the selected variable will be used for
#' grouping and coloring.
#' Types of graph:
#' * Map - map of geographical location with each record represented as a point
#' * age length - age limits of records, with ech record represented by a
#' single line
#' * Count of data - Only used when `grouping_variable` is selected. A bar plot
#' with number of records in each group
#' @seealso [plot_map_of_data()], [plot_age_lenght_of_data()],
#' [plot_count_of_data()],
#' @export
plot_graphical_summary <- function(data_source,
                                   grouping_variable = NULL,
                                   drop_na = FALSE,
                                   col_default = "gray50",
                                   col_light = "gray90",
                                   col_dark = "gray30",
                                   point_size = 3,
                                   line_size = 1,
                                   line_width = 0.1,
                                   text_size = 16,
                                   legend_position = c(
                                     "none",
                                     "bottom",
                                     "top",
                                     "left",
                                     "right"
                                   ),
                                   color_palette = NULL) {
  RUtilpol::check_class("data_source", "data.frame")
  RUtilpol::check_color("col_default")
  RUtilpol::check_color("col_light")
  RUtilpol::check_color("col_dark")
  RUtilpol::check_class("point_size", "numeric")
  RUtilpol::check_class("line_size", "numeric")
  RUtilpol::check_class("line_width", "numeric")
  RUtilpol::check_class("text_size", "numeric")

  if (
    is.null(grouping_variable)
  ) {
    def_colour <- col_default
  } else {
    RUtilpol::check_class("grouping_variable", "character")

    RUtilpol::check_col_names("data_source", grouping_variable)

    RUtilpol::check_class("drop_na", "logical")

    if (
      isTRUE(drop_na)
    ) {
      data_source <-
        data_source %>%
        tidyr::drop_na(
          dplyr::all_of(grouping_variable)
        )
    }

    def_colour <- grouping_variable

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

    if (
      isFALSE(is.null(color_palette))
    ) {
      RUtilpol::check_class("color_palette", "character")

      assertthat::assert_that(
        purrr::pluck(data_source, grouping_variable) %in% names(color_palette),
        msg = paste(
          "The provided `color_palette` does not cointain all values",
          "present in", RUtilpol::paste_as_vector(grouping_variable)
        )
      )
    }
  }

  p_map <-
    plot_map_of_data(
      data_source = data_source,
      point_size = point_size,
      point_alpha = 1,
      point_colour = def_colour,
      point_colour_accent = col_dark,
      col_map_fill = col_light,
      col_map_borders = col_dark,
      map_data_margin = 1,
      text_size = text_size,
      line_size = line_size,
      legend_position = legend_position
    )

  p_count <-
    plot_count_of_data(
      data_source = data_source,
      fill_colour = def_colour,
      outline_width = line_width,
      ouline_colour = col_dark,
      line_size = line_size,
      text_size = text_size,
      legend_position = legend_position
    )

  p_age_length <-
    plot_age_lenght_of_data(
      data_source = data_source,
      line_width = 1,
      line_alpha = 1,
      line_colour = def_colour,
      text_size = text_size,
      line_size = line_size,
      legend_position = legend_position
    )

  plot_list <-
    list(
      p_map,
      p_count,
      p_age_length
    )

  if (
    isFALSE(is.null(color_palette))
  ) {
    plot_list <-
      purrr::map(
        .x = plot_list,
        .f = ~ .x +
          ggplot2::scale_color_manual(
            values = color_palette
          ) +
          ggplot2::scale_fill_manual(
            values = color_palette
          )
      )
  }

  res <-
    ggpubr::ggarrange(
      plotlist = plot_list,
      nrow = 1,
      common.legend = TRUE,
      legend = legend_position,
      labels = LETTERS[seq_along(plot_list)]
    )

  return(res)
}
