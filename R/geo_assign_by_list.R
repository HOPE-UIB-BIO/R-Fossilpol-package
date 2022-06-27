#' @title Assign series of geographical variables from shapefiles and/or tifs
#' @param data_source Data.frame to add values to.
#' @param dir The directory with shapefile/tifs
#' @param optional_info_to_assign Data,frame with information about layers to be
#' assigned. See `geo_assign_value`
#' @export
#' @seealso geo_assign_value
geo_assign_by_list <-
  function(data_source, dir, optional_info_to_assign) {
    util_check_class("data_source", "data.frame")

    util_check_class("optional_info_to_assign", "data.frame")

    util_check_col_names(
      "optional_info_to_assign",
      c(
        "var_name",
        "sel_method",
        "dir",
        "file_name",
        "var"
      )
    )

    info_to_assign_filtered <-
      optional_info_to_assign %>%
      tidyr::drop_na()

    info_to_assign <-
      tibble::tribble(
        ~var_name, ~sel_method, ~dir, ~file_name, ~var,

        # regions
        "region",
        "shapefile",
        paste0(dir, "/Data/Input/Spatial/Regions_shapefile"),
        "Regions",
        "region",

        # sociopolitical units (countries)
        "country",
        "shapefile",
        paste0(dir, "/Data/Input/Spatial/Countries_shapefile"),
        "Countries_global",
        "NAME",

        # Harmonisation regions
        "harmonisation_region",
        "shapefile",
        paste0(dir, "/Data/Input/Spatial/Harmonisation_regions_shapefile"),
        "Harmonisation_regions",
        "harm_reg",

        # Calibration curves
        "curve_name",
        "shapefile",
        paste0(dir, "/Data/Input/Spatial/Calibration_curves_shapefile"),
        "Calibration_curves",
        "curve_name",

        # postbomb calibration curves
        "postbomb_curve_name",
        "shapefile",
        paste0(dir, "/Data/Input/Spatial/Postbomb_shapefile"),
        "Postbomb_calib_curves",
        "curve"
      )

    info_to_assign_all <-
      dplyr::bind_rows(
        info_to_assign,
        info_to_assign_filtered
      )

    res <- data_source

    util_output_message(
      msg = "Assign information based on geographical position"
    )

    for (i in seq_along(info_to_assign_all$var_name)) {
      res <-
        geo_assign_value(
          data_source = res,
          dir = info_to_assign_all$dir[i],
          sel_method = info_to_assign_all$sel_method[i],
          file_name = info_to_assign_all$file_name[i],
          var = info_to_assign_all$var[i],
          var_name = info_to_assign_all$var_name[i]
        )
    }

    return(res)
  }
