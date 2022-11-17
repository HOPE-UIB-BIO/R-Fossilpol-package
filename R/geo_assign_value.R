#' @title Assign geographical value to a dataset
#' @param data_source Data.frame with `lat` and `long` coordinates to be assign a
#' value
#' @param dir Directory of the source of information
#' @param sel_method What is the format of the source of information.
#' Could be either `shapefile` or `tif`
#' @param file_name Name of the layer for shapefile or tif file
#' @param var Which variable should be extracted from source of information?
#' Note that it is set as "raster_values" as it is default for tifs
#' @param var_name Optional name of the new variable for extracted data
#' @param tif_fill_na Logical. For tif only. If `TRUE`, function will search
#' for most common value in the increasing distance of `tif_distance_step`
#' @param tif_distance_step For tif only. Use for values, which should be
#' treated as `NA` for the purposes of looking for additional value.
#' @param tif_n_max_step Numeric. For tif only. Distance in m giving radius
#' to increase the search radius
#' @description Extract data from shape file or tif and add to points
#' provided in `data_source` based on the data `lat` and `long`. There are
#' additional options for tif files. If `tif_fill_na == TRUE`, function will
#'  search for the most common value in the increasing distance of
#'  `tif_distance_step`. Function will stop search if all values are found.
#' @export
geo_assign_value <-
  function(data_source,
           dir,
           sel_method = c("shapefile", "tif"),
           file_name,
           var = "raster_values",
           var_name,
           tif_fill_na = TRUE,
           tif_distance_step = 5e3,
           tif_n_max_step = 20) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_class("dir", "character")

    RUtilpol::check_vector_values("sel_method", c("shapefile", "tif"))

    sel_method <- match.arg(sel_method)

    RUtilpol::check_class("file_name", "character")

    RUtilpol::check_class("var", "character")

    if (
      missing(var_name)
    ) {
      var_name <- var
    }

    RUtilpol::check_class("var_name", "character")

    RUtilpol::check_class("tif_fill_na", "logical")

    RUtilpol::check_class("tif_distance_step", "numeric")

    RUtilpol::check_class("tif_n_max_step", "numeric")

    if (
      sel_method == "shapefile"
    ) {
      sel_shape_file <-
        sf::read_sf(dsn = dir, layer = file_name)

      data_with_value <-
        geo_assign_shapefile(
          data_source = data_source,
          shapefile = sel_shape_file
        )
    } else if (
      sel_method == "tif"
    ) {
      data_with_value <-
        geo_assign_tif(
          data_source = data_source,
          tif_file_name = paste0(dir, "/", file_name, ".tif"),
          fill_na = tif_fill_na,
          distance_step = tif_distance_step,
          n_max_step = tif_n_max_step
        )
    } else {
      return(NA)
    }


    RUtilpol::stop_if_not(
      var %in% names(data_with_value),
      true_msg = paste("Selected shapefile resulted in the selected value:", var),
      false_msg = paste(
        "Selected shapefile does not contains value:", var
      )
    )

    res <-
      data_with_value %>%
      dplyr::select(
        dplyr::any_of(
          c(
            names(data_source),
            var
          )
        )
      ) %>%
      proc_rename_column(
        data_source = .,
        old_name = var,
        new_name = var_name
      )

    RUtilpol::stop_if_not(
      var_name %in% names(res),
      true_msg = paste(
        "Data.frame contains the preferred values in variable:", var_name
      ),
      false_msg = paste(
        "Data.frame does not contain the preferred values in variable:", var_name
      )
    )

    return(res)
  }
